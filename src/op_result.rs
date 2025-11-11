use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, quote_spanned};
use syn::{Expr, ExprBinary, ExprUnary, Item, ItemFn};

use crate::utils;

#[derive(Clone)]
struct OpResultConfig {
    well_formedness_syntax: bool,  // true = enable well-formedness syntax [(); ...]:
    marker_trait_syntax: bool,      // true = enable marker trait syntax (): IsDefined<{...}>
    marker_trait_name: String,       // name of the marker trait (default: "IsDefined")
}

impl Default for OpResultConfig {
    fn default() -> Self {
        OpResultConfig {
            well_formedness_syntax: true,
            marker_trait_syntax: true,
            marker_trait_name: "IsDefined".to_string(),
        }
    }
}

fn parse_attr(attr: TokenStream) -> OpResultConfig {
    let attr_ts = proc_macro2::TokenStream::from(attr);
    let mut config = OpResultConfig::default();
    
    // If attribute is empty, use defaults
    if attr_ts.is_empty() {
        return config;
    }
    
    // Parse the attribute tokens
    let tokens: Vec<proc_macro2::TokenTree> = attr_ts.into_iter().collect();
    let mut iter = tokens.iter().peekable();
    
    while let Some(tt) = iter.next() {
        // Check for identifier flags
        if let proc_macro2::TokenTree::Ident(ident) = tt {
            let ident_str = ident.to_string();
            
            if ident_str == "marker_trait_syntax" {
                // marker_trait_syntax means only marker trait syntax, disable well-formedness
                config.marker_trait_syntax = true;
                config.well_formedness_syntax = false;
                
                // Check for optional second parameter (trait name)
                if let Some(proc_macro2::TokenTree::Punct(comma)) = iter.peek() {
                    if comma.as_char() == ',' {
                        iter.next(); // consume comma
                        if let Some(proc_macro2::TokenTree::Ident(trait_ident)) = iter.next() {
                            config.marker_trait_name = trait_ident.to_string();
                        }
                    }
                }
            } else if ident_str == "well_formedness_syntax" {
                // well_formedness_syntax means only well-formedness syntax, disable marker trait
                config.well_formedness_syntax = true;
                config.marker_trait_syntax = false;
            } else if ident_str == "any_syntax" {
                // any_syntax means both syntaxes enabled (default behavior)
                config.well_formedness_syntax = true;
                config.marker_trait_syntax = true;
                
                // Check for optional second parameter (trait name)
                if let Some(proc_macro2::TokenTree::Punct(comma)) = iter.peek() {
                    if comma.as_char() == ',' {
                        iter.next(); // consume comma
                        if let Some(proc_macro2::TokenTree::Ident(trait_ident)) = iter.next() {
                            config.marker_trait_name = trait_ident.to_string();
                        }
                    }
                }
            }
        }
    }
    
    config
}

pub fn expand_op_result(attr: TokenStream, item: TokenStream) -> TokenStream {
    let config = parse_attr(attr);
    let marker_trait_name = config.marker_trait_name.clone();
    
    // Process the entire token stream, find `(): IsDefined<{...}>` patterns and transform them
    let item_ts = proc_macro2::TokenStream::from(item);
    
    // Expand all `(): IsDefined<{...}>` patterns in the token stream
    let (expanded_ts, mut defined_expansions) = expand_defined_in_tokens(item_ts, config.clone());
    
    // Try to parse as different item types
    match syn::parse2::<Item>(expanded_ts.clone()) {
        Ok(item) => {
            // Recursively process the item and all nested items, collecting additional expansions
            let (processed_item, nested_expansions) = process_item_recursive(item, config);
            
            // Combine all expansions (top-level and nested)
            defined_expansions.extend(nested_expansions);
            
            // Create type aliases for IDE hover support - one for each IsDefined usage
            let mut defined_types = proc_macro2::TokenStream::new();
            
            for (_idx, (span, expansion)) in defined_expansions.iter().enumerate() {
                let expansion_str: String = expansion.to_string();
                let expansion_doc = format!("**Expands to:** `{}`", expansion_str);
                let defined_ident = proc_macro2::Ident::new(&marker_trait_name, *span);
                let throwaway_fn  = quote! {
                    #[allow(dead_code)]
                    let _: () = {
                        /// An operator bound expression.
                        ///
                        #[doc = #expansion_doc]
                        type #defined_ident = ();
                    };
                };
                defined_types.extend(throwaway_fn);
            }
            
            TokenStream::from(quote! {
                const _: () = {
                    #defined_types
                };
                #processed_item
            })
        }
        Err(_) => {
            // If parsing as Item fails, try parsing as ItemFn for backward compatibility
            match syn::parse2::<ItemFn>(expanded_ts) {
                Ok(item_fn) => {
                    let mut defined_types = proc_macro2::TokenStream::new();
                    
                    for (_idx, (span, expansion)) in defined_expansions.iter().enumerate() {
                        let expansion_str: String = expansion.to_string();
                        let expansion_doc = format!("**Expands to:** `{}`", expansion_str);
                        let defined_ident = proc_macro2::Ident::new(&marker_trait_name, *span);
                        let throwaway_fn  = quote! {
                            #[allow(dead_code)]
                            let _: () = {
                                /// An operator bound expression.
                                ///
                                #[doc = #expansion_doc]
                                type #defined_ident = ();
                            };
                        };
                        defined_types.extend(throwaway_fn);
                    }
                    
                    TokenStream::from(quote! {
                        const _: () = {
                            #defined_types
                        };
                        #item_fn
                    })
                }
                Err(e) => {
                    TokenStream::from(e.to_compile_error())
                }
            }
        }
    }
}

// Recursively processes an item and all nested items, processing where clauses in each
// Returns (processed_item, collected_expansions)
fn process_item_recursive(item: Item, config: OpResultConfig) -> (proc_macro2::TokenStream, Vec<(Span, proc_macro2::TokenStream)>) {
    match item {
        Item::Fn(item_fn) => {
            // Process nested items in the function body (e.g., nested functions, impl blocks, etc.)
            // Note: function bodies are expressions/blocks, not items, so we process the where clause
            // The where clause is already processed at the token level, so we just need to handle nested items
            // For functions, we can't have nested items in the body, so we just return the function
            (quote! { #item_fn }, Vec::new())
        }
        Item::Struct(item_struct) => {
            // Process where clause (already done at token level)
            // Process nested items if any (structs can't have nested items in Rust)
            (quote! { #item_struct }, Vec::new())
        }
        Item::Enum(item_enum) => {
            // Process where clause (already done at token level)
            // Process nested items if any (enums can't have nested items)
            (quote! { #item_enum }, Vec::new())
        }
        Item::Impl(mut item_impl) => {
            // The impl block's where clause should already be expanded at the token level
            // We just need to recursively process nested items and collect their expansions
            let mut processed_items = Vec::new();
            let mut all_expansions = Vec::new();
            
            for item in item_impl.items.iter() {
                let (processed, expansions) = process_impl_item(item.clone(), config.clone());
                processed_items.push(processed);
                all_expansions.extend(expansions);
            }
            
            item_impl.items = processed_items;
            (quote! { #item_impl }, all_expansions)
        }
        Item::Trait(mut item_trait) => {
            // The trait's where clause should already be expanded at the token level
            // We just need to recursively process nested items and collect their expansions
            let mut processed_items = Vec::new();
            let mut all_expansions = Vec::new();
            
            for item in item_trait.items.iter() {
                let (processed, expansions) = process_trait_item(item.clone(), config.clone());
                processed_items.push(processed);
                all_expansions.extend(expansions);
            }
            
            item_trait.items = processed_items;
            (quote! { #item_trait }, all_expansions)
        }
        Item::Type(item_type) => {
            // Process where clause (already done at token level)
            (quote! { #item_type }, Vec::new())
        }
        Item::Mod(mut item_mod) => {
            // Process nested items in the module
            let mut all_expansions = Vec::new();
            if let Some((_, ref mut items)) = item_mod.content {
                let mut processed_items = Vec::new();
                for item in items.iter() {
                    let (processed_ts, expansions) = process_item_recursive(item.clone(), config.clone());
                    all_expansions.extend(expansions);
                    // Parse the processed token stream back to an item
                    match syn::parse2::<Item>(processed_ts) {
                        Ok(processed_item) => processed_items.push(processed_item),
                        Err(_) => processed_items.push(item.clone()), // Fallback to original
                    }
                }
                *items = processed_items;
            }
            (quote! { #item_mod }, all_expansions)
        }
        Item::Const(item_const) => {
            (quote! { #item_const }, Vec::new())
        }
        Item::Static(item_static) => {
            (quote! { #item_static }, Vec::new())
        }
        Item::ForeignMod(item_foreign) => {
            (quote! { #item_foreign }, Vec::new())
        }
        Item::Use(item_use) => {
            (quote! { #item_use }, Vec::new())
        }
        Item::Macro(item_macro) => {
            (quote! { #item_macro }, Vec::new())
        }
        Item::Verbatim(item_verbatim) => {
            (quote! { #item_verbatim }, Vec::new())
        }
        _ => {
            // For any other item type, just quote it as-is
            (quote! { #item }, Vec::new())
        }
    }
}

// Process an impl item (method, associated type, etc.)
// Returns (processed_item, collected_expansions)
fn process_impl_item(item: syn::ImplItem, config: OpResultConfig) -> (syn::ImplItem, Vec<(Span, proc_macro2::TokenStream)>) {
    match item {
        syn::ImplItem::Fn(method) => {
            // Process the method's where clause recursively
            let method_ts = quote! { #method };
            let (expanded_ts, expansions) = expand_defined_in_tokens(method_ts, config);
            match syn::parse2::<syn::ImplItem>(expanded_ts) {
                Ok(processed) => (processed, expansions),
                Err(_) => (syn::ImplItem::Fn(method), expansions), // Fallback to original, but keep expansions
            }
        }
        syn::ImplItem::Type(ty) => {
            // Process the associated type's where clause
            let ty_ts = quote! { #ty };
            let (expanded_ts, expansions) = expand_defined_in_tokens(ty_ts, config);
            match syn::parse2::<syn::ImplItem>(expanded_ts) {
                Ok(processed) => (processed, expansions),
                Err(_) => (syn::ImplItem::Type(ty), expansions), // Fallback to original, but keep expansions
            }
        }
        syn::ImplItem::Const(item_const) => {
            (syn::ImplItem::Const(item_const), Vec::new())
        }
        syn::ImplItem::Macro(item_macro) => {
            (syn::ImplItem::Macro(item_macro), Vec::new())
        }
        syn::ImplItem::Verbatim(item_verbatim) => {
            (syn::ImplItem::Verbatim(item_verbatim), Vec::new())
        }
        _ => (item, Vec::new()),
    }
}

// Process a trait item (method, associated type, etc.)
// Returns (processed_item, collected_expansions)
fn process_trait_item(item: syn::TraitItem, config: OpResultConfig) -> (syn::TraitItem, Vec<(Span, proc_macro2::TokenStream)>) {
    match item {
        syn::TraitItem::Fn(method) => {
            // Process the method's where clause recursively
            let method_ts = quote! { #method };
            let (expanded_ts, expansions) = expand_defined_in_tokens(method_ts, config);
            match syn::parse2::<syn::TraitItem>(expanded_ts) {
                Ok(processed) => (processed, expansions),
                Err(_) => (syn::TraitItem::Fn(method), expansions), // Fallback to original, but keep expansions
            }
        }
        syn::TraitItem::Type(ty) => {
            // Process the associated type's where clause
            let ty_ts = quote! { #ty };
            let (expanded_ts, expansions) = expand_defined_in_tokens(ty_ts, config);
            match syn::parse2::<syn::TraitItem>(expanded_ts) {
                Ok(processed) => (processed, expansions),
                Err(_) => (syn::TraitItem::Type(ty), expansions), // Fallback to original, but keep expansions
            }
        }
        syn::TraitItem::Const(item_const) => {
            (syn::TraitItem::Const(item_const), Vec::new())
        }
        syn::TraitItem::Macro(item_macro) => {
            (syn::TraitItem::Macro(item_macro), Vec::new())
        }
        syn::TraitItem::Verbatim(item_verbatim) => {
            (syn::TraitItem::Verbatim(item_verbatim), Vec::new())
        }
        _ => (item, Vec::new()),
    }
}

fn expand_defined_in_tokens(tokens: proc_macro2::TokenStream, config: OpResultConfig) -> (proc_macro2::TokenStream, Vec<(Span, proc_macro2::TokenStream)>) {
    expand_defined_in_token_tree(tokens, config)
}


fn expand_defined_in_token_tree(tt: proc_macro2::TokenStream, config: OpResultConfig) -> (proc_macro2::TokenStream, Vec<(Span, proc_macro2::TokenStream)>) {
    let mut result = proc_macro2::TokenStream::new();
    let mut iter = tt.into_iter().peekable();
    let mut tokens: Vec<proc_macro2::TokenTree> = Vec::new();
    let mut defined_expansions = Vec::new();
    
    while let Some(tt) = iter.next() {
        tokens.push(tt);
        
        // Look for pattern: `[(); T + U]:`
        // This is a bracket group containing: Group(()), Punct(;), ...expression...
        // followed by a Punct(:) after the bracket group
        // Only process if well_formedness_syntax is enabled
        if config.well_formedness_syntax && tokens.len() >= 2 {
            let len = tokens.len();
            if let (
                proc_macro2::TokenTree::Group(bracket_group),
                proc_macro2::TokenTree::Punct(colon_after),
            ) = (&tokens[len-2], &tokens[len-1]) {
                if bracket_group.delimiter() == proc_macro2::Delimiter::Bracket &&
                   colon_after.as_char() == ':' {
                    let bracket_content = bracket_group.stream();
                    let bracket_tokens: Vec<proc_macro2::TokenTree> = bracket_content.into_iter().collect();
                    
                    // Check if bracket content starts with `();`
                    if bracket_tokens.len() >= 2 {
                        if let (
                            proc_macro2::TokenTree::Group(paren_group),
                            proc_macro2::TokenTree::Punct(semicolon),
                        ) = (&bracket_tokens[0], &bracket_tokens[1]) {
                            if paren_group.delimiter() == proc_macro2::Delimiter::Parenthesis &&
                               paren_group.stream().is_empty() &&
                               semicolon.as_char() == ';' {
                                // Found `[(); ... ]:` pattern - extract the expression
                                let expr_tokens: proc_macro2::TokenStream = bracket_tokens[2..]
                                    .iter()
                                    .cloned()
                                    .collect();
                                
                                if let Some(expanded) = try_parse_and_expand_defined_from_stream(expr_tokens) {
                                    // Use the bracket group span
                                    let bracket_span = bracket_group.span();
                                    defined_expansions.push((bracket_span, expanded.clone()));
                                    // Remove the bracket group and colon, replace with expanded bound
                                    tokens.truncate(len - 2);
                                    result.extend(tokens.drain(..));
                                    result.extend(expanded);
                                    continue;
                                }
                            }
                        }
                    }
                }
            }
        }
        
        // Look for pattern: `(): IsDefined<{...}>`
        // This means we need: Group(()), Punct(:), Ident(IsDefined), Punct(<), Group({...}), Punct(>)
        // Only process if marker_trait_syntax is enabled
        if config.marker_trait_syntax && tokens.len() >= 3 {
            let len = tokens.len();
            if let (
                proc_macro2::TokenTree::Group(group1),
                proc_macro2::TokenTree::Punct(colon),
                proc_macro2::TokenTree::Ident(ident),
            ) = (&tokens[len-3], &tokens[len-2], &tokens[len-1]) {
                if group1.delimiter() == proc_macro2::Delimiter::Parenthesis && 
                   group1.stream().is_empty() &&
                   colon.as_char() == ':' &&
                   ident.to_string() == config.marker_trait_name {
                    // Found `(): IsDefined` - check if next is `<{...}>` (const generic syntax)
                    let defined_span = ident.span();
                    
                    // Only support brace syntax: `IsDefined<{ ... }>`
                    if let Some(proc_macro2::TokenTree::Punct(lt)) = iter.peek() {
                        if lt.as_char() == '<' {
                            // Consume the `<`
                            iter.next();
                            
                            // Check if the next token is a Group with braces `{...}`
                            if let Some(proc_macro2::TokenTree::Group(group)) = iter.peek() {
                                if group.delimiter() == proc_macro2::Delimiter::Brace {
                                    // Consume the group
                                    let brace_group = iter.next().unwrap();
                                    if let proc_macro2::TokenTree::Group(brace_group) = brace_group {
                                        let brace_tokens = brace_group.stream();
                                        
                                        // Look for the closing `>`
                                        if let Some(proc_macro2::TokenTree::Punct(gt)) = iter.peek() {
                                            if gt.as_char() == '>' {
                                                iter.next();
                                                
                                                if let Some(expanded) = try_parse_and_expand_defined_from_stream(brace_tokens) {
                                                    // Capture the span and expansion for this IsDefined usage
                                                    defined_expansions.push((defined_span, expanded.clone()));
                                                    // Remove the last 3 tokens ((): IsDefined) and replace with expanded bound
                                                    tokens.truncate(len - 3);
                                                    result.extend(tokens.drain(..));
                                                    result.extend(expanded);
                                                    continue;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        
        // If we have too many tokens, flush the first one
        if tokens.len() > 10 {
            result.extend(std::iter::once(tokens.remove(0)));
        }
    }
    
    // Flush remaining tokens
    result.extend(tokens.into_iter());
    
    (result, defined_expansions)
}

fn try_parse_and_expand_defined_from_stream(arg_stream: proc_macro2::TokenStream) -> Option<proc_macro2::TokenStream> {
    // Parse the input stream which may contain:
    // 1. A simple operator expression: `T + U`
    // 2. An operator expression with Output assignment: `T + U = V`
    // 3. A chained operator expression: `T + U + V` → `T: Add<U, Output: Add<V>>`
    
    // First, try parsing as equals-separated: Expr = Expr (for Output type)
    let (main_expr, output_expr) = match parse_equals_separated(arg_stream.clone()) {
        Ok((expr, output)) => (expr, output),
        Err(_) => {
            // Try parsing as just an expression
            match syn::parse2::<Expr>(arg_stream) {
                Ok(expr) => (expr, None),
                Err(_) => return None,
            }
        }
    };
    
    // Build the trait bound, handling chained operators
    expand_expr_to_bound(&main_expr, output_expr.as_ref())
}

// Recursively expands an expression to a trait bound, handling chained operators
fn expand_expr_to_bound(expr: &Expr, output_assign: Option<&Expr>) -> Option<proc_macro2::TokenStream> {
    match expr {
        // Unwrap parentheses - syn respects precedence automatically, so we just unwrap
        Expr::Paren(expr_paren) => {
            expand_expr_to_bound(&expr_paren.expr, output_assign)
        }
        Expr::Binary(ExprBinary { left, op, right, .. }) => {
            let op_span = utils::get_op_span(op);
            let trait_name = utils::op_to_trait_spanned(op, op_span);
            
            // Check if left is also a binary expression with the same operator (chained)
            if let Expr::Binary(ExprBinary { op: left_op, .. }) = left.as_ref() {
                // Check if operators match (for chaining)
                if std::mem::discriminant(op) == std::mem::discriminant(left_op) {
                    // This is a chain: (T + U) + V or ((T + U) + V) + W, etc.
                    // Extract the base type and all the operations
                    let (base_type, mut operations) = extract_chain_operations(left.as_ref(), op);
                    operations.push((op_span, trait_name, right.clone()));
                    
                    // Build the nested bound
                    return Some(build_nested_bound(base_type, &operations, output_assign));
                }
            }
            
            // Not a chain, or operators don't match - treat as simple binary expression
            // Check if right is a complex expression - if so, nest the right's bound inside current Output
            if matches!(right.as_ref(), Expr::Binary(_) | Expr::Unary(_)) {
                // Right is a binary or unary expression - nest the right's bound inside current Output
                // For example, T + (T / T) = T should become: T: Add<T, Output: Div<T, Output = T>>
                if let Expr::Binary(ExprBinary { left: right_left, op: right_op, right: right_right, .. }) = right.as_ref() {
                    let right_op_span = utils::get_op_span(right_op);
                    let right_trait_name = utils::op_to_trait_spanned(right_op, right_op_span);
                    
                    let nested_output = if let Some(output) = output_assign {
                        quote! { Output: core::ops::#right_trait_name<#right_right, Output = #output> }
                    } else {
                        quote! { Output: core::ops::#right_trait_name<#right_right> }
                    };
                    
                    let trait_spanned = quote_spanned! { op_span => core::ops::#trait_name };
                    let generic_params = quote! { #right_left, #nested_output };
                    
                    return Some(quote! {
                        #left: #trait_spanned<#generic_params>
                    });
                } else if let Expr::Unary(ExprUnary { op: right_op, expr: right_expr, .. }) = right.as_ref() {
                    let right_op_span = utils::get_un_op_span(right_op);
                    let right_trait_name = utils::un_op_to_trait_spanned(right_op, right_op_span);
                    
                    let nested_output = if let Some(output) = output_assign {
                        quote! { Output: core::ops::#right_trait_name<Output = #output> }
                    } else {
                        quote! { Output: core::ops::#right_trait_name }
                    };
                    
                    let trait_spanned = quote_spanned! { op_span => core::ops::#trait_name };
                    return Some(quote! {
                        #left: #trait_spanned<#right_expr, #nested_output>
                    });
                }
            }
            
            // If left is a complex expression, we need to recursively expand it to build nested bounds
            if matches!(left.as_ref(), Expr::Binary(_) | Expr::Unary(_)) {
                // Left is a binary or unary expression - nest the current bound inside the left's Output
                // For example, T + T / T = T should become: T: Add<T, Output: Div<T, Output = T>>
                let left_op_span = if let Expr::Binary(ExprBinary { op: left_op, .. }) = left.as_ref() {
                    utils::get_op_span(left_op)
                } else if let Expr::Unary(ExprUnary { op: left_op, .. }) = left.as_ref() {
                    utils::get_un_op_span(left_op)
                } else {
                    return None;
                };
                
                let left_trait_name = if let Expr::Binary(ExprBinary { op: left_op, .. }) = left.as_ref() {
                    utils::op_to_trait_spanned(left_op, left_op_span)
                } else if let Expr::Unary(ExprUnary { op: left_op, .. }) = left.as_ref() {
                    utils::un_op_to_trait_spanned(left_op, left_op_span)
                } else {
                    return None;
                };
                
                // Extract the base type and right operand from the left expression
                let (left_base, left_right_operand) = if let Expr::Binary(ExprBinary { left: base, right: right_op, .. }) = left.as_ref() {
                    (base.clone(), right_op.clone())
                } else if let Expr::Unary(ExprUnary { expr: inner, .. }) = left.as_ref() {
                    (inner.clone(), inner.clone()) // For unary, use the inner expr as both
                } else {
                    return None;
                };
                
                // Build nested bound: left_base: Trait<left_right, Output: current_trait<right, Output = output>>
                let nested_output = if let Some(output) = output_assign {
                    quote! { Output: core::ops::#trait_name<#right, Output = #output> }
                } else {
                    // If right is also complex, recursively expand it
                    if matches!(right.as_ref(), Expr::Binary(_) | Expr::Unary(_)) {
                        if let Some(right_bound) = expand_expr_to_bound(right.as_ref(), None) {
                            quote! { Output: core::ops::#trait_name<#right, #right_bound> }
                        } else {
                            quote! { Output: core::ops::#trait_name<#right> }
                        }
                    } else {
                        quote! { Output: core::ops::#trait_name<#right> }
                    }
                };
                
                let left_trait_spanned = quote_spanned! { left_op_span => core::ops::#left_trait_name };
                let generic_params = quote! { #left_right_operand, #nested_output };
                
                return Some(quote! {
                    #left_base: #left_trait_spanned<#generic_params>
                });
            }
            
            // Left is a simple type identifier - use it directly
            let mut generic_params = quote! { #right };
            
            // Add Output = ... if specified
            if let Some(output) = output_assign {
                generic_params = quote! { #right, Output = #output };
            }
            
            Some(quote! {
                #left: core::ops::#trait_name<#generic_params>
            })
        }
        Expr::Unary(ExprUnary { op, expr: inner_expr, .. }) => {
            let op_span = utils::get_un_op_span(op);
            let trait_name = utils::un_op_to_trait_spanned(op, op_span);
            
            let mut generic_params = proc_macro2::TokenStream::new();
            
            // Add Output = ... if specified
            if let Some(output) = output_assign {
                generic_params = quote! { Output = #output };
            }
            
            if generic_params.is_empty() {
                Some(quote! {
                    #inner_expr: core::ops::#trait_name
                })
            } else {
                Some(quote! {
                    #inner_expr: core::ops::#trait_name<#generic_params>
                })
            }
        }
        _ => None,
    }
}

// Extracts the base type and all operations from a chained expression
// Returns (base_type, operations) where operations are in left-to-right order
fn extract_chain_operations(
    expr: &Expr,
    op: &syn::BinOp,
) -> (Box<Expr>, Vec<(proc_macro2::Span, proc_macro2::TokenStream, Box<Expr>)>) {
    match expr {
        // Unwrap parentheses - syn already handled precedence
        Expr::Paren(expr_paren) => {
            extract_chain_operations(&expr_paren.expr, op)
        }
        Expr::Binary(ExprBinary { left, op: left_op, right, .. }) => {
            if std::mem::discriminant(op) == std::mem::discriminant(left_op) {
                // Same operator - continue extracting from left
                let (base_type, mut operations) = extract_chain_operations(left.as_ref(), op);
                // Add the current operation
                let left_op_span = utils::get_op_span(left_op);
                let left_trait_name = utils::op_to_trait_spanned(left_op, left_op_span);
                operations.push((left_op_span, left_trait_name, right.clone()));
                (base_type, operations)
            } else {
                // Different operator - this is the base, extract operations from left
                let (base_type, mut operations) = extract_chain_operations(left.as_ref(), left_op);
                // Add the current operation
                let left_op_span = utils::get_op_span(left_op);
                let left_trait_name = utils::op_to_trait_spanned(left_op, left_op_span);
                operations.push((left_op_span, left_trait_name, right.clone()));
                (base_type, operations)
            }
        }
        _ => {
            // Base case: not a binary expression - this is the base type
            (Box::new(expr.clone()), Vec::new())
        }
    }
}

// Builds a nested bound like `T: Add<U, Output: Add<V, Output: Add<W>>>`
// operations should be in order from leftmost to rightmost
// For T + U + V + W, operations = [(Add, U), (Add, V), (Add, W)]
fn build_nested_bound(
    base_type: Box<Expr>,
    operations: &[(proc_macro2::Span, proc_macro2::TokenStream, Box<Expr>)],
    output_assign: Option<&Expr>,
) -> proc_macro2::TokenStream {
    if operations.is_empty() {
        return quote! {};
    }
    
    // For T + U + V + W, we want: T: Add<U, Output: Add<V, Output: Add<W>>>
    // Build from rightmost (innermost) to leftmost (outermost)
    
    if operations.len() == 1 {
        // Single operation: T: Add<U>
        let (span, trait_name, right) = &operations[0];
        let trait_spanned = quote_spanned! { *span => core::ops::#trait_name };
        let mut generic_params = quote! { #right };
        if let Some(output) = output_assign {
            generic_params = quote! { #right, Output = #output };
        }
        return quote! {
            #base_type: #trait_spanned<#generic_params>
        };
    }
    
    // Multiple operations: build nested Output bounds
    let (last_span, last_trait, last_right) = &operations[operations.len() - 1];
    let last_trait_spanned = quote_spanned! { *last_span => core::ops::#last_trait };
    
    // Start with the innermost bound: Output: Add<W> or Output: Add<W, Output = X>
    let mut nested_output = if let Some(output) = output_assign {
        quote! {
            Output: #last_trait_spanned<#last_right, Output = #output>
        }
    } else {
        quote! {
            Output: #last_trait_spanned<#last_right>
        }
    };
    
    // Build nested Output bounds from second-to-last down to second operation
    // For T + U + V + W: operations = [(Add, U), (Add, V), (Add, W)]
    // We iterate over [(Add, V)] (skip last, stop before first)
    for (span, trait_name, right) in operations.iter().rev().skip(1).take(operations.len() - 2) {
        let trait_spanned = quote_spanned! { *span => core::ops::#trait_name };
        nested_output = quote! {
            Output: #trait_spanned<#right, #nested_output>
        };
    }
    
    // Add the outermost operation
    let (first_span, first_trait, first_right) = &operations[0];
    let first_trait_spanned = quote_spanned! { *first_span => core::ops::#first_trait };
    
    let generic_params = quote! { #first_right, #nested_output };
    
    quote! {
        #base_type: #first_trait_spanned<#generic_params>
    }
}

fn parse_equals_separated(input: proc_macro2::TokenStream) -> syn::Result<(Expr, Option<Expr>)> {
    // Parse as equals-separated: Expr = Expr
    // We need to manually split on `=` because `=` is a valid binary operator (equality)
    // in Rust expressions, so syn would parse `T + U = V` as an equality comparison.
    
    // Convert to a vector of tokens so we can find the `=` separator
    let tokens: Vec<proc_macro2::TokenTree> = input.into_iter().collect();
    
    // Collect tokens up to the first top-level `=`
    // Groups are atomic (we see the whole group as one token), so we don't need
    // to track depth - we just split on the first `=` we see
    let mut first_tokens = proc_macro2::TokenStream::new();
    let mut second_tokens = proc_macro2::TokenStream::new();
    let mut found_equals = false;
    
    for tt in tokens {
        if found_equals {
            second_tokens.extend(std::iter::once(tt));
            continue;
        }
        
        if let proc_macro2::TokenTree::Punct(p) = &tt {
            if p.as_char() == '=' {
                found_equals = true;
                // Don't add `=` to either stream
                continue;
            }
        }
        
        first_tokens.extend(std::iter::once(tt));
    }
    
    let first = syn::parse2::<Expr>(first_tokens)?;
    let second = if found_equals {
        Some(syn::parse2::<Expr>(second_tokens)?)
    } else {
        None
    };
    
    Ok((first, second))
}

