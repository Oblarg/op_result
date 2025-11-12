use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, quote_spanned};
use syn::{Expr, ExprBinary, ExprUnary, Item, ItemFn};

use crate::utils;

#[derive(Clone)]
struct OpResultConfig {
    well_formedness_syntax: bool, // true = enable well-formedness syntax [(); ...]:
    marker_trait_syntax: bool,    // true = enable marker trait syntax (): IsDefined<{...}>
    marker_trait_name: String,    // name of the marker trait (default: "IsDefined")
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
    let (expanded_ts, mut defined_expansions) =
        expand_defined_in_token_tree(item_ts, config.clone());

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
                let throwaway_fn = quote! {
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
                        let throwaway_fn = quote! {
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
                Err(e) => TokenStream::from(e.to_compile_error()),
            }
        }
    }
}

// Recursively processes an item and all nested items, processing where clauses in each
// Returns (processed_item, collected_expansions)
fn process_item_recursive(
    item: Item,
    config: OpResultConfig,
) -> (
    proc_macro2::TokenStream,
    Vec<(Span, proc_macro2::TokenStream)>,
) {
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
                    let (processed_ts, expansions) =
                        process_item_recursive(item.clone(), config.clone());
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
        Item::Const(item_const) => (quote! { #item_const }, Vec::new()),
        Item::Static(item_static) => (quote! { #item_static }, Vec::new()),
        Item::ForeignMod(item_foreign) => (quote! { #item_foreign }, Vec::new()),
        Item::Use(item_use) => (quote! { #item_use }, Vec::new()),
        Item::Macro(item_macro) => (quote! { #item_macro }, Vec::new()),
        Item::Verbatim(item_verbatim) => (quote! { #item_verbatim }, Vec::new()),
        _ => {
            // For any other item type, just quote it as-is
            (quote! { #item }, Vec::new())
        }
    }
}

// Process an impl item (method, associated type, etc.)
// Returns (processed_item, collected_expansions)
fn process_impl_item(
    item: syn::ImplItem,
    config: OpResultConfig,
) -> (syn::ImplItem, Vec<(Span, proc_macro2::TokenStream)>) {
    match item {
        syn::ImplItem::Fn(method) => {
            // Process the method's where clause recursively
            let method_ts = quote! { #method };
            let (expanded_ts, expansions) = expand_defined_in_token_tree(method_ts, config);
            match syn::parse2::<syn::ImplItem>(expanded_ts) {
                Ok(processed) => (processed, expansions),
                Err(_) => (syn::ImplItem::Fn(method), expansions), // Fallback to original, but keep expansions
            }
        }
        syn::ImplItem::Type(ty) => {
            // Process the associated type's where clause
            let ty_ts = quote! { #ty };
            let (expanded_ts, expansions) = expand_defined_in_token_tree(ty_ts, config);
            match syn::parse2::<syn::ImplItem>(expanded_ts) {
                Ok(processed) => (processed, expansions),
                Err(_) => (syn::ImplItem::Type(ty), expansions), // Fallback to original, but keep expansions
            }
        }
        syn::ImplItem::Const(item_const) => (syn::ImplItem::Const(item_const), Vec::new()),
        syn::ImplItem::Macro(item_macro) => (syn::ImplItem::Macro(item_macro), Vec::new()),
        syn::ImplItem::Verbatim(item_verbatim) => {
            (syn::ImplItem::Verbatim(item_verbatim), Vec::new())
        }
        _ => (item, Vec::new()),
    }
}

// Process a trait item (method, associated type, etc.)
// Returns (processed_item, collected_expansions)
fn process_trait_item(
    item: syn::TraitItem,
    config: OpResultConfig,
) -> (syn::TraitItem, Vec<(Span, proc_macro2::TokenStream)>) {
    match item {
        syn::TraitItem::Fn(method) => {
            // Process the method's where clause recursively
            let method_ts = quote! { #method };
            let (expanded_ts, expansions) = expand_defined_in_token_tree(method_ts, config);
            match syn::parse2::<syn::TraitItem>(expanded_ts) {
                Ok(processed) => (processed, expansions),
                Err(_) => (syn::TraitItem::Fn(method), expansions), // Fallback to original, but keep expansions
            }
        }
        syn::TraitItem::Type(ty) => {
            // Process the associated type's where clause
            let ty_ts = quote! { #ty };
            let (expanded_ts, expansions) = expand_defined_in_token_tree(ty_ts, config);
            match syn::parse2::<syn::TraitItem>(expanded_ts) {
                Ok(processed) => (processed, expansions),
                Err(_) => (syn::TraitItem::Type(ty), expansions), // Fallback to original, but keep expansions
            }
        }
        syn::TraitItem::Const(item_const) => (syn::TraitItem::Const(item_const), Vec::new()),
        syn::TraitItem::Macro(item_macro) => (syn::TraitItem::Macro(item_macro), Vec::new()),
        syn::TraitItem::Verbatim(item_verbatim) => {
            (syn::TraitItem::Verbatim(item_verbatim), Vec::new())
        }
        _ => (item, Vec::new()),
    }
}

fn expand_defined_in_token_tree(
    tt: proc_macro2::TokenStream,
    config: OpResultConfig,
) -> (
    proc_macro2::TokenStream,
    Vec<(Span, proc_macro2::TokenStream)>,
) {
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
            ) = (&tokens[len - 2], &tokens[len - 1])
            {
                if bracket_group.delimiter() == proc_macro2::Delimiter::Bracket
                    && colon_after.as_char() == ':'
                {
                    let bracket_content = bracket_group.stream();
                    let bracket_tokens: Vec<proc_macro2::TokenTree> =
                        bracket_content.into_iter().collect();

                    // Check if bracket content starts with `();`
                    if bracket_tokens.len() >= 2 {
                        if let (
                            proc_macro2::TokenTree::Group(paren_group),
                            proc_macro2::TokenTree::Punct(semicolon),
                        ) = (&bracket_tokens[0], &bracket_tokens[1])
                        {
                            if paren_group.delimiter() == proc_macro2::Delimiter::Parenthesis
                                && paren_group.stream().is_empty()
                                && semicolon.as_char() == ';'
                            {
                                // Found `[(); ... ]:` pattern - extract the expression
                                let expr_tokens: proc_macro2::TokenStream =
                                    bracket_tokens[2..].iter().cloned().collect();

                                if let Some(expanded) =
                                    try_parse_and_expand_defined_from_stream(expr_tokens)
                                {
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
            ) = (&tokens[len - 3], &tokens[len - 2], &tokens[len - 1])
            {
                if group1.delimiter() == proc_macro2::Delimiter::Parenthesis
                    && group1.stream().is_empty()
                    && colon.as_char() == ':'
                    && ident.to_string() == config.marker_trait_name
                {
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
                                    if let proc_macro2::TokenTree::Group(brace_group) = brace_group
                                    {
                                        let brace_tokens = brace_group.stream();

                                        // Look for the closing `>`
                                        if let Some(proc_macro2::TokenTree::Punct(gt)) = iter.peek()
                                        {
                                            if gt.as_char() == '>' {
                                                iter.next();

                                                if let Some(expanded) =
                                                    try_parse_and_expand_defined_from_stream(
                                                        brace_tokens,
                                                    )
                                                {
                                                    // Capture the span and expansion for this IsDefined usage
                                                    defined_expansions
                                                        .push((defined_span, expanded.clone()));
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
    }

    // Flush remaining tokens
    result.extend(tokens.into_iter());

    (result, defined_expansions)
}

fn try_parse_and_expand_defined_from_stream(
    arg_stream: proc_macro2::TokenStream,
) -> Option<proc_macro2::TokenStream> {
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

// Extracts trait information from a binary or unary expression
// Returns (trait_name, nested_output_operand, generic_param_operand)
// where nested_output_operand is Some for binary ops, None for unary ops
fn extract_trait_info_from_expr(
    expr: &Expr,
) -> Option<(proc_macro2::TokenStream, Option<Box<Expr>>, Box<Expr>)> {
    macro_rules! extract_trait_info {
        ($op:expr, $get_span:path, $get_trait:path, $nested_output:expr, $generic_param:expr) => {{
            let op_span = $get_span($op);
            let trait_name = $get_trait($op, op_span);
            Some((trait_name, $nested_output, $generic_param))
        }};
    }

    match expr {
        Expr::Binary(ExprBinary {
            left, op, right, ..
        }) => {
            extract_trait_info!(
                op,
                utils::get_op_span,
                utils::op_to_trait_spanned,
                Some(right.clone()),
                left.clone()
            )
        }
        Expr::Unary(ExprUnary {
            op,
            expr: inner_expr,
            ..
        }) => {
            extract_trait_info!(
                op,
                utils::get_un_op_span,
                utils::un_op_to_trait_spanned,
                None,
                inner_expr.clone()
            )
        }
        _ => None,
    }
}

// Recursively expands an expression to a trait bound, handling chained operators
fn expand_expr_to_bound(
    expr: &Expr,
    output_assign: Option<&Expr>,
) -> Option<proc_macro2::TokenStream> {
    match expr {
        // Unwrap parentheses - syn respects precedence automatically, so we just unwrap
        Expr::Paren(expr_paren) => expand_expr_to_bound(&expr_paren.expr, output_assign),
        Expr::Binary(ExprBinary {
            left, op, right, ..
        }) => {
            let op_span = utils::get_op_span(op);
            let trait_name = utils::op_to_trait_spanned(op, op_span);

            // Branch 1: RIGHT operand is a complex expression (binary/unary)
            // Nest the right side's trait bound inside the current operation's Output type.
            // Example: T + (U / V) becomes: T: Add<U, Output: Div<V>>
            // Example: T + (U / V) = W becomes: T: Add<U, Output: Div<V, Output = W>>
            if let Some((right_trait_name, nested_output_operand, generic_param_operand)) =
                extract_trait_info_from_expr(right.as_ref())
            {
                // Build generic params for the nested trait
                let nested_trait_params = match (nested_output_operand, output_assign) {
                    (Some(operand), Some(output)) => quote! { #operand, Output = #output },
                    (Some(operand), None) => quote! { #operand },
                    (None, Some(output)) => quote! { Output = #output },
                    (None, None) => proc_macro2::TokenStream::new(),
                };

                let nested_output = if nested_trait_params.is_empty() {
                    quote! { Output: core::ops::#right_trait_name }
                } else {
                    quote! { Output: core::ops::#right_trait_name<#nested_trait_params> }
                };

                let trait_spanned = quote_spanned! { op_span => core::ops::#trait_name };
                let generic_params = quote! { #generic_param_operand, #nested_output };

                return Some(quote! {
                    #left: #trait_spanned<#generic_params>
                });
            }

            // Branch 2: LEFT operand is a complex expression (binary/unary)
            // Extract all operations from the left side to preserve operator precedence,
            // then add the current operation to build a complete nested bound structure.
            // Example: (T + U) * V needs to extract T + U first, then add * V
            // This handles mixed operators and ensures correct nesting order.
            if matches!(left.as_ref(), Expr::Binary(_) | Expr::Unary(_)) {
                // Verify the left side can be expanded (is a valid operation expression)
                if expand_expr_to_bound(left.as_ref(), None).is_some() {
                    // Extract all operations from left expression (handles both binary and unary)
                    let (base_type, mut operations) = extract_all_operations(left.as_ref());

                    // Add the current operation to the sequence
                    let op_span = utils::get_op_span(op);
                    let trait_name = utils::op_to_trait_spanned(op, op_span);
                    operations.push((op_span, trait_name, Some(right.clone())));

                    // Build the nested bound with all operations in correct order
                    return Some(build_mixed_nested_bound(
                        base_type,
                        &operations,
                        output_assign,
                    ));
                }
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
        Expr::Unary(ExprUnary {
            op,
            expr: inner_expr,
            ..
        }) => {
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

// Extracts all operations from an expression in left-to-right order
// Returns (base_type, operations) where operations can have different operators
// For unary operations, the right operand is None
fn extract_all_operations(
    expr: &Expr,
) -> (
    Box<Expr>,
    Vec<(
        proc_macro2::Span,
        proc_macro2::TokenStream,
        Option<Box<Expr>>,
    )>,
) {
    match expr {
        Expr::Paren(expr_paren) => extract_all_operations(&expr_paren.expr),
        Expr::Binary(ExprBinary {
            left, op, right, ..
        }) => {
            let (base_type, mut operations) = extract_all_operations(left.as_ref());
            let op_span = utils::get_op_span(op);
            let trait_name = utils::op_to_trait_spanned(op, op_span);
            operations.push((op_span, trait_name, Some(right.clone())));
            (base_type, operations)
        }
        Expr::Unary(ExprUnary {
            op, expr: inner, ..
        }) => {
            let (base_type, mut operations) = extract_all_operations(inner.as_ref());
            let op_span = utils::get_un_op_span(op);
            let trait_name = utils::un_op_to_trait_spanned(op, op_span);
            operations.push((op_span, trait_name, None)); // None indicates unary operation
            (base_type, operations)
        }
        _ => (Box::new(expr.clone()), Vec::new()),
    }
}

// Builds a nested bound from a list of operations (can have different operators)
// For unary operations, right operand is None
fn build_mixed_nested_bound(
    base_type: Box<Expr>,
    operations: &[(
        proc_macro2::Span,
        proc_macro2::TokenStream,
        Option<Box<Expr>>,
    )],
    output_assign: Option<&Expr>,
) -> proc_macro2::TokenStream {
    if operations.is_empty() {
        return quote! {};
    }

    if operations.len() == 1 {
        let (span, trait_name, right) = &operations[0];
        let trait_spanned = quote_spanned! { *span => core::ops::#trait_name };
        let generic_params = match right {
            Some(right) => {
                if let Some(output) = output_assign {
                    quote! { #right, Output = #output }
                } else {
                    quote! { #right }
                }
            }
            None => {
                // Unary operation - only Output parameter
                if let Some(output) = output_assign {
                    quote! { Output = #output }
                } else {
                    proc_macro2::TokenStream::new()
                }
            }
        };

        if generic_params.is_empty() {
            return quote! {
                #base_type: #trait_spanned
            };
        } else {
            return quote! {
                #base_type: #trait_spanned<#generic_params>
            };
        }
    }

    // Build from rightmost (innermost) to leftmost (outermost)
    let (last_span, last_trait, last_right) = &operations[operations.len() - 1];
    let last_trait_spanned = quote_spanned! { *last_span => core::ops::#last_trait };

    let mut nested_output = match last_right {
        Some(last_right) => {
            if let Some(output) = output_assign {
                quote! {
                    Output: #last_trait_spanned<#last_right, Output = #output>
                }
            } else {
                quote! {
                    Output: #last_trait_spanned<#last_right>
                }
            }
        }
        None => {
            // Unary operation - only Output parameter
            if let Some(output) = output_assign {
                quote! {
                    Output: #last_trait_spanned<Output = #output>
                }
            } else {
                quote! {
                    Output: #last_trait_spanned
                }
            }
        }
    };

    // Build nested Output bounds from second-to-last down to second operation
    for (span, trait_name, right) in operations.iter().rev().skip(1).take(operations.len() - 2) {
        let trait_spanned = quote_spanned! { *span => core::ops::#trait_name };
        nested_output = match right {
            Some(right) => quote! {
                Output: #trait_spanned<#right, #nested_output>
            },
            None => quote! {
                Output: #trait_spanned<#nested_output>
            },
        };
    }

    // Add the outermost operation
    let (first_span, first_trait, first_right) = &operations[0];
    let first_trait_spanned = quote_spanned! { *first_span => core::ops::#first_trait };

    let generic_params = match first_right {
        Some(first_right) => quote! { #first_right, #nested_output },
        None => nested_output, // For unary, the nested_output becomes the generic params
    };

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
