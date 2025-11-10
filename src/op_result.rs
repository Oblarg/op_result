use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, quote_spanned};
use syn::{Expr, ExprBinary, ExprUnary, ItemFn};

use crate::utils;

pub fn expand_op_result(_attr: TokenStream, item: TokenStream) -> TokenStream {
    // Process the entire token stream, find `(): IsDefined<{...}>` patterns and transform them
    let item_ts = proc_macro2::TokenStream::from(item);
    
    // Expand all `(): IsDefined<{...}>` patterns in the token stream
    let (expanded_ts, defined_expansions) = expand_defined_in_tokens(item_ts);
    
    // Now parse the function with the expanded where clause
    match syn::parse2::<ItemFn>(expanded_ts) {
        Ok(item_fn) => {
            // Create type aliases for IDE hover support - one for each IsDefined usage
            // Each shows what that specific usage expands to
            let mut defined_types = proc_macro2::TokenStream::new();
            
            // Create a type alias for each IsDefined usage with its specific expansion
            // Each is in its own throwaway function scope so they don't conflict
            for (_idx, (span, expansion)) in defined_expansions.iter().enumerate() {
                let expansion_str: String = expansion.to_string();
                let expansion_doc = format!("**Expands to:** `{}`", expansion_str);
                let defined_ident = proc_macro2::Ident::new("IsDefined", *span);
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
            
            // Successfully parsed - quote it back with the type definitions
            // This preserves all macro invocations (including `output!`) in the function body
            TokenStream::from(quote! {
                const _: () = {
                    #defined_types
                };
                #item_fn
            })
        }
        Err(e) => {
            // If parsing fails, return a compile error
            TokenStream::from(e.to_compile_error())
        }
    }
}

fn expand_defined_in_tokens(tokens: proc_macro2::TokenStream) -> (proc_macro2::TokenStream, Vec<(Span, proc_macro2::TokenStream)>) {
    expand_defined_in_token_tree(tokens)
}


fn expand_defined_in_token_tree(tt: proc_macro2::TokenStream) -> (proc_macro2::TokenStream, Vec<(Span, proc_macro2::TokenStream)>) {
    let mut result = proc_macro2::TokenStream::new();
    let mut iter = tt.into_iter().peekable();
    let mut tokens: Vec<proc_macro2::TokenTree> = Vec::new();
    let mut defined_expansions = Vec::new();
    
    while let Some(tt) = iter.next() {
        tokens.push(tt);
        
        // Look for pattern: `[(); T + U]:`
        // This is a bracket group containing: Group(()), Punct(;), ...expression...
        // followed by a Punct(:) after the bracket group
        if tokens.len() >= 2 {
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
        if tokens.len() >= 3 {
            let len = tokens.len();
            if let (
                proc_macro2::TokenTree::Group(group1),
                proc_macro2::TokenTree::Punct(colon),
                proc_macro2::TokenTree::Ident(ident),
            ) = (&tokens[len-3], &tokens[len-2], &tokens[len-1]) {
                if group1.delimiter() == proc_macro2::Delimiter::Parenthesis && 
                   group1.stream().is_empty() &&
                   colon.as_char() == ':' &&
                   ident.to_string() == "IsDefined" {
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
            let mut generic_params = quote! { #right };
            
            // Add Output = ... if specified
            if let Some(output) = output_assign {
                generic_params = quote! { #right, Output = #output };
            }
            
            Some(quote! {
                #left: std::ops::#trait_name<#generic_params>
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
                    #inner_expr: std::ops::#trait_name
                })
            } else {
                Some(quote! {
                    #inner_expr: std::ops::#trait_name<#generic_params>
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
        let trait_spanned = quote_spanned! { *span => std::ops::#trait_name };
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
    let last_trait_spanned = quote_spanned! { *last_span => std::ops::#last_trait };
    
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
        let trait_spanned = quote_spanned! { *span => std::ops::#trait_name };
        nested_output = quote! {
            Output: #trait_spanned<#right, #nested_output>
        };
    }
    
    // Add the outermost operation
    let (first_span, first_trait, first_right) = &operations[0];
    let first_trait_spanned = quote_spanned! { *first_span => std::ops::#first_trait };
    
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

