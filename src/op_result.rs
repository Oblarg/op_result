use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote};
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
                        /// Asserts the well-formedness of an operator definition expression
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
    // The arg_stream might be wrapped in braces (for const generic expressions)
    // or it might be a direct expression. Try parsing both ways.
    
    // First, try parsing as comma-separated: Expr, Expr? (for Output type)
    match parse_defined_input(arg_stream.clone()) {
        Ok((first_expr, second_expr)) => {
            // Check if first_expr is a binary expression
            if let Expr::Binary(ExprBinary { left, op, right, .. }) = &first_expr {
                let op_span = utils::get_op_span(op);
                let trait_name = utils::op_to_trait_spanned(op, op_span);
                
                if let Some(output_expr) = second_expr {
                    return Some(quote! {
                        #left: std::ops::#trait_name<#right, Output = #output_expr>
                    });
                } else {
                    return Some(quote! {
                        #left: std::ops::#trait_name<#right>
                    });
                }
            }
            // Check if first_expr is a unary expression
            if let Expr::Unary(ExprUnary { op, expr: inner_expr, .. }) = &first_expr {
                let op_span = utils::get_un_op_span(op);
                let trait_name = utils::un_op_to_trait_spanned(op, op_span);
                
                if let Some(output_expr) = second_expr {
                    return Some(quote! {
                        #inner_expr: std::ops::#trait_name<Output = #output_expr>
                    });
                } else {
                    return Some(quote! {
                        #inner_expr: std::ops::#trait_name
                    });
                }
            }
        }
        Err(_) => {
            // Try parsing as just an expression (no Output type)
            if let Ok(expr) = syn::parse2::<Expr>(arg_stream.clone()) {
                if let Expr::Binary(ExprBinary { left, op, right, .. }) = &expr {
                    let op_span = utils::get_op_span(op);
                    let trait_name = utils::op_to_trait_spanned(op, op_span);
                    return Some(quote! {
                        #left: std::ops::#trait_name<#right>
                    });
                }
                if let Expr::Unary(ExprUnary { op, expr: inner_expr, .. }) = &expr {
                    let op_span = utils::get_un_op_span(op);
                    let trait_name = utils::un_op_to_trait_spanned(op, op_span);
                    return Some(quote! {
                        #inner_expr: std::ops::#trait_name
                    });
                }
            }
        }
    }
    
    None
}


fn parse_defined_input(input: proc_macro2::TokenStream) -> syn::Result<(Expr, Option<Expr>)> {
    // Parse as comma-separated: Expr, Expr?
    struct DefinedInput {
        first: Expr,
        second: Option<Expr>,
    }
    
    impl syn::parse::Parse for DefinedInput {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            let first = input.parse()?;
            let second = if input.peek(syn::Token![,]) {
                let _comma: syn::Token![,] = input.parse()?;
                Some(input.parse()?)
            } else {
                None
            };
            Ok(DefinedInput { first, second })
        }
    }
    
    syn::parse2::<DefinedInput>(input).map(|parsed| (parsed.first, parsed.second))
}

