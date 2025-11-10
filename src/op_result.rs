use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, quote_spanned};
use syn::{Expr, ExprBinary, ExprUnary, ItemFn};

use crate::output;
use crate::utils;

pub fn expand_op_result(_attr: TokenStream, item: TokenStream) -> TokenStream {
    // Process the entire token stream, find `(): IsDefined<{...}>` patterns and transform them
    let item_ts = proc_macro2::TokenStream::from(item);
    
    // Expand all `Output<...>` and `(): IsDefined<{...}>` patterns in the token stream
    // `Output<>` is expanded first so it can be used inside `IsDefined<{...}>` expressions
    let (expanded_ts, defined_expansions) = expand_defined_in_tokens(item_ts);
    
    // Now parse the function with the expanded where clause
    match syn::parse2::<ItemFn>(expanded_ts) {
        Ok(item_fn) => {
            // Create type aliases for IDE hover support - one for each IsDefined usage
            // Each shows what that specific usage expands to
            let mut defined_types = proc_macro2::TokenStream::new();
            
            if defined_expansions.is_empty() {
                // If no IsDefined usages were found, add a default one for completeness
                let fn_span = item_fn.sig.ident.span();
                let defined_type = quote_spanned! { fn_span =>
                    /// Checker trait used in `(): IsDefined<{ ... }>` syntax to express trait bounds.
                    /// 
                    /// This is a marker type that exists only for IDE hover support.
                    /// The `#[op_result]` attribute macro transforms `(): IsDefined<{ T + U }>` 
                    /// into `T: std::ops::Add<U>`, or `(): IsDefined<{ T - U }>` into `T: std::ops::Sub<U>`.
                    #[allow(dead_code)]
                    type IsDefined<T> = T;
                };
                defined_types.extend(defined_type);
            } else {
                // Create a type alias for each IsDefined usage with its specific expansion
                // Each is in its own throwaway function scope so they don't conflict
                for (idx, (span, expansion)) in defined_expansions.iter().enumerate() {
                    // Convert the expansion tokens to a readable string for the docstring
                    let expansion_str: String = expansion.to_string();
                    let fn_name = proc_macro2::Ident::new(
                        &format!("__op_result_is_defined_{}", idx),
                        proc_macro2::Span::call_site()
                    );
                    let expansion_doc = format!("**Expands to:** `{}`", expansion_str);
                    // Use quote_spanned only for the IsDefined type name
                    let defined_ident = proc_macro2::Ident::new("IsDefined", *span);
                    let throwaway_fn = quote! {
                        #[allow(dead_code)]
                        fn #fn_name() {
                            /// Checker trait used in `(): IsDefined<{ ... }>` syntax to express trait bounds.
                            /// 
                            /// This is a marker type that exists only for IDE hover support.
                            /// 
                            #[doc = #expansion_doc]
                            type #defined_ident<T> = T;
                            let _ = ();
                        }
                    };
                    defined_types.extend(throwaway_fn);
                }
            }
            
            // Successfully parsed - quote it back with the type definitions
            // This preserves all macro invocations (including `output!`) in the function body
            TokenStream::from(quote! {
                #defined_types
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
    // First expand Output<> patterns, then IsDefined<> patterns
    // This allows Output<> to work inside IsDefined<> expressions
    let expanded = expand_output_in_tokens(tokens);
    expand_defined_in_token_tree(expanded)
}

/// Expand `Output<...>` patterns to their corresponding associated type syntax.
/// This works recursively and can be used inside `IsDefined<>` expressions.
fn expand_output_in_tokens(tokens: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    expand_output_in_token_tree(tokens)
}

fn expand_output_in_token_tree(tt: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    let mut result = proc_macro2::TokenStream::new();
    let mut iter = tt.into_iter().peekable();
    let mut tokens: Vec<proc_macro2::TokenTree> = Vec::new();
    
    while let Some(tt) = iter.next() {
        tokens.push(tt);
        
        // Look for pattern: `Output<...>`
        // This means we need: Ident(Output), Punct(<), ...content..., Punct(>)
        if tokens.len() >= 1 {
            let len = tokens.len();
            if let proc_macro2::TokenTree::Ident(ident) = &tokens[len-1] {
                if ident.to_string() == "Output" {
                    // Found `Output` - check if next is `<`
                    if let Some(proc_macro2::TokenTree::Punct(lt)) = iter.peek() {
                        if lt.as_char() == '<' {
                            // Consume the `<`
                            iter.next();
                            // Look for the closing `>` - we need to collect everything between
                            let mut angle_bracket_tokens = proc_macro2::TokenStream::new();
                            let mut depth = 1;
                            while depth > 0 {
                                if let Some(tt) = iter.next() {
                                    match &tt {
                                        proc_macro2::TokenTree::Punct(p) if p.as_char() == '<' => depth += 1,
                                        proc_macro2::TokenTree::Punct(p) if p.as_char() == '>' => depth -= 1,
                                        _ => {}
                                    }
                                    if depth > 0 {
                                        angle_bracket_tokens.extend(std::iter::once(tt));
                                    }
                                } else {
                                    break;
                                }
                            }
                            
                            // Recursively expand any nested Output<> or IsDefined<> patterns
                            let expanded_inner = expand_output_in_tokens(angle_bracket_tokens.clone());
                            
                            // Try to parse and expand the expression
                            if let Some(expanded) = try_expand_output_from_stream(expanded_inner) {
                                // Remove the last token (Output) and replace with expanded expression
                                tokens.truncate(len - 1);
                                result.extend(tokens.drain(..));
                                result.extend(expanded);
                                continue;
                            } else {
                                // If we couldn't expand, put back the tokens we collected
                                tokens.push(proc_macro2::TokenTree::Punct(
                                    proc_macro2::Punct::new('<', proc_macro2::Spacing::Alone)
                                ));
                                // Reconstruct the angle bracket tokens
                                let mut angle_iter = angle_bracket_tokens.into_iter();
                                while let Some(tt) = angle_iter.next() {
                                    tokens.push(tt);
                                }
                                tokens.push(proc_macro2::TokenTree::Punct(
                                    proc_macro2::Punct::new('>', proc_macro2::Spacing::Alone)
                                ));
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
    
    result
}

/// Try to expand an `Output<...>` expression to its associated type syntax.
fn try_expand_output_from_stream(arg_stream: proc_macro2::TokenStream) -> Option<proc_macro2::TokenStream> {
    // Try parsing as an expression
    if let Ok(expr) = syn::parse2::<Expr>(arg_stream) {
        // Use the same expansion logic as the output! macro
        return Some(output::expand_expr(&expr));
    }
    None
}

fn expand_defined_in_token_tree(tt: proc_macro2::TokenStream) -> (proc_macro2::TokenStream, Vec<(Span, proc_macro2::TokenStream)>) {
    let mut result = proc_macro2::TokenStream::new();
    let mut iter = tt.into_iter().peekable();
    let mut tokens: Vec<proc_macro2::TokenTree> = Vec::new();
    let mut defined_expansions = Vec::new();
    
    while let Some(tt) = iter.next() {
        tokens.push(tt);
        
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
                                                
                                                // Expand any Output<> patterns inside the IsDefined expression
                                                let expanded_bracket_tokens = expand_output_in_tokens(brace_tokens);
                                                
                                                if let Some(expanded) = try_parse_and_expand_defined_from_stream(expanded_bracket_tokens) {
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
            // Try parsing as an expression
            if let Ok(expr) = syn::parse2::<Expr>(arg_stream.clone()) {
                if let Expr::Binary(ExprBinary { left, op, right, .. }) = &expr {
                    let op_span = utils::get_op_span(op);
                    let trait_name = utils::op_to_trait_spanned(op, op_span);
                    // Preserve macro invocations in left/right sides
                    return Some(quote! {
                        #left: std::ops::#trait_name<#right>
                    });
                }
                if let Expr::Unary(ExprUnary { op, expr: inner_expr, .. }) = &expr {
                    let op_span = utils::get_un_op_span(op);
                    let trait_name = utils::un_op_to_trait_spanned(op, op_span);
                    // Preserve macro invocations in inner expression
                    return Some(quote! {
                        #inner_expr: std::ops::#trait_name
                    });
                }
            }
            
            // If expression parsing failed, it might be because Output<> is in the expression
            // Try manual parsing first (before trying parentheses) to handle macro invocations
            if let Some(expanded) = try_parse_binary_with_macros(arg_stream.clone()) {
                return Some(expanded);
            }
            // Try parsing unary expressions manually
            if let Some(expanded) = try_parse_unary_with_macros(arg_stream.clone()) {
                return Some(expanded);
            }
            
            // Try wrapping in parentheses to help syn parse it
            let paren_wrapped = quote! { (#arg_stream) };
            if let Ok(expr) = syn::parse2::<Expr>(paren_wrapped) {
                if let Expr::Paren(paren_expr) = &expr {
                    if let Expr::Binary(ExprBinary { left, op, right, .. }) = paren_expr.expr.as_ref() {
                        let op_span = utils::get_op_span(op);
                        let trait_name = utils::op_to_trait_spanned(op, op_span);
                        // Preserve macro invocations in left/right
                        return Some(quote! {
                            #left: std::ops::#trait_name<#right>
                        });
                    }
                    if let Expr::Unary(ExprUnary { op, expr: inner_expr, .. }) = paren_expr.expr.as_ref() {
                        let op_span = utils::get_un_op_span(op);
                        let trait_name = utils::un_op_to_trait_spanned(op, op_span);
                        // Preserve macro invocations in inner expression
                        return Some(quote! {
                            #inner_expr: std::ops::#trait_name
                        });
                    }
                }
            }
            
            // If expression parsing failed, it might be because Output<> expanded to a type
            // Try parsing as a type expression (like `<T as Add<U>>::Output + V`)
            // We can try wrapping it in parentheses to help parsing
            let type_expr_attempt = quote! { (#arg_stream) };
            if let Ok(expr) = syn::parse2::<Expr>(type_expr_attempt) {
                if let Expr::Paren(paren_expr) = &expr {
                    if let Expr::Binary(ExprBinary { left, op, right, .. }) = paren_expr.expr.as_ref() {
                        let op_span = utils::get_op_span(op);
                        let trait_name = utils::op_to_trait_spanned(op, op_span);
                        return Some(quote! {
                            #left: std::ops::#trait_name<#right>
                        });
                    }
                    if let Expr::Unary(ExprUnary { op, expr: inner_expr, .. }) = paren_expr.expr.as_ref() {
                        let op_span = utils::get_un_op_span(op);
                        let trait_name = utils::un_op_to_trait_spanned(op, op_span);
                        return Some(quote! {
                            #inner_expr: std::ops::#trait_name
                        });
                    }
                }
            }
        }
    }
    
    None
}

/// Try to parse a binary expression that may contain macro invocations (like output!).
/// This handles cases like `output!(T + U) + V` where the left side is a macro invocation.
fn try_parse_binary_with_macros(tokens: proc_macro2::TokenStream) -> Option<proc_macro2::TokenStream> {
    use proc_macro2::{TokenTree, Delimiter};
    
    // Convert to vec so we can index into it
    let token_vec: Vec<TokenTree> = tokens.into_iter().collect();
    let mut i = 0;
    let mut paren_depth = 0;
    let mut bracket_depth = 0;
    let mut angle_depth = 0;
    
    // Find the binary operator at the top level
    while i < token_vec.len() {
        match &token_vec[i] {
            TokenTree::Group(group) => {
                match group.delimiter() {
                    Delimiter::Parenthesis => paren_depth += 1,
                    Delimiter::Bracket => bracket_depth += 1,
                    _ => {}
                }
                i += 1;
            }
            TokenTree::Punct(punct) => {
                let ch = punct.as_char();
                
                // Track depth for angle brackets
                if ch == '<' {
                    angle_depth += 1;
                } else if ch == '>' {
                    angle_depth -= 1;
                }
                
                // Check if this is a binary operator at the top level
                if paren_depth == 0 && bracket_depth == 0 && angle_depth == 0 {
                    // Check for binary operators
                    let op_char = match ch {
                        '+' => Some(('+', false)),
                        '-' => Some(('-', false)),
                        '*' => Some(('*', false)),
                        '/' => Some(('/', false)),
                        '%' => Some(('%', false)),
                        '&' => Some(('&', false)),
                        '|' => Some(('|', false)),
                        '^' => Some(('^', false)),
                        '<' => {
                            // Check if this is <<
                            if i + 1 < token_vec.len() {
                                if let TokenTree::Punct(next) = &token_vec[i + 1] {
                                    if next.as_char() == '<' {
                                        Some(('<', true))
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        }
                        '>' => {
                            // Check if this is >>
                            if i + 1 < token_vec.len() {
                                if let TokenTree::Punct(next) = &token_vec[i + 1] {
                                    if next.as_char() == '>' {
                                        Some(('>', true))
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        }
                        _ => None,
                    };
                    
                    if let Some((op_ch, consume_next)) = op_char {
                        // Split into left and right
                        let left_tokens: proc_macro2::TokenStream = token_vec[..i].iter().cloned().collect();
                        let skip = if consume_next { 2 } else { 1 };
                        let right_tokens: proc_macro2::TokenStream = token_vec[i + skip..].iter().cloned().collect();
                        
                        // Parse left and right as expressions to preserve macro invocations
                        let left_expr: Expr = syn::parse2(left_tokens).ok()?;
                        let right_expr: Expr = syn::parse2(right_tokens).ok()?;
                        
                        // Create a dummy binary expression to get the operator type
                        let dummy_bin = quote! { #left_expr #op_ch #right_expr };
                        if let Ok(Expr::Binary(ExprBinary { op, .. })) = syn::parse2::<Expr>(dummy_bin) {
                            let op_span = utils::get_op_span(&op);
                            let trait_name = utils::op_to_trait_spanned(&op, op_span);
                            
                            return Some(quote! {
                                #left_expr: std::ops::#trait_name<#right_expr>
                            });
                        }
                    }
                }
                
                i += 1;
            }
            _ => {
                i += 1;
            }
        }
    }
    
    None
}

/// Try to parse a unary expression that may contain macro invocations (like output!).
/// This handles cases like `!output!(T + U)` or `-output!(T + U)` where the unary operator is applied to a macro invocation.
fn try_parse_unary_with_macros(tokens: proc_macro2::TokenStream) -> Option<proc_macro2::TokenStream> {
    use proc_macro2::TokenTree;
    
    // Convert to vec so we can index into it
    let token_vec: Vec<TokenTree> = tokens.into_iter().collect();
    
    // Check if the first token is a unary operator
    if token_vec.is_empty() {
        return None;
    }
    
    // Check for unary operators at the start (! or -)
    if let TokenTree::Punct(punct) = &token_vec[0] {
        let op_char = punct.as_char();
        if op_char == '!' || op_char == '-' {
            // Parse the rest as an expression
            let rest_tokens: proc_macro2::TokenStream = token_vec[1..].iter().cloned().collect();
            if let Ok(inner_expr) = syn::parse2::<Expr>(rest_tokens) {
                // Create a dummy unary expression to get the operator type
                let dummy_unary = if op_char == '!' {
                    quote! { !#inner_expr }
                } else {
                    quote! { -#inner_expr }
                };
                if let Ok(Expr::Unary(ExprUnary { op, .. })) = syn::parse2::<Expr>(dummy_unary) {
                    let op_span = utils::get_un_op_span(&op);
                    let trait_name = utils::un_op_to_trait_spanned(&op, op_span);
                    
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

