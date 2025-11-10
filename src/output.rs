use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, Expr, ExprBinary, ExprUnary};

use crate::utils;

pub fn expand_expr(expr: &Expr) -> proc_macro2::TokenStream {
    expand_expr_with_doc(expr, None)
}

fn expand_expr_with_doc(expr: &Expr, _parent_span: Option<Span>) -> proc_macro2::TokenStream {
    match expr {
        Expr::Binary(ExprBinary { left, op, right, .. }) => {
            let left_expanded = expand_expr_with_doc(left, None);
            let right_expanded = expand_expr_with_doc(right, None);
            let op_span = utils::get_op_span(op);
            let trait_name = utils::op_to_trait_spanned(op, op_span);
            
            // Only apply span to the trait name, not the entire expression
            // This prevents the 'as' keyword from inheriting the operator's span
            quote! {
                <#left_expanded as std::ops::#trait_name<#right_expanded>>::Output
            }
        }
        Expr::Unary(ExprUnary { op, expr: inner_expr, .. }) => {
            let inner_expanded = expand_expr_with_doc(inner_expr, None);
            let op_span = utils::get_un_op_span(op);
            let trait_name = utils::un_op_to_trait_spanned(op, op_span);
            
            quote! {
                <#inner_expanded as std::ops::#trait_name>::Output
            }
        }
        Expr::Paren(expr_paren) => {
            let inner = expand_expr_with_doc(&expr_paren.expr, None);
            quote! { (#inner) }
        }
        _ => {
            quote! { #expr }
        }
    }
}

pub fn expand_output(input: TokenStream) -> TokenStream {
    let expr = parse_macro_input!(input as Expr);
    let output = expand_expr(&expr);
    TokenStream::from(output)
}

