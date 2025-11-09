//! A macro for writing associated type algebra with ordinary std op expressions.
//!
//! # Example
//!
//! ```rust
//! use op_result::output;
//!
//! // `output!(T + U)` expands to `<T as Add<U>>::Output`
//! type Sum = output!(i32 + i64);
//!
//! // Works recursively
//! type Complex = output!((i32 + i64) * f32);
//! ```

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, quote_spanned};
use syn::{parse_macro_input, Expr, ExprBinary, BinOp};


/// Expands operator expressions to associated type outputs.
///
/// This macro provides syntactic sugar for accessing the `Output` associated type
/// of `std::ops` traits. Instead of writing verbose associated type syntax like
/// `<T as std::ops::Add<U>>::Output`, you can write `output!(T + U)`.
///
/// The macro recursively processes nested expressions, preserving parentheses for
/// operator precedence. Each binary operator is transformed into its corresponding
/// `std::ops` trait's `Output` type.
///
/// # Supported Operators
///
/// All binary operators from `std::ops` that have an associated `Output` type:
///
/// - `+` → [`std::ops::Add`]
/// - `-` → [`std::ops::Sub`]
/// - `*` → [`std::ops::Mul`]
/// - `/` → [`std::ops::Div`]
/// - `%` → [`std::ops::Rem`]
/// - `&` → [`std::ops::BitAnd`]
/// - `|` → [`std::ops::BitOr`]
/// - `^` → [`std::ops::BitXor`]
/// - `<<` → [`std::ops::Shl`]
/// - `>>` → [`std::ops::Shr`]
///
/// Operators without associated `Output` types (comparison and logical operators)
/// are not supported and will result in a compile-time error.
///
/// # Examples
///
/// ```rust
/// use op_result::output;
///
/// // Basic operation
/// type Sum = output!(i32 + i64);
/// // Expands to: <i32 as std::ops::Add<i64>>::Output
///
/// // Nested operations with parentheses
/// type Complex = output!((i32 + i64) * f32);
/// // Expands to: <<i32 as std::ops::Add<i64>>::Output as std::ops::Mul<f32>>::Output
/// ```
#[proc_macro]
pub fn output(input: TokenStream) -> TokenStream {
    let expr = parse_macro_input!(input as Expr);
    let output = expand_expr(&expr);
    TokenStream::from(output)
}

fn expand_expr(expr: &Expr) -> proc_macro2::TokenStream {
    expand_expr_with_doc(expr, None)
}

fn expand_expr_with_doc(expr: &Expr, _parent_span: Option<Span>) -> proc_macro2::TokenStream {
    match expr {
        Expr::Binary(ExprBinary { left, op, right, .. }) => {
            let left_expanded = expand_expr_with_doc(left, None);
            let right_expanded = expand_expr_with_doc(right, None);
            let op_span = get_op_span(op);
            let trait_name = op_to_trait_spanned(op, op_span);
            
            // Only apply span to the trait name, not the entire expression
            // This prevents the 'as' keyword from inheriting the operator's span
            quote! {
                <#left_expanded as std::ops::#trait_name<#right_expanded>>::Output
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


fn get_op_span(op: &BinOp) -> Span {
    use syn::BinOp::*;
    match op {
        Add(token) => token.span,
        Sub(token) => token.span,
        Mul(token) => token.span,
        Div(token) => token.span,
        Rem(token) => token.span,
        BitAnd(token) => token.span,
        BitOr(token) => token.span,
        BitXor(token) => token.span,
        Shl(token) => token.spans[0],
        Shr(token) => token.spans[0],
        And(token) => token.spans[0],
        Or(token) => token.spans[0],
        Eq(token) => token.spans[0],
        Lt(token) => token.span,
        Le(token) => token.spans[0],
        Ne(token) => token.spans[0],
        Ge(token) => token.spans[0],
        Gt(token) => token.span,
        _ => Span::call_site(),
    }
}

fn op_to_trait_spanned(op: &BinOp, span: Span) -> proc_macro2::TokenStream {
    use syn::BinOp::*;
    match op {
        Add(_) => quote_spanned! { span => Add },
        Sub(_) => quote_spanned! { span => Sub },
        Mul(_) => quote_spanned! { span => Mul },
        Div(_) => quote_spanned! { span => Div },
        Rem(_) => quote_spanned! { span => Rem },
        BitAnd(_) => quote_spanned! { span => BitAnd },
        BitOr(_) => quote_spanned! { span => BitOr },
        BitXor(_) => quote_spanned! { span => BitXor },
        Shl(_) => quote_spanned! { span => Shl },
        Shr(_) => quote_spanned! { span => Shr },
        And(_) | Or(_) | Eq(_) | Lt(_) | Le(_) | Ne(_) | Ge(_) | Gt(_) => {
            syn::Error::new_spanned(op, "This operator does not have an associated Output type in std::ops")
                .to_compile_error()
        }
        _ => {
            syn::Error::new_spanned(op, "Unsupported operator")
                .to_compile_error()
        }
    }
}
