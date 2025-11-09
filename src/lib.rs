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
use quote::quote;
use syn::{parse_macro_input, Expr, ExprBinary, BinOp};

/// Documentation shadows for IDE hover polish.
/// 
/// These map operator symbols to their corresponding `std::ops` traits.
/// While not technically used in the macro expansion, they serve as documentation
/// to help IDEs understand the relationship between operators and traits.
#[doc(hidden)]
#[allow(unused_imports)]
mod __doc_shadows {
    /// `+` operator maps to [`std::ops::Add`]
    pub use std::ops::Add;
    
    /// `-` operator maps to [`std::ops::Sub`]
    pub use std::ops::Sub;
    
    /// `*` operator maps to [`std::ops::Mul`]
    pub use std::ops::Mul;
    
    /// `/` operator maps to [`std::ops::Div`]
    pub use std::ops::Div;
    
    /// `%` operator maps to [`std::ops::Rem`]
    pub use std::ops::Rem;
    
    /// `&` operator maps to [`std::ops::BitAnd`]
    pub use std::ops::BitAnd;
    
    /// `|` operator maps to [`std::ops::BitOr`]
    pub use std::ops::BitOr;
    
    /// `^` operator maps to [`std::ops::BitXor`]
    pub use std::ops::BitXor;
    
    /// `<<` operator maps to [`std::ops::Shl`]
    pub use std::ops::Shl;
    
    /// `>>` operator maps to [`std::ops::Shr`]
    pub use std::ops::Shr;
}

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
    match expr {
        Expr::Binary(ExprBinary { left, op, right, .. }) => {
            let left_expanded = expand_expr(left);
            let right_expanded = expand_expr(right);
            let trait_name = op_to_trait(op);
            
            quote! {
                <#left_expanded as std::ops::#trait_name<#right_expanded>>::Output
            }
        }
        Expr::Paren(expr_paren) => {
            let inner = expand_expr(&expr_paren.expr);
            quote! { (#inner) }
        }
        _ => {
            quote! { #expr }
        }
    }
}

fn op_to_trait(op: &BinOp) -> proc_macro2::TokenStream {
    use syn::BinOp::*;
    match op {
        Add(_) => quote! { Add },
        Sub(_) => quote! { Sub },
        Mul(_) => quote! { Mul },
        Div(_) => quote! { Div },
        Rem(_) => quote! { Rem },
        BitAnd(_) => quote! { BitAnd },
        BitOr(_) => quote! { BitOr },
        BitXor(_) => quote! { BitXor },
        Shl(_) => quote! { Shl },
        Shr(_) => quote! { Shr },
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
