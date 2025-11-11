use proc_macro2::Span;
use quote::quote_spanned;
use syn::{BinOp, UnOp};

pub fn get_op_span(op: &BinOp) -> Span {
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

pub fn op_to_trait_spanned(op: &BinOp, span: Span) -> proc_macro2::TokenStream {
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
            syn::Error::new_spanned(op, "This operator does not have an associated Output type in core::ops")
                .to_compile_error()
        }
        _ => {
            syn::Error::new_spanned(op, "Unsupported operator")
                .to_compile_error()
        }
    }
}

pub fn get_un_op_span(op: &UnOp) -> Span {
    use syn::UnOp::*;
    match op {
        Not(token) => token.span,
        Deref(token) => token.span,
        Neg(token) => token.span,
        _ => Span::call_site(),
    }
}

pub fn un_op_to_trait_spanned(op: &UnOp, span: Span) -> proc_macro2::TokenStream {
    use syn::UnOp::*;
    match op {
        Not(_) => quote_spanned! { span => Not },
        Neg(_) => quote_spanned! { span => Neg },
        Deref(_) => {
            syn::Error::new_spanned(op, "The deref operator (*) does not have an associated Output type in core::ops")
                .to_compile_error()
        }
        _ => {
            syn::Error::new_spanned(op, "Unsupported unary operator")
                .to_compile_error()
        }
    }
}

