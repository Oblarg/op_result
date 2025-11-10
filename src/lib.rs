//! Syntactic sugar for writing associated type algebra with operator expressions.
//!
//! Provides two macros:
//!
//! - `output!`: Expands operator expressions to associated type outputs
//! - `#[op_result]`: Transforms `(): IsDefined<{ ... }>` patterns in where clauses into trait bounds
//!
//! ## Example
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
//! 
//! ## Supported Operators
//!
//! All binary and unary operators from `std::ops` that have an associated `Output` type:
//! - `+` → [`std::ops::Add`]
//! - `-` → [`std::ops::Sub`]
//! - `*` → [`std::ops::Mul`]
//! - `/` → [`std::ops::Div`]
//! - `%` → [`std::ops::Rem`]
//! - `&` → [`std::ops::BitAnd`]
//! - `|` → [`std::ops::BitOr`]
//! - `^` → [`std::ops::BitXor`]
//! - `<<` → [`std::ops::Shl`]
//! - `>>` → [`std::ops::Shr`]
//! - `!` → [`std::ops::Not`] (unary operator)
//! - `-` → [`std::ops::Neg`] (unary operator)

mod utils;
mod output;
mod op_result;

use proc_macro::TokenStream;

/// Expands operator expressions to associated type outputs.
///
/// Transforms `output!(T + U)` into `<T as std::ops::Add<U>>::Output`. Works recursively
/// for nested expressions, preserving parentheses for operator precedence.
/// 
/// ## Syntax
/// 
/// ```rust,ignore
/// output!(<expr>)
/// ```
/// 
/// where `<expr>` is any valid operator output expression.  An "operator output expression" is:
///  - A binary operator expression, e.g. `T + U`
///  - A unary operator expression, e.g. `!T` or `-T`
///  - A parenthesized operator expression, e.g. `(T + U)`
///  - A combination thereof, e.g. `(T + U) * V` or `(T + U) * (V + W)`
///
/// ## Examples
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
    output::expand_output(input)
}

/// Transforms `(): IsDefined<{ ... }>` patterns in where clauses into trait bounds.
///
/// The attribute macro processes the entire function and transforms `(): IsDefined<{ T + U }>`
/// into `T: std::ops::Add<U>`, `(): IsDefined<{ T - U }>` into `T: std::ops::Sub<U>`, etc.
/// 
/// ## Syntax
/// 
/// ```rust,ignore
/// #[op_result]
/// fn <fn_name>()
/// where
///     (): IsDefined<{ <expr> }>,
/// {
/// }
/// ```
/// 
/// where `<expr>` is any valid operator definition expression. An "operator definition expression" is:
///  - A binary operator expression, e.g. `T + U`
///  - A unary operator expression, e.g. `!T` or `-T`
/// 
/// To assert the definition of a nested operator expression, use the `output!` macro inside the `IsDefined` expression.
///
/// ## Examples
///
/// ```rust
/// use op_result::op_result;
///
/// #[op_result]
/// fn example_add<T, U>()
/// where
///     (): IsDefined<{ T + U }>,
/// {
/// }
/// ```
///
/// ```rust
/// use op_result::op_result;
/// use op_result::output;
///
/// #[op_result]
/// fn example_sub<T, U>()
/// where
///     (): IsDefined<{ T - U }>,
///     (): IsDefined<{ output!(T - U) - U }>,
/// {
/// }
/// ```
#[proc_macro_attribute]
pub fn op_result(attr: TokenStream, item: TokenStream) -> TokenStream {
    op_result::expand_op_result(attr, item)
}
