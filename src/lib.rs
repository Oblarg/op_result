//! Syntactic sugar for writing associated type algebra with operator expressions.
//!
//! Provides two macros:
//!
//! - `output!`: Expands operator expressions to associated type outputs
//! - `#[op_result]`: Transforms operator expressions in where clauses into trait bounds. Supports two syntaxes: `(): IsDefined<{ ... }>` and `[(); ...]:`
//!
//! ## Example
//!
//! ```rust
//! use op_result::op_result;
//! use op_result::output;
//!
//! #[op_result]
//! fn add<T, U>(a: T, b: U) -> output!(T + U)
//! where
//!     [(); T + U]:,
//! {
//!     a + b
//! }
//!
//! let result = add(1, 2);
//! assert_eq!(result, 3);
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
/// use op_result::op_result;
/// use op_result::output;
///
/// #[op_result]
/// fn add<T, U>(a: T, b: U) -> output!(T + U)
/// where
///     [(); T + U]:,
/// {
///     a + b
/// }
///
/// // Type inference works automatically
/// let result = add(1, 2);
/// ```
#[proc_macro]
pub fn output(input: TokenStream) -> TokenStream {
    output::expand_output(input)
}

/// Transforms operator expressions in where clauses into trait bounds.
///
/// The attribute macro processes the entire function and transforms operator expressions
/// into trait bounds.
/// 
/// ## Syntax
/// 
/// ```rust,ignore
/// #[op_result]
/// fn <fn_name>()
/// where
///     [(); <expr>]:, // "well-formedness" syntax
///     // or, equivalently,
///     (): IsDefined<{ <expr> }>, // "marker trait" syntax
/// {
/// }
/// ```
/// 
/// where `<expr>` is any valid operator definition expression. An "operator definition expression" is:
///  - A binary operator expression, e.g. `T + U`
///  - A unary operator expression, e.g. `!T` or `-T`
/// 
/// To assert the definition of a nested operator expression, use the `output!` macro inside the expression.
///
/// ## Examples
///
/// ```rust
/// # use op_result::op_result;
/// # use op_result::output;
/// #[op_result]
/// fn example_add<T, U>(a: T, b: U) -> output!(T + U)
/// where
///     [(); T + U]:,
/// {
///     a + b
/// }
///
/// let result = example_add(1, 2);
/// assert_eq!(result, 3);
/// ```
///
/// ```rust
/// # use op_result::op_result;
/// # use op_result::output;
/// #[op_result]
/// fn example_sub<T, U>(a: T, b: U) -> output!(output!(T - U) - U)
/// where
///     [(); T - U]:,
///     [(); output!(T - U) - U]:,
///     U: Copy,
/// {
///     (a - b) - b
/// }
///
/// let result = example_sub(10, 2);
/// assert_eq!(result, 6);
/// ```
///
/// The output type can be assigned using the `=` operator:
///
/// ```rust
/// # use op_result::op_result;
/// #[op_result]
/// fn example_output_assignment<T, U, V>(a: T, b: U) -> V
/// where
///     [(); T + U = V]:,
/// {
///     a + b
/// }
///
/// let result: i32 = example_output_assignment(1, 2);
/// assert_eq!(result, 3);
/// ```
#[proc_macro_attribute]
pub fn op_result(attr: TokenStream, item: TokenStream) -> TokenStream {
    op_result::expand_op_result(attr, item)
}
