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
//! All binary and unary operators from `core::ops` that have an associated `Output` type:
//! - `+` → [`core::ops::Add`](https://doc.rust-lang.org/core/ops/trait.Add.html)
//! - `-` → [`core::ops::Sub`](https://doc.rust-lang.org/core/ops/trait.Sub.html)
//! - `*` → [`core::ops::Mul`](https://doc.rust-lang.org/core/ops/trait.Mul.html)
//! - `/` → [`core::ops::Div`](https://doc.rust-lang.org/core/ops/trait.Div.html)
//! - `%` → [`core::ops::Rem`](https://doc.rust-lang.org/core/ops/trait.Rem.html)
//! - `&` → [`core::ops::BitAnd`](https://doc.rust-lang.org/core/ops/trait.BitAnd.html)
//! - `|` → [`core::ops::BitOr`](https://doc.rust-lang.org/core/ops/trait.BitOr.html)
//! - `^` → [`core::ops::BitXor`](https://doc.rust-lang.org/core/ops/trait.BitXor.html)
//! - `<<` → [`core::ops::Shl`](https://doc.rust-lang.org/core/ops/trait.Shl.html)
//! - `>>` → [`core::ops::Shr`](https://doc.rust-lang.org/core/ops/trait.Shr.html)
//! - `!` → [`core::ops::Not`](https://doc.rust-lang.org/core/ops/trait.Not.html) (unary operator)
//! - `-` → [`core::ops::Neg`](https://doc.rust-lang.org/core/ops/trait.Neg.html) (unary operator)

mod op_result;
mod output;
mod utils;

use proc_macro::TokenStream;

/// Expands operator expressions to associated type outputs.
///
/// Transforms `output!(T + U)` into `<T as core::ops::Add<U>>::Output`. Works recursively
/// for nested expressions, preserving parentheses for operator precedence.
///
/// ## Syntax
///
/// ```rust,ignore
/// output!(<expr>)
/// ```
///
/// where `<expr>` is any valid operator output expression.  An "operator output expression" is:
///  - A unary operator expression, e.g. `!T` or `-T` (equivalent to `<T as core::ops::Not>::Output` or `<T as core::ops::Neg>::Output`)
///  - A binary operator expression, e.g. `T + U` (equivalent to `<T as core::ops::Add<U>>::Output`)
///  - A combination thereof, e.g. `(T + U) * V` or `(T + U) * (V + W)`
///    (equivalent to `<T as core::ops::Add<U, Output: Mul<V>>>::Output` or `<T as core::ops::Add<U, Output: Mul<V, Output: Mul<W>>>>::Output`)
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
/// #[op_result(marker_trait_syntax)]
/// #[op_result(marker_trait_syntax, TraitName)]
/// #[op_result(well_formedness_syntax)]
/// #[op_result(any_syntax)]
/// #[op_result(any_syntax, TraitName)]
/// fn <fn_name>()
/// where
///     [(); <expr>]:, // "well-formedness" syntax
///     // or, equivalently,
///     (): IsDefined<{ <expr> }>, // "marker trait" syntax (default trait name)
///     // or, with custom trait name,
///     (): TraitName<{ <expr> }>, // "marker trait" syntax (custom trait name)
/// {
/// }
/// ```
///
/// By default, both syntaxes are supported. However, the well-formedness syntax `[(); <expr>]:` may
/// conflict with const generic syntax in some cases. You can disable well-formedness syntax parsing
/// by using `#[op_result(marker_trait_syntax)]`, which will only process `(): IsDefined<{ <expr> }>` syntax.
/// Alternatively, you can disable marker trait syntax parsing by using `#[op_result(well_formedness_syntax)]`,
/// which will only process `[(); <expr>]:` syntax. The `IsDefined<>` syntax is uniquely detectable
/// and will not conflict with const generics.
///
/// You can also use `#[op_result(any_syntax)]` to explicitly enable both syntaxes (same as default).
///
/// An optional second parameter can be provided to specify a custom marker trait name (defaults to `IsDefined`).
/// This parameter is only valid with `marker_trait_syntax` or `any_syntax`. For example:
/// - `#[op_result(marker_trait_syntax, MyTrait)]` - only processes `(): MyTrait<{ <expr> }>` syntax
/// - `#[op_result(any_syntax, MyTrait)]` - processes both `[(); <expr>]:` and `(): MyTrait<{ <expr> }>` syntax
///
/// where `<expr>` is any valid operator bound expression. An "operator bound expression" is:
///  - A unary operator expression, e.g. `!T` or `-T` (equivalent to `T: Not` or `T: Neg`)
///  - A binary operator expression, e.g. `T + U` (equivalent to `T: Add<U>`)
///  - Any combination thereof, e.g. `(T + U) * V` or `(T + U) * (V + W)`
///    (equivalent to `T: Add<U, Output: Mul<V>>` or `T: Add<U, Output: Mul<V, Output: Mul<W>>>`)
///  - An assignment of any of the above to a type, e.g. `T + U = V` (equivalent to `T: Add<U, Output = V>`)
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
/// fn example_sub<T, U>(a: T, b: U) -> output!(T - U - U)
/// where
///     [(); T - U - U]:,
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
///
/// ## Syntax Selection
///
/// If you need to use bracket syntax for const generics, you can disable the well-formedness syntax
/// parsing while still using `IsDefined<>` syntax:
///
/// ```rust
/// # use op_result::op_result;
/// # use op_result::output;
/// #[op_result(marker_trait_syntax)]
/// fn example<T, U>(a: T, b: U) -> output!(T + U)
/// where
///     (): IsDefined<{ T + U }>, // Only this syntax is processed
///     // [(); T + U]:, // This would NOT be transformed if uncommented
/// {
///     a + b
/// }
///
/// let result = example(1, 2);
/// assert_eq!(result, 3);
/// ```
///
/// Alternatively, you can disable marker trait syntax parsing to only use well-formedness syntax:
///
/// ```rust
/// # use op_result::op_result;
/// # use op_result::output;
/// #[op_result(well_formedness_syntax)]
/// fn example<T, U>(a: T, b: U) -> output!(T + U)
/// where
///     [(); T + U]:, // Only this syntax is processed
///     // (): IsDefined<{ T + U }>, // This would NOT be transformed if uncommented
/// {
///     a + b
/// }
///
/// let result = example(1, 2);
/// assert_eq!(result, 3);
/// ```
#[proc_macro_attribute]
pub fn op_result(attr: TokenStream, item: TokenStream) -> TokenStream {
    op_result::expand_op_result(attr, item)
}
