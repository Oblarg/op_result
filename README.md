# op_result

Syntactic sugar for writing associated type algebra with operator expressions.

Provides two macros:

- `output!`: Expands operator expressions to associated type outputs
- `#[op_result]`: Transforms operator expressions in where clauses into trait bounds. Supports two syntaxes: `(): IsDefined<{ ... }>` and `[(); ...]:`

## Example

```rust
use op_result::output;

// `output!(T + U)` expands to `<T as Add<U>>::Output`
type Sum = output!(i32 + i64);

// Works recursively
type Complex = output!((i32 + i64) * f32);
```

## Usage

### `output!` macro

Transforms `output!(T + U)` into `<T as std::ops::Add<U>>::Output`. Works recursively for nested expressions, preserving parentheses for operator precedence.

```rust
use op_result::output;

// Basic operation
type Sum = output!(i32 + i64);
// Expands to: <i32 as std::ops::Add<i64>>::Output

// Nested operations with parentheses
type Complex = output!((i32 + i64) * f32);
// Expands to: <<i32 as std::ops::Add<i64>>::Output as std::ops::Mul<f32>>::Output
```

### `#[op_result]` attribute macro

Transforms operator expressions in where clauses into trait bounds.

```rust
use op_result::op_result;

#[op_result]
fn example_add<T, U>()
where
    [(); T + U]:,
{
}
```

To assert the definition of a nested operator expression, use the `output!` macro inside the expression:

```rust
use op_result::op_result;
use op_result::output;

#[op_result]
fn example_sub<T, U>()
where
    [(); T - U]:,
    [(); output!(T - U) - U]:,
{
}
```

Both `[(); ...]:` and `(): IsDefined<{ ... }>` syntaxes are supported.

## Supported Operators

All binary and unary operators from `std::ops` that have an associated `Output` type:
- `+` → [`std::ops::Add`]
- `-` → [`std::ops::Sub`]
- `*` → [`std::ops::Mul`]
- `/` → [`std::ops::Div`]
- `%` → [`std::ops::Rem`]
- `&` → [`std::ops::BitAnd`]
- `|` → [`std::ops::BitOr`]
- `^` → [`std::ops::BitXor`]
- `<<` → [`std::ops::Shl`]
- `>>` → [`std::ops::Shr`]
- `!` → [`std::ops::Not`] (unary operator)
- `-` → [`std::ops::Neg`] (unary operator)

## License

Licensed under either of Apache License, Version 2.0 or MIT license at your option.
