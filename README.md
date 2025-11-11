# op_result

Syntactic sugar for writing associated type algebra with operator expressions.

Provides two macros:

- `output!`: Expands operator expressions to associated type outputs
- `#[op_result]`: Transforms operator expressions in where clauses into trait bounds. Supports two syntaxes: `(): IsDefined<{ ... }>` and `[(); ...]:`

## Example

```rust
use op_result::op_result;
use op_result::output;

#[op_result]
fn add<T, U>(a: T, b: U) -> output!(T + U)
where
    [(); T + U]:,
{
    a + b
}

let result = add(1, 2);
assert_eq!(result, 3);
```

## Usage

### `output!` macro

Transforms `output!(T + U)` into `<T as core::ops::Add<U>>::Output`. Works recursively for nested expressions, preserving parentheses for operator precedence.

```rust
use op_result::op_result;
use op_result::output;

#[op_result]
fn add<T, U>(a: T, b: U) -> output!(T + U)
where
    [(); T + U]:,
{
    a + b
}

let result = add(1, 2);
```

### `#[op_result]` attribute macro

Transforms operator expressions in where clauses into trait bounds.

```rust
use op_result::op_result;
use op_result::output;

#[op_result]
fn example_sub<T, U>(a: T, b: U) -> output!(T - U - U)
where
    [(); T - U - U]:,
    U: Copy,
{
    (a - b) - b
}
```

The output type can be assigned using the `=` operator:

```rust
use op_result::op_result;

#[op_result]
fn example_output_assignment<T, U, V>(a: T, b: U) -> V
where
    [(); T + U = V]:,
{
    a + b
}
```

Both `[(); ...]:` and `(): IsDefined<{ ... }>` syntaxes are supported.

## Supported Operators

All binary and unary operators from `core::ops` that have an associated `Output` type:
- `+` → [`core::ops::Add`]
- `-` → [`core::ops::Sub`]
- `*` → [`core::ops::Mul`]
- `/` → [`core::ops::Div`]
- `%` → [`core::ops::Rem`]
- `&` → [`core::ops::BitAnd`]
- `|` → [`core::ops::BitOr`]
- `^` → [`core::ops::BitXor`]
- `<<` → [`core::ops::Shl`]
- `>>` → [`core::ops::Shr`]
- `!` → [`core::ops::Not`] (unary operator)
- `-` → [`core::ops::Neg`] (unary operator)

## License

Licensed under either of Apache License, Version 2.0 or MIT license at your option.
