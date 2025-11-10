# op_result

Syntactic sugar for writing associated type algebra. Provides two macros:

- `output!`: Transforms `output!(T + U)` into `<T as std::ops::Add<U>>::Output`. Works recursively for nested operations.
- `#[op_result]`: Attribute macro that transforms `(): IsDefined<{ ... }>` patterns in where clauses into trait bounds like `T: std::ops::Add<U>`.

## Usage

The `output!` macro expands operator expressions to associated type outputs. For example:

```rust
use op_result::output;

type Sum = output!(i32 + i64);
// Expands to: <i32 as std::ops::Add<i64>>::Output

type Complex = output!((i32 + i64) * f32);
// Expands to: <<i32 as std::ops::Add<i64>>::Output as std::ops::Mul<f32>>::Output
```

## The `#[op_result]` attribute macro

The `#[op_result]` attribute macro transforms `(): IsDefined<{ ... }>` patterns in where clauses into trait bounds:

```rust
use op_result::op_result;

#[op_result]
fn compute_sum<T, U>(a: T, b: U) -> T
where
    (): IsDefined<{ T + U }>,  // Expands to: T: std::ops::Add<U>
{
    a + b
}
```

### Using `output!` with `#[op_result]`

The `output!` macro can be used inside `IsDefined` expressions to specify bounds on operation results:

```rust
use op_result::op_result;
use op_result::output;

#[op_result]
fn compute_nested<T, U, V>(a: T, b: U, c: V) -> output!(T + U + V)
where
    (): IsDefined<{ T + U }>,                    // T: std::ops::Add<U>
    (): IsDefined<{ output!(T + U) + V }>,      // <T as std::ops::Add<U>>::Output: std::ops::Add<V>
{
    a + b + c
}
```

### How it works

Normal macros cannot be used in where clauses. The `#[op_result]` attribute macro processes the entire function and transforms `IsDefined` patterns before the compiler sees them, enabling operator syntax in trait bounds.

## Examples

Using `output!` to define type aliases for operation results:

```rust
pub struct PIDController<
    PV: ProcessVariable,
    CO: ControlOutput,
    T: Time,
    ProportionalGain = output!(CO / PV),
    IntegralGain = output!(CO / (PV * T)),
    DerivativeGain = output!((CO * T) / PV),
> {
    // ...
}
```

Using `#[op_result]` to specify trait bounds in where clauses:

```rust
use op_result::op_result;
use op_result::output;

#[op_result]
fn process_data<T, U, V>(data: T, scale: U, offset: V) -> output!((T * U) + V)
where
    (): IsDefined<{ T * U }>,
    (): IsDefined<{ output!(T * U) + V }>,
{
    (data * scale) + offset
}
```

## Supported operators

All `std::ops` binary and unary operators with associated `Output` types:

### Binary operators

- `+` → `Add`
- `-` → `Sub`
- `*` → `Mul`
- `/` → `Div`
- `%` → `Rem`
- `&` → `BitAnd`
- `|` → `BitOr`
- `^` → `BitXor`
- `<<` → `Shl`
- `>>` → `Shr`

### Unary operators

- `!` → `Not`
- `-` → `Neg` (unary negation)

Both macros support all of these operators. Comparison and logical operators (`==`, `&&`, `||`, etc.) are not supported as they don't have associated `Output` types in `std::ops`.

## License

Licensed under either of Apache License, Version 2.0 or MIT license at your option.

