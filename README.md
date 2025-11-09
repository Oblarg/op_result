# op_result

Thin syntactic sugar for writing associated type algebra. Transforms `output!(T + U)` into `<T as std::ops::Add<U>>::Output`, and so on recursively for nested operations.

## What it does

Instead of writing verbose associated type syntax like:

```rust
fn compute_sum<T, U>() 
where
    T: std::ops::Add<U>,
{
    type Sum = <T as std::ops::Add<U>>::Output;
    // ...
}

fn compute_complex<T, U, V>()
where
    T: std::ops::Add<U>,
    <T as std::ops::Add<U>>::Output: std::ops::Mul<V>,
{
    type Complex = <<T as std::ops::Add<U>>::Output as std::ops::Mul<V>>::Output;
    // ...
}
```

You can write:

```rust
use op_result::output;

fn compute_sum<T, U>() 
where
    T: std::ops::Add<U>,
{
    type Sum = output!(T + U);
    // ...
}

fn compute_complex<T, U, V>()
where
    T: std::ops::Add<U>,
    output!(T + U): std::ops::Mul<V>,
{
    type Complex = output!((T + U) * V);
    // ...
}
```

## When to use it

Useful when you need to refer to the `Output` associated type of operator traits without performing the operation. For example, in dimensional analysis:

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

## Supported operators

All `std::ops` binary operators with associated `Output` types:

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

## License

Licensed under either of Apache License, Version 2.0 or MIT license at your option.

