use op_result::op_result;
use op_result::output;

#[test]
fn test_output_addition() {
    #[op_result]
    fn test<T, U, V>(a: T, b: U, c: V) -> output!(T + U + V)
    where
        (): IsDefined<{ T + U + V }>,
    {
        a + b + c
    }

    assert_eq!(test(1, 2, 3), 6);
}

#[test]
fn test_output_subtraction() {
    #[op_result]
    fn test<T, U, V>(a: T, b: U, c: V) -> output!(T - U - V)
    where
        (): IsDefined<{ T - U - V }>,
    {
        a - b - c
    }
    assert_eq!(test(1, 2, 3), -4);
}

#[test]
fn test_bracket_notation_addition() {
    #[op_result]
    fn test<T, U, V>(a: T, b: U, c: V) -> output!(T + U + V)
    where
        [(); T + U + V]:,
    {
        a + b + c
    }

    assert_eq!(test(1, 2, 3), 6);
}

#[test]
fn test_bracket_notation_subtraction() {
    #[op_result]
    fn test<T, U, V>(a: T, b: U, c: V) -> output!(T - U - V)
    where
        [(); T - U - V]:,
    {
        a - b - c
    }
    assert_eq!(test(1, 2, 3), -4);
}

#[test]
fn test_mixed_notations() {
    #[op_result]
    fn test<T, U, V>(a: T, b: U, c: V) -> output!(T + U + V)
    where
        (): IsDefined<{ T + U + V }>,
    {
        a + b + c
    }

    assert_eq!(test(1, 2, 3), 6);
}

#[test]
fn test_bracket_notation_with_equals() {
    #[op_result]
    fn test<T, U, V>(a: T, b: U) -> V
    where
        [(); T + U = V]:,
    {
        a + b
    }

    assert_eq!(test(1, 2), 3);
}

#[test]
fn test_chained_operator_bounds() {
    // Test that T + U + V expands to T: Add<U, Output: Add<V>>
    // output! already supports chaining, so output!(T + U + V) works
    #[op_result]
    fn compute_nested<T, U, V>(a: T, b: U, c: V) -> output!(T + U + V)
    where
        [(); T + U + V]:,
    {
        a + b + c
    }

    assert_eq!(compute_nested(1, 2, 3), 6);
}

#[test]
fn test_chained_operator_bounds_three_ops() {
    // Test that T + U + V + W expands to T: Add<U, Output: Add<V, Output: Add<W>>>
    #[op_result]
    fn compute_nested_four<T, U, V, W>(a: T, b: U, c: V, d: W) -> output!(T + U + V + W)
    where
        [(); T + U + V + W]:,
    {
        ((a + b) + c) + d
    }

    assert_eq!(compute_nested_four(1, 2, 3, 4), 10);
}

#[test]
fn test_chained_operator_bounds_with_precedence() {
    // Test that operator precedence is respected: T + U * V should not chain
    // syn parses this as T + (U * V), so we need separate bounds
    #[op_result]
    fn test_precedence<T, U, V>(a: T, b: U, c: V) -> output!(T + output!(U * V))
    where
        [(); T + output!(U * V)]:,
        [(); U * V]:,
    {
        a + (b * c)
    }

    assert_eq!(test_precedence(1, 2, 3), 7);
}

#[test]
fn test_chained_operator_bounds_with_output_assertion() {
    // Test that T + U + V = W expands to T: Add<U, Output: Add<V, Output = W>>
    #[op_result]
    fn compute_nested_with_output<T, U, V, W>(a: T, b: U, c: V) -> W
    where
        [(); T + U + V = W]:,
    {
        a + b + c
    }

    let result: i32 = compute_nested_with_output(1, 2, 3);
    assert_eq!(result, 6);
}

#[test]
fn test_chained_operator_bounds_three_ops_with_output() {
    // Test that T + U + V + W = X expands to T: Add<U, Output: Add<V, Output: Add<W, Output = X>>>
    #[op_result]
    fn compute_nested_four_with_output<T, U, V, W, X>(a: T, b: U, c: V, d: W) -> X
    where
        [(); T + U + V + W = X]:,
    {
        ((a + b) + c) + d
    }

    let result: i32 = compute_nested_four_with_output(1, 2, 3, 4);
    assert_eq!(result, 10);
}

#[test]
fn test_marker_trait_syntax_flag() {
    // Test that marker_trait_syntax disables well-formedness syntax but keeps IsDefined syntax
    #[op_result(marker_trait_syntax)]
    fn test<T, U>(a: T, b: U) -> output!(T + U)
    where
        (): IsDefined<{ T + U }>,
    {
        a + b
    }

    assert_eq!(test(1, 2), 3);
}

#[test]
fn test_well_formedness_syntax_flag() {
    // Test that well_formedness_syntax disables marker trait syntax but keeps bracket syntax
    #[op_result(well_formedness_syntax)]
    fn test<T, U>(a: T, b: U) -> output!(T + U)
    where
        [(); T + U]:,
    {
        a + b
    }

    assert_eq!(test(1, 2), 3);
}

#[test]
fn test_any_syntax_flag() {
    // Test that any_syntax enables both syntaxes (same as default)
    #[op_result(any_syntax)]
    fn test<T, U>(a: T, b: U) -> output!(T + U)
    where
        [(); T + U]:,
        (): IsDefined<{ T + U }>,
    {
        a + b
    }

    assert_eq!(test(1, 2), 3);
}

#[test]
fn test_custom_trait_name_with_marker_trait_syntax() {
    #[op_result(marker_trait_syntax, MyTrait)]
    fn test<T, U>(a: T, b: U) -> output!(T + U)
    where
        (): MyTrait<{ T + U }>,
    {
        a + b
    }

    assert_eq!(test(1, 2), 3);
}

#[test]
fn test_custom_trait_name_with_any_syntax() {
    #[op_result(any_syntax, MyCustomTrait)]
    fn test<T, U>(a: T, b: U) -> output!(T + U)
    where
        [(); T + U]:,
        (): MyCustomTrait<{ T + U }>,
    {
        a + b
    }

    assert_eq!(test(1, 2), 3);
}

#[test]
fn test_op_result_chained_mul_div() {
    #[op_result]
    fn test<T, U, V>(a: T, b: U) -> V
    where
        [(); T * U * U / U = V]:,
        U: Copy,
    {
        ((a * b) * b) / b
    }

    assert_eq!(test(1, 2), 2);
}
