use op_result::op_result;
use op_result::output;

#[test]
fn test_output_addition() {
    #[op_result]
    fn test<T, U, V>(
        a: T,
        b: U,
        c: V,
    ) -> output!(T + U + V)
    where
        (): IsDefined<{ T + U }>,
        (): IsDefined<{ output!(T + U) + V }>,
    {
        a + b + c
    }

    assert_eq!(test(1, 2, 3), 6);
}

#[test]
fn test_output_subtraction() {
    #[op_result]
    fn test<T, U, V>(
        a: T,
        b: U,
        c: V,
    ) -> output!(T - U - V)
    where
        (): IsDefined<{ T - U }>,
        (): IsDefined<{ output!(T - U) - V }>,
    {
        a - b - c
    }
    assert_eq!(test(1, 2, 3), -4);
}

#[test]
fn test_bracket_notation_addition() {
    #[op_result]
    fn test<T, U, V>(
        a: T,
        b: U,
        c: V,
    ) -> output!(T + U + V)
    where
        [(); T + U]:,
        [(); output!(T + U) + V]:,
    {
        a + b + c
    }

    assert_eq!(test(1, 2, 3), 6);
}

#[test]
fn test_bracket_notation_subtraction() {
    #[op_result]
    fn test<T, U, V>(
        a: T,
        b: U,
        c: V,
    ) -> output!(T - U - V)
    where
        [(); T - U]:,
        [(); output!(T - U) - V]:,
    {
        a - b - c
    }
    assert_eq!(test(1, 2, 3), -4);
}

#[test]
fn test_mixed_notations() {
    #[op_result]
    fn test<T, U, V>(
        a: T,
        b: U,
        c: V,
    ) -> output!(T + U + V)
    where
        (): IsDefined<{ T + U }>,
        [(); output!(T + U) + V]:,
    {
        a + b + c
    }

    assert_eq!(test(1, 2, 3), 6);
}