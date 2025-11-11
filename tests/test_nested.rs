use op_result::op_result;
use op_result::output;

#[test]
fn test_struct_with_where_clause() {
    #[op_result]
    struct TestStruct<T, U>
    where
        [(); T + U]:,
    {
        a: T,
        b: U,
    }

    let _s = TestStruct { a: 1, b: 2 };
}

#[test]
fn test_impl_with_where_clause() {
    struct Wrapper<T>(T);

    #[op_result]
    impl<T> Wrapper<T> {
        fn add<U>(self, other: U) -> output!(T + U)
        where
            [(); T + U]:,
        {
            self.0 + other
        }
    }

    let w = Wrapper(1);
    assert_eq!(w.add(2), 3);
}

#[test]
fn test_impl_with_nested_method_where_clause() {
    struct Wrapper<T>(T);

    #[op_result]
    impl<T> Wrapper<T> {
        fn add<U, V>(self, other: U) -> V
        where
            [(); T + U = V]:,
        {
            self.0 + other
        }
    }

    let w = Wrapper(1);
    let result: i32 = w.add(2);
    assert_eq!(result, 3);
}

#[test]
fn test_trait_with_where_clause() {
    #[op_result]
    trait Addable<T> {
        fn add(self, other: T) -> output!(Self + T)
        where
            [(); Self + T]:;
    }

    impl Addable<i32> for i32 {
        fn add(self, other: i32) -> output!(Self + i32) {
            self + other
        }
    }

    // This test is more about compilation than runtime
    let _: i32 = 1.add(2);
}

// Note: Trait method where clauses with `=` syntax may not work correctly
// due to how Rust parses trait definitions. This is a limitation.
#[test]
fn test_trait_with_nested_method_where_clause() {
    #[op_result]
    trait Addable {
        fn add<T>(self, other: T) -> output!(Self + T)
        where
            [(); Self + T]:;
    }

    #[op_result]
    impl Addable for i32 {
        fn add<T>(self, other: T) -> output!(Self + T)
        where
            [(); Self + T]:,
        {
            self + other
        }
    }

    let result: i32 = 1.add(2);
    assert_eq!(result, 3);
}

#[test]
fn test_impl_block_with_where_clause() {
    struct Wrapper<T>(T);

    // Test that where clause on impl block itself works
    #[op_result]
    impl<T> Wrapper<T>
    where
        [(); T + T]:,
        T: Copy,
    {
        fn double(self) -> output!(T + T) {
            self.0 + self.0
        }
    }

    let w = Wrapper(1);
    assert_eq!(w.double(), 2);
}

#[test]
fn test_impl_block_with_where_clause_and_method_where_clause() {
    struct Wrapper<T>(T);

    // Test that both impl block where clause and method where clause work
    #[op_result]
    impl<T> Wrapper<T>
    where
        [(); T + T]:,
    {
        fn add<U>(self, other: U) -> output!(T + U)
        where
            [(); T + U]:,
        {
            self.0 + other
        }
    }

    let w = Wrapper(1);
    assert_eq!(w.add(2), 3);
}

