use op_result::output;

// Custom types for testing each operator
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct AddType(i32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SubType(i32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct MulType(i32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct DivType(i32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RemType(i32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct BitAndType(i32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct BitOrType(i32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct BitXorType(i32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ShlType(i32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ShrType(i32);

// Output types for each operation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct AddOutput(i32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SubOutput(i32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct MulOutput(i32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct DivOutput(i32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RemOutput(i32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct BitAndOutput(i32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct BitOrOutput(i32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct BitXorOutput(i32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ShlOutput(i32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ShrOutput(i32);

// Implement all the operators
impl std::ops::Add<AddType> for AddType {
    type Output = AddOutput;
    fn add(self, rhs: AddType) -> Self::Output {
        AddOutput(self.0 + rhs.0)
    }
}

impl std::ops::Sub<SubType> for SubType {
    type Output = SubOutput;
    fn sub(self, rhs: SubType) -> Self::Output {
        SubOutput(self.0 - rhs.0)
    }
}

impl std::ops::Mul<MulType> for MulType {
    type Output = MulOutput;
    fn mul(self, rhs: MulType) -> Self::Output {
        MulOutput(self.0 * rhs.0)
    }
}

impl std::ops::Div<DivType> for DivType {
    type Output = DivOutput;
    fn div(self, rhs: DivType) -> Self::Output {
        DivOutput(self.0 / rhs.0)
    }
}

impl std::ops::Rem<RemType> for RemType {
    type Output = RemOutput;
    fn rem(self, rhs: RemType) -> Self::Output {
        RemOutput(self.0 % rhs.0)
    }
}

impl std::ops::BitAnd<BitAndType> for BitAndType {
    type Output = BitAndOutput;
    fn bitand(self, rhs: BitAndType) -> Self::Output {
        BitAndOutput(self.0 & rhs.0)
    }
}

impl std::ops::BitOr<BitOrType> for BitOrType {
    type Output = BitOrOutput;
    fn bitor(self, rhs: BitOrType) -> Self::Output {
        BitOrOutput(self.0 | rhs.0)
    }
}

impl std::ops::BitXor<BitXorType> for BitXorType {
    type Output = BitXorOutput;
    fn bitxor(self, rhs: BitXorType) -> Self::Output {
        BitXorOutput(self.0 ^ rhs.0)
    }
}

impl std::ops::Shl<ShlType> for ShlType {
    type Output = ShlOutput;
    fn shl(self, rhs: ShlType) -> Self::Output {
        ShlOutput(self.0 << rhs.0)
    }
}

impl std::ops::Shr<ShrType> for ShrType {
    type Output = ShrOutput;
    fn shr(self, rhs: ShrType) -> Self::Output {
        ShrOutput(self.0 >> rhs.0)
    }
}

// Additional impls for nested operation tests
impl std::ops::Mul<MulType> for AddOutput {
    type Output = MulOutput;
    fn mul(self, rhs: MulType) -> Self::Output {
        MulOutput(self.0 * rhs.0)
    }
}

impl std::ops::Div<DivType> for SubOutput {
    type Output = DivOutput;
    fn div(self, rhs: DivType) -> Self::Output {
        DivOutput(self.0 / rhs.0)
    }
}

impl std::ops::Mul<SubOutput> for AddOutput {
    type Output = MulOutput;
    fn mul(self, rhs: SubOutput) -> Self::Output {
        MulOutput(self.0 * rhs.0)
    }
}

#[test]
fn test_add() {
    let a = AddType(10);
    let b = AddType(32);
    let result: output!(AddType + AddType) = a + b;
    assert_eq!(result.0, 42);
}

#[test]
fn test_sub() {
    let a = SubType(50);
    let b = SubType(8);
    let result: output!(SubType - SubType) = a - b;
    assert_eq!(result.0, 42);
}

#[test]
fn test_mul() {
    let a = MulType(6);
    let b = MulType(7);
    let result: output!(MulType * MulType) = a * b;
    assert_eq!(result.0, 42);
}

#[test]
fn test_div() {
    let a = DivType(84);
    let b = DivType(2);
    let result: output!(DivType / DivType) = a / b;
    assert_eq!(result.0, 42);
}

#[test]
fn test_rem() {
    let a = RemType(142);
    let b = RemType(100);
    let result: output!(RemType % RemType) = a % b;
    assert_eq!(result.0, 42);
}

#[test]
fn test_bitand() {
    let a = BitAndType(0b111111);
    let b = BitAndType(0b101010);
    let result: output!(BitAndType & BitAndType) = a & b;
    assert_eq!(result.0, 0b101010);
}

#[test]
fn test_bitor() {
    let a = BitOrType(0b101000);
    let b = BitOrType(0b000010);
    let result: output!(BitOrType | BitOrType) = a | b;
    assert_eq!(result.0, 0b101010);
}

#[test]
fn test_bitxor() {
    let a = BitXorType(0b111111);
    let b = BitXorType(0b101101);
    let result: output!(BitXorType ^ BitXorType) = a ^ b;
    assert_eq!(result.0, 0b010010);
}

#[test]
fn test_shl() {
    let a = ShlType(21);
    let b = ShlType(1);
    let result: output!(ShlType << ShlType) = a << b;
    assert_eq!(result.0, 42);
}

#[test]
fn test_shr() {
    let a = ShrType(84);
    let b = ShrType(1);
    let result: output!(ShrType >> ShrType) = a >> b;
    assert_eq!(result.0, 42);
}

// Test nested operations
#[test]
fn test_nested_add_mul() {
    let a = AddType(10);
    let b = AddType(2);
    let c = MulType(3);
    // (10 + 2) * 3 = 12 * 3 = 36
    let result: output!((AddType + AddType) * MulType) = (a + b) * c;
    assert_eq!(result.0, 36);
}

#[test]
fn test_nested_sub_div() {
    let a = SubType(50);
    let b = SubType(8);
    let c = DivType(2);
    // (50 - 8) / 2 = 42 / 2 = 21
    let result: output!((SubType - SubType) / DivType) = (a - b) / c;
    assert_eq!(result.0, 21);
}

#[test]
fn test_triple_nested() {
    let a = AddType(10);
    let b = AddType(2);
    let c = SubType(8);
    let d = SubType(2);
    // (10 + 2) * (8 - 2) = 12 * 6 = 72
    let result: output!((AddType + AddType) * (SubType - SubType)) = (a + b) * (c - d);
    assert_eq!(result.0, 72);
}

#[test]
fn test_parens_preservation() {
    let a = AddType(20);
    let b = AddType(22);
    let result: output!((AddType + AddType)) = a + b;
    assert_eq!(result.0, 42);
}
