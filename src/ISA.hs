{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module ISA where
import           Data.List (find, intercalate)

type Name = String

data Register = Register{rType :: RegisterType, name :: Name} deriving(Ord, Eq)
data RegisterType = Temporary | Argument | Saved | Virtual | Hardwired | Special deriving(Ord, Eq)


instance Show Register where
    show = name

zero :: Register
zero = Register Hardwired "zero"
ra :: Register
ra = Register Special "ra"
pc :: Register
pc = Register Special "pc"
sp :: Register
sp = Register Special "sp"
a0 :: Register
a0 = Register Argument "a0"
a1 :: Register
a1 = Register Argument "a1"
a2 :: Register
a2 = Register Argument "a2"
a3 :: Register
a3 = Register Argument "a3"
a4 :: Register
a4 = Register Argument "a4"
a5 :: Register
a5 = Register Argument "a5"
a6 :: Register
a6 = Register Argument "a6"
a7 :: Register
a7 = Register Argument "a7"

s1 :: Register
s1 = Register Saved "s1"
s2 :: Register
s2 = Register Saved "s2"
s3 :: Register
s3 = Register Saved "s3"
s4 :: Register
s4 = Register Saved "s4"
s5 :: Register
s5 = Register Saved "s5"
s6 :: Register
s6 = Register Saved "s6"
s7 :: Register
s7 = Register Saved "s7"
s8 :: Register
s8 = Register Saved "s8"
s9 :: Register
s9 = Register Saved "s9"
s10 :: Register
s10 = Register Saved "s10"
s11 :: Register
s11 = Register Saved "s11"

t0 :: Register
t0 = Register Temporary "t0"
t1 :: Register
t1 = Register Temporary "t1"
t2 :: Register
t2 = Register Temporary "t2"
t3 :: Register
t3 = Register Temporary "t3"
t4 :: Register
t4 = Register Temporary "t4"
t5 :: Register
t5 = Register Temporary "t5"
t6 :: Register
t6 = Register Temporary "t6"


registers :: [Register]
registers = hardwiredRegisters ++ specialRegisters ++ argumentRegisters ++ temporaryRegisters ++ savedRegisters

hardwiredRegisters :: [Register]
hardwiredRegisters = [zero]

specialRegisters :: [Register]
specialRegisters = [ra, pc, sp]

argumentRegisters :: [Register]
argumentRegisters = [a0, a1, a2, a3, a4, a5, a6, a7]

temporaryRegisters :: [Register]
temporaryRegisters = [t0, t1, t2, t3, t4, t5, t6]

savedRegisters :: [Register]
savedRegisters = [s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11]


toReg :: String -> Register
toReg s = case find (\r -> name r == s) registers of
    Just reg -> reg
    _        -> error "Register does not exist"


type Opcode = Int
type Rd = Register
type Rs1 = Register
type Rs2 = Register
type Imm = Int
type LabelName = String
type BranchLabelName = String

{-
I-type: imm - 12 bit signed, rs2 - 5 bit, rs1 - 5 bit, rd - 5 bit, opcode - 5 bit
II-tpye: imm - 17 bit signed, rs1 - 5 bit, rd - 5 bit, opcode - 5 bit
III-type: imm - 27 bit signed, opcode - 5 bit
-}

data Instruction =
    MathOp Opcode Rd Rs1 Rs2 Imm |
    Branch    Opcode Rd Rs1 Rs2 BranchLabelName |
    MathImmideate Opcode Rd Rs1 Imm |
    LoadAddress Rd LabelName |
    Label Name |
    Jump Opcode BranchLabelName |
    Call LabelName |
    Ret |
    Halt  | -- actually there is no halt, it'll be replaced with a syscall
    Section Name |
    StoreData Rs1 Rs2 | -- rs1 - base, rs2 - src
    LoadData Rd Rs1 | -- rd - dest, rs1 - base
    Global Name |
    DataByte [Int] String
    deriving(Eq)

instance Show Instruction where
    show(MathOp op rd rs1 rs2 _) =
        let
            prefix = case op of
                0 -> "add"
                1 -> "sub"
                2 -> "mul"
                3 -> "div"
                8 -> "rem"
        in
            prefix ++ " " ++ show rd ++ ", " ++ show rs1 ++ ", " ++ show rs2
    show(Branch op _ rs1 rs2 imm) =
        let
            prefix = case op of
                4 -> "beq"
                5 -> "bne"
                6 -> "bge"
                7 -> "blt"
        in
            prefix ++ " "  ++ show rs1 ++ ", " ++ show rs2 ++ ", " ++ imm
    show(MathImmideate op rd rs1 imm) =
        let
            prefix = case op of
                16 -> "addi"
        in
            prefix ++ " " ++ show rd ++ ", " ++ show rs1 ++ ", " ++ show imm
    show (LoadAddress rd label) = "la " ++ show rd ++ ", " ++ label
    show (Label name) = name ++ ":"
    show (Jump _ label) = "j " ++ label
    show (Call label) = "call " ++ label
    show Ret = "ret"
    show Halt = "call halt"
    show (Section name) = ".section " ++ name
    show (StoreData base src) = "sd " ++ show base ++ ", 0(" ++ show src ++ ")"
    show (LoadData dest base) = "ld " ++ show dest ++ ", 0(" ++ show base ++ ")"
    show (Global name) = ".global " ++ name
    show (DataByte lst addr) = addr ++ ": " ++ ".byte " ++ (intercalate ", " $ map show lst)

-- R-R instructions
add :: Rd -> Rs1 -> Rs2 -> Instruction
add rd rs1 rs2 = MathOp 0 rd rs1 rs2 0
sub :: Rd -> Rs1 -> Rs2 -> Instruction
sub rd rs1 rs2 = MathOp 1 rd rs1 rs2 0
mul :: Rd -> Rs1 -> Rs2 -> Instruction
mul rd rs1 rs2 = MathOp 2 rd rs1 rs2 0
div :: Rd -> Rs1 -> Rs2 -> Instruction
div rd rs1 rs2 = MathOp 3 rd rs1 rs2 0
rem :: Rd -> Rs1 -> Rs2 -> Instruction
rem rd rs1 rs2 = MathOp 8 rd rs1 rs2 0
-- Branch instructions
beq :: Rs1 -> Rs2 -> BranchLabelName -> Instruction
beq = Branch 4 pc -- if(@rs1 == @rs2) pc <- pc + imm
bne :: Rs1 -> Rs2 -> BranchLabelName -> Instruction
bne = Branch 5 pc
bge :: Rs1 -> Rs2 -> BranchLabelName -> Instruction
bge = Branch 6 pc
blt :: Rs1 -> Rs2 -> BranchLabelName -> Instruction
blt = Branch 7 pc
jal :: LabelName -> Instruction
jal = Jump 31

addI :: Rd -> Rs1 -> Imm -> Instruction
addI = MathImmideate 16

push :: Rd -> [Instruction]
push rd = [ISA.addI sp sp (-8), ISA.StoreData rd sp]

pop :: Rd -> [Instruction]
pop rd = [ISA.LoadData rd sp, ISA.addI sp sp 8]