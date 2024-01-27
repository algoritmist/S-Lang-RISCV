# Slang RISC-V
Slang compiler for RISC-V 64-bit architecture.

## Language Syntax
```Haskell
Program ::= [FunctionDefinition]
FunctionDefinition ::= FunctionName "(" DefinitionArgs ")" "=" Expr
VariableDefinition ::= VariableName "=" Expr
DefinitionArgs ::= (Variable)
Expr ::= "(" Expr ")" | FunctionCall | LetExpr | IfThenElseExpr | BinOp Expr Expr | UnOp Expr | Primitive
FunctionCall ::= FunctionName CallArgs
LetExpr ::= "let" [VariableDefinition] "in" VariableDefinition
IfThenElseExpr ::= "if" Expr "then" [Expr] | "if" Expr "then" [Expr] "else" [Expr]
BinOp ::= + | - | * | / | == | > | < | /= | ++
UnOp ::= -
CallArgs ::= (Expr)
FunctionName ::= Name
VariableName ::= Name
Variable ::= Name
Name ::= ASCII-String
Primitive ::= Int | String | List | Bool
```
where
```Haskell
[T] - list of type T, (T) - tuple of type T
```
More examples you can find in directory ```golden```, wich also contais golden tests.

## Building the project
### Dependencies
You need at least ```ghc-9.4.8``` and ```cabal 3.8``` to successfuly build the project. I recommend using ```ghcup``` for managind dependencies.

Also you'll need to build [qemu](https://github.com/qemu/qemu) for riscv64.
For virtual machine you need ```openbsbi, u-boot-qemu``` packages. Also you'll need ```libslirp``` and ```./configure --enable-libslirp``` in newest versions of qemu.

### Steps
1. build qemu for RISC-V
2. install & run RISC-V virtual machine using configuration script in ```qemu``` dir
3. run ```cabal run Compiler-exe -- <path_to_source.sl> <path_to_dest.asm>```
4. use ssh to send the asm file and ```stdlib``` to the virtual machine. Default port is 5555, but you can change it in qemu configuration script
5. run ```make <name_of_asm_file> & ./a.out``` inside the ```stdlib``` directory on VM
6. enjoy the result
