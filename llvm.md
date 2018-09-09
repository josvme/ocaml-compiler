## LLVM

# LLVM IR
* Infinite number of registers - stack locations or SSA values
* Locals starts with %, globals with @

# LLVM Structure
Module contains Functions/Global Variables
Function contains BasicBlocks/Arguments and they are similar to c functions.
BasicBlock contains list of instructions - They end with a control flow instruction.
Instruction is opcode + vector of operands
All operands have types and instruction result is typed
IR is almost all doubly-linked lists.

Example of Transformation
![llvm](images/llvmtrans.png)

https://www.cs.cmu.edu/afs/cs/academic/class/15745-s12/public/lectures/L3-LLVM-Part1.pdf page 19

# Instructions
* Subclassed to LoadInst, SmpInst, BranchInst etc. 
* Branches can occur only at end of basic blocks

# IR Types
* Integers, Floating Points
* Arrays [ #elements (>= 0) x element type ]
* Functions (returntype (paramlist))
* Pointers (type*, type addrspace(N)*)
* Vectors (<#elements (>0) x element type>)
* Structures ( { typelist })
* Types in LLVM IR are structural

