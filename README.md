 # LFVM STG
Map lazy functional language constructs to LLVM IR. 

Try it ! ```make && ./lfvm examples/map.stg --jit```

LLVM is an extremely powerful framework for compiler backends, giving us access to all sorts of optimizations, cross-platform compilation, the lldb debugger, interoperability with other llvm frontends (eg. clang), and more.

LFVM STG is very similar to ghc STG, but see [Vs ghc STG](#ghc-stg) for differences with ghc STG.


- LFVM STG is a very thin layer over LLVM (Using LLVM Types and Instructions),
See stg/StgToLLVM.hs for a detailed description of how this maps to llvm
- LFVM implements desugaring routines for lowering language constructs like algebraic datatypes and free variables to LLVM STG.

LFVM is an STG (Spineless tagless g-machine)
- Spineless: No single data structure: top level bindings reference each other
- Tagless: Heap values aren't annotated (eg. type, evaluation status etc..)
- Graph-reducing: Closures can be overwritten by simpler values.

Example
------------
`fib n = case n of { 0->0; 1->1; _-> fib (n-1) + fib (n-2) }`

```llvm
define external ccc  double @fib(double  %a)    {
; <label>:0:
  switch double %a, label %1 [double 0.000000e0, label %7 double 1.000000e0, label %8] 
; <label>:1:
  %2 = fsub double %a, 1.000000e0 
  %3 =  call ccc  double  @fib(double  %2)  
  %4 = fsub double %a, 2.000000e0 
  %5 =  call ccc  double  @fib(double  %4)  
  %6 = fadd double %3, %5 
  br label %9 
; <label>:7:
  br label %9 
; <label>:8:
  br label %9 
; <label>:9:
  %10 = phi double [%6, %1], [0.000000e0, %7], [1.000000e0, %8] 
  ret double %10 
}
```

Example - Map Function
---------------
```haskell
type Int = IntegerType {typeBits = 32}
data List = Nothing | Cons Int List
map :: (Int -> Int) -> List -> List
map f l = decon l of
  Nothing -> Nothing
  Cons a b -> Cons (f a) (map f b)
  ```

```llvm
declare external ccc  i8* @malloc(i32)
declare external ccc  void @free(i8*)
declare external ccc  void @error(i32, i32, i8*)
@noPatternMsg =  unnamed_addr  constant [26 x i8] c"No default switchCase alt\00"

define external ccc  void @NoPatternMatchError()    {
   call ccc  void  @error(i32  0, i32  0, i8*  getelementptr inbounds ([26 x i8], [26 x i8]* @noPatternMsg, i32 0, i32 0))
  unreachable
}


define external ccc  {i32, i8*}* @List(i32  %tag, i8*  %unionPtr)    {
  %1 =  call ccc  i8*  @malloc(i32  ptrtoint ({i32, i8*}* getelementptr inbounds ({i32, i8*}, {i32, i8*}* inttoptr (i32 0 to {i32, i8*}*), i32 1) to i32))
  %2 = bitcast i8* %1 to {i32, i8*}*
  %3 = getelementptr  {i32, i8*}, {i32, i8*}* %2, i32 0, i32 0
  store  i32 %tag, i32* %3
  %4 = getelementptr  {i32, i8*}, {i32, i8*}* %2, i32 0, i32 1
  store  i8* %unionPtr, i8** %4
  ret {i32, i8*}* %2
}

%Nothing = type {}
@Nothing =    global {i32, i8*} { i32 0, i8* inttoptr (i32 0 to i8*) }

%Cons = type {i32, {i32, i8*}*}

define external ccc  {i32, i8*}* @Cons(i32  %A, {i32, i8*}*  %A1)    {
  %1 =  call ccc  i8*  @malloc(i32  ptrtoint (%Cons* getelementptr inbounds (%Cons, %Cons* inttoptr (i32 0 to %Cons*), i32 1) to i32))
  %2 = bitcast i8* %1 to %Cons*
  %3 = getelementptr  %Cons, %Cons* %2, i32 0, i32 0
  store  i32 %A, i32* %3
  %4 = getelementptr  %Cons, %Cons* %2, i32 0, i32 1
  store  {i32, i8*}* %A1, {i32, i8*}** %4
  %5 = bitcast %Cons* %2 to i8*
  %6 =  call ccc  {i32, i8*}*  @List(i32  1, i8*  %5)
  ret {i32, i8*}* %6
}

define external ccc  {i32, i8*}* @map(i32 (i32)*  %a, {i32, i8*}*  %a1)    {
; <label>:0:
  %1 = getelementptr  {i32, i8*}, {i32, i8*}* %a1, i32 0, i32 0
  %tag = load  i32, i32* %1
  %valPtr = getelementptr  {i32, i8*}, {i32, i8*}* %a1, i32 0, i32 1
  %cast = bitcast i8** %valPtr to %Nothing**
  %2 = load  %Nothing*, %Nothing** %cast
  %cast1 = bitcast i8** %valPtr to %Cons**
  %3 = load  %Cons*, %Cons** %cast1
  switch i32 %tag, label %13 [i32 0, label %4 i32 1, label %5]
; <label>:4:
  br label %14
; <label>:5:
  %6 = getelementptr  %Cons, %Cons* %3, i32 0, i32 0
  %7 = load  i32, i32* %6
  %8 = getelementptr  %Cons, %Cons* %3, i32 0, i32 1
  %9 = load  {i32, i8*}*, {i32, i8*}** %8
  %10 =  call ccc  i32  %a(i32  %7)
  %11 =  call ccc  {i32, i8*}*  @map(i32 (i32)*  %a, {i32, i8*}*  %9)
  %12 =  call ccc  {i32, i8*}*  @Cons(i32  %10, {i32, i8*}*  %11)
  br label %14
; <label>:13:
   call ccc  void  @NoPatternMatchError()
  unreachable
; <label>:14:
  %15 = phi {i32, i8*}* [@Nothing, %4], [%12, %5]
  ret {i32, i8*}* %15
}
```

Vs ghc STG
--------------
* LFVM STG only understands LLVM types.
* Arbitrary LLVM functions can be embedded directly in the STG as primitives. (Not quite the same as a foreign call) - This is useful mainly for optimizations in the desugarer, like lowering algebraic product types to llvm structs.
* All free varables (that aren't functions) become explicit arguments before codegen.
  STG "Free vars" are functions (arity >=0) residing in llvm global scope.
* Data constructors are desugared to tagged StgCases beforehand (or direct jumps if possible, but only for non-top level bindings)
* "trivial" types become unions/structs, this excludes:
    sum types that refer to themselves, these become 'lazy' closures.
    TODO: some of these could (?!) be optimized to dynamic/static arrays, especially in cases where we can predict their size before evaluating them.

Frontend
-----------
LFVM supplies a simple frontend (exts/main.hs) for experimentation, using the same syntax as haskell one-liners, with some vague support for simple indentation.

Status
-------
The most pressing features missing is freeing datatypes as they come out of scope and allowing deconstructors to be listed in any order in the parser.
I'm researching a perfect gc that will either pass data down the let-in stack or free it immediately; I suspect rearranging let-ins to reflect memory scope is the way to go.

See TODO.md
