 # LFVM STG
Map lazy functional language constructs to LLVM IR. 

Try it ! ```make && ./lfvm examples/map.stg --jit```

LLVM is an extremely powerful framework for compiler backends, giving us access to cutting edge optimizations, cross-platform compilation, the lldb debugger, interoperability with other llvm frontends (eg. clang), and more.

The top-level constructs of LFVM STG are very similar to ghc STG, here are the differences:
- Leaf types are llvm primitives (types and instructions)
- Data is as much as possible seperated from the language: lfvm will produces constructor, deconstructor (and destructor if recursive) llvm functions.
- No transpilation, the only overhead is lowering llvm-hs-pure AST to C++, so no file IO or feeding C source to gcc
- No unnecessary laziness: only data is lazy by default, and the 'lazy' wrapping is removed (via coercion channel, think pi calculus) if it is used more than once.
- Every non-trivial binding gets a function, this is a more natural execution model and gives us proper stack traces. llvm can easily inline if optimizing aggressively.
- No gc: LFVM aims for perfect memory management, certainly no need to copy most of the heap when more memory is needed as in ghc's gc.
- Memory management: A stack frame is responsible for all data returned by functions it calls. It gives it's stack space for those functions to use. Recursive data still must go on the heap obviously, and the stack frame is responsible for calling their deconstructors
- Buffered recursive types, especially the list type will be internally a linked list of arrays with logarithmically increasing size
- Lazy data is conveniently represented by an llvm struct containing a function pointer and the args to call it with.
See stg/StgToLLVM.hs to understand how this maps to llvm

LFVM is an STG (Spineless tagless g-machine)
- Spineless: No single data structure: top level bindings reference each other
- Tagless: Heap values aren't annotated (eg. type, evaluation status etc..)
- Graph-reducing: Lazy data can be overwritten by simpler values. (if used more than once)

Examples - Naive fib function (no llvm optimizations)
------------
fib 40 runs 5x faster than equivalent in ghc
`fib n = case n of { 0->0; 1->1; _-> fib (n-1) + fib (n-2) }`

```llvm
define i32 @fib(i32 %a) {
  switch i32 %a, label %3 [
    i32 0, label %1
    i32 1, label %2
  ]

; <label>:1:                                      ; preds = %0
  br label %9

; <label>:2:                                      ; preds = %0
  br label %9

; <label>:3:                                      ; preds = %0
  %4 = sub i32 %a, 1
  %5 = call i32 @fib(i32 %4)
  %6 = sub i32 %a, 2
  %7 = call i32 @fib(i32 %6)
  %8 = add i32 %5, %7
  br label %9

; <label>:9:                                      ; preds = %3, %2, %1
  %10 = phi i32 [ %8, %3 ], [ 0, %1 ], [ 1, %2 ]
  ret i32 %10
```

Example - Map Function (no llvm optimizations)
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

%List = type { i32, i8* }
@Nothing = global %List zeroinitializer

define %List* @List(i8* %Mem, i32 %tag, i8* %unionPtr) {
  %return-mem = bitcast i8* %Mem to %List*
  %1 = getelementptr %List, %List* %return-mem, i32 0, i32 0
  store i32 %tag, i32* %1
  %2 = getelementptr %List, %List* %return-mem, i32 0, i32 1
  store i8* %unionPtr, i8** %2
  ret %List* %return-mem
}

define %List* @Cons(i8* %mem, i32 %A, %List* %A1) {
  %1 = bitcast i8* %mem to %List*
  %2 = bitcast %List* %1 to i8*
  %3 = call i8* @malloc(i32 ptrtoint (%List* getelementptr inbounds (%List, %List* null, i32 1) to i32))
  %4 = bitcast i8* %3 to { i32, %List* }*
  %5 = getelementptr { i32, %List* }, { i32, %List* }* %4, i32 0, i32 0
  store i32 %A, i32* %5
  %6 = getelementptr { i32, %List* }, { i32, %List* }* %4, i32 0, i32 1
  store %List* %A1, %List** %6
  %7 = bitcast { i32, %List* }* %4 to i8*
  %8 = call %List* @List(i8* %mem, i32 1, i8* %7)
  ret %List* %8
}

define %List* @map(i32 (i32)* %a, %List* %a1) {
  %1 = getelementptr %List, %List* %a1, i32 0, i32 0
  %tag = load i32, i32* %1
  %valPtr = getelementptr %List, %List* %a1, i32 0, i32 1
  %cast = bitcast i8** %valPtr to {}**
  %2 = load {}*, {}** %cast
  %cast1 = bitcast i8** %valPtr to { i32, %List* }**
  %3 = load { i32, %List* }*, { i32, %List* }** %cast1
  switch i32 %tag, label %14 [
    i32 0, label %4
    i32 1, label %5
  ]

; <label>:4:                                      ; preds = %0
  br label %15

; <label>:5:                                      ; preds = %0
  %6 = getelementptr { i32, %List* }, { i32, %List* }* %3, i32 0, i32 0
  %7 = load i32, i32* %6
  %8 = getelementptr { i32, %List* }, { i32, %List* }* %3, i32 0, i32 1
  %9 = load %List*, %List** %8
  %10 = call i32 %a(i32 %7)
  %11 = call %List* @map(i32 (i32)* %a, %List* %9)
  %12 = call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %13 = call %List* @Cons(i8* %12, i32 %10, %List* %11)
  br label %15

; <label>:14:                                     ; preds = %0
  call void @NoPatternMatchError()
  unreachable

; <label>:15:                                     ; preds = %5, %4
  %16 = phi %List* [ @Nothing, %4 ], [ %13, %5 ]
  ret %List* %16
}
```

Frontend
-----------
LFVM supplies a simple frontend (exts/main.hs) for experimentation, using the same syntax as haskell one-liners, with some vague support for simple indentation.

Status
-------
Missing:
 - Memory management
 - Lazy
 - Threading
