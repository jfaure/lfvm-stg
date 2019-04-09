 # LFVM STG
Map lazy functional language constructs to LLVM IR. 

Try it ! ```stack build --exec "lfvm-exe examples/fib.stg"```

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
`fib n = case n of { 0->0; 1->1; _-> fib (n-1) fib (n-2) }`

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
LFVM supplies a simple frontend (exts/main.hs) for experimentation, using the same syntax as haskell one-liners, and (mostly) ignoring newlines.
for example: ```not a = let b = 0 ; c = 1 in case a of { b->c ; _ -> 1 } ``` The ';' syntax is simpler to implement because the parser can ignore indentation.
