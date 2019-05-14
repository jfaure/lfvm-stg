deconstruction order atm must correspond exactly to the order as defined `data =`. obviously this is cancerous behavior

Alternatives in algebraic sum types can either be inlined in the main struct/union or allocated seperately and pointed to. Currently subtypes are always allocated seperately, but this should only happen for subtypes much bigger than their alternatives. types <= pointer size should always be inline

Find the last use points of local bindings and free them just after (is this always possible ? : probably yes since let-in expressions are easy to reason with. we can't release top level bindings unless the program is in the form `let topbinds in main`)

Lazy evaluation + graph reduction

llvm-hs-pure module level fresh name supply (mainly for emitting string globals)

Improve parser:
 - properly handle indentation

---

Benchmarks
