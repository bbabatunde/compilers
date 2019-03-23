# compilers

#TODO 

#AMOGH
1. Implement ANF for ELambda. Hint: it’s quite similar to what needed to be done to ANF a declaration.2. 
2. EXTRA: Update your type system to handle the new constructs.

#BABA
1. Implement the compilation of CLambda and CApp, ignoring stored variables. You’ll deal with storing and checking the arity and code pointer, and generating and jumping over the instructions for a function. Test as you go.
2. Implement freevars, testing as you go. You can test with the helper tfvs, which takes a name, an expression string, and a list of identifiers, and checks that freevars returns the same list of strings (in any order).
3. Implement storing and restoring of variables in the compilation of CLambda and CApp
4. Implement support for ELetRec. 4410: You need to support just a single recursive declaration. 6410: You need to support multiple, mutually recursive declarations.
5. EXTRA: Restore support for print, equal, and any other runtime-provided functions, by generating an appropriate closure value.

#DONE

#AMOGH

#BABA
