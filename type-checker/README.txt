ECE553 Compiler Construction
Type-Checker

Group: Ang Li, Kuang Han, Dan Deng
Date: 02/28/2014

To run the type-checker:

CM.make "sources.cm";
Semant.transProg(Parse.parse "test.tig");

All errors will be reported in the console as standard output. Return type of transProg function is unit.

We improved our parser to be able to handle the following exp: lvalue LBRACK exp RBRACK, which will produce a syntax error in previous version of our parser. We also put the mutually recursive type or function declarations in a list as specified by Absyn.sml instead of lists of single elements in previous version of our parser.

Apart from all the usual type checking requirements, we implemented the following features as described in the Tiger Lanuage Manual:

1. Mutually recursive types and functions are type-checked correctly. Mutually recursive types are only valid with either record or array types.

2. Mutually recursive types with cycles will generate an error. Mutually recursive types or functions with repeated names will generate an error, as specified by Tiger Language Manual.

3. Two record types are not the same even though their fields can be identical.

4. Subtyping system--every type is a subtype of unit, and error type is a subtype of any type to prevent excessive error reporting.

5. Detection of excessive break expressions in for/while loops.
