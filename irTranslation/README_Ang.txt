IR Translation
Team Memebers: Kuang Han, Dan Deng, Ang Li
Date: 03/27/2014 (used two late days on this assignment)


How to run (in SML console):

CM.make "sources.cm";
Main.test "test.tig";

where "test.tig" can be replaced with any tiger language source file.
The output will be printed to the console immediately after running the command.


Implementations:

All main features including let expressions, function declarations, variable declarations, for/while loops, static links in nested functions, array and record type variables, subscript variables and field variables are all tested and good (eyeballed assembly-like code and it looks logical and good).

Important Note:

We got 85 for type-checker but haven't updated it yet. So please do not test on programs that our type-checkers throw errors, because the IR translation may not be handled for those error cases (like it just throws an error instead of producing the IR tree).
We will let you know when we improved it.