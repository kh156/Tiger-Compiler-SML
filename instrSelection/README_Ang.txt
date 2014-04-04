Instruction Selection
Team Memebers: Kuang Han, Dan Deng, Ang Li
Date: 04/03/2014


How to run (in SML console):

CM.make "sources.cm"; (There're match non-exhaustive warnings in the given file canon.sml)
Main.test "test.tig"; (to see the IR Translation Tree printed to console)
MainGiven.compile "test.tig" (result will be in the folder with "test.tig.s")

where "test.tig" can be replaced with any tiger language source file.

Implementations:

All main features including let expressions, function declarations, variable declarations, for/while loops, static links in nested functions, array and record type variables, subscript variables and field variables are all tested and good.

Important Note:

We got 85 for type-checker but haven't updated it yet. So please do not test on programs that our type-checkers throw errors.

Added:

Type-checker is now fixed. I checked queens.tig, merge.tig and all 49 test cases available. It reports specific errors when there are errors, and goes through when it is valid Tiger program.

Things to do:

-display labels for frags correctly, in addition to useless labels
-move arguments into variable accesses (inFrame or inReg) at start of function
-jr $ra at the end of function
-display special registers correctly in string
-