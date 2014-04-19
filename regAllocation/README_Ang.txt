Register Allocation
Team Memebers: Kuang Han, Dan Deng, Ang Li
Date: 04/18/2014


How to run (in SML console):

CM.make "sources.cm"; (There're match non-exhaustive warnings in the given file canon.sml)
Main.compile "test.tig";

where "test.tig" can be replaced with any tiger language source file and "test.tig.s" is the resultant assembly code.


Important message:

1.
We implemented Coalescing as Extra Credit. Move instructions are eliminated wherever possible, except for the case with pre-colored registers.

2.
Type-checker is now fixed. I checked queens.tig, merge.tig and all 49 test cases available. It reports specific errors when there are errors, and goes through when it is valid Tiger program.