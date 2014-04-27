Final Compiler Submission
Team Memebers: Kuang Han, Dan Deng, Ang Li
Date: 04/26/2014

How to run (in SML console):

CM.make "sources.cm";
Main.compile "test.tig";

where "test.tig" can be replaced with any tiger language source file and "test.tig.s" is the resultant assembly code.


Important messages:

1.
We implemented Actual Spilling, and Coalescing as Extra Credit.

2.
We provided a "testSpill.tig" file for your convenience of testing the actual spilling functionalities.
Actual spilling selects the node with the highest degree in the interference graph with the hope that this frees up many more nodes for simplifying.
Move instructions are eliminated wherever possible with Coalescing, except for the case with pre-colored registers such as $v0, $a0, etc.

3.
There are no known errors or bugs. If any of our previous parts is imperfect when you grade it, please regrade the current version (since we haven't heard back from instruction selection and register allocation).