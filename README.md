Compiler Construction

ECE/CS 553

Duke University

========

Part.1--Lexical Analysis

Due Data: 01/27/2014

Team Members: Kuang Han, Dan Deng, Ang Li

In this part of the compiler, we implemented features for a working compiler that contains fully functional rules for translating an input string into a sequences of tokens associated with the specific programming language.

Main techniques used in this analysis include:

	-deterministic finite-state automata

	-regular expressions


We defined four different states for handling syntax and conventions in the Tiger language: INITIAL, COMMENT, STRING, and NPSTRING.

INITIAL state:

	-the start state of any token sequence

	-when special characters, symbols, integer numbers, and reserved words were encountered, a token of the matched type is produced based on the longest match rule

	-accept "/*" to transition into a COMMENT state, and \" to transition into a STRING state

COMMENT state:

	-discard any character encountered during COMMENT state except for the transition rules

	-when "/*" is encountered during COMMENT state, a nested comment is detected, and we increase the comment count by 1

	-when "*/" is encountered, decrease the comment count by 1, and transition to INITIAL if count is 0

STRING state:

	-append every non-special character to the string builder when in STRING state

	-when [\\] is encountered, the program transitions into NPSTRING state, whereby it is ready to take legal escape characters defined by the Tiger language

	-accept \" to end the STRING state and transition into INITIAL state, producing a string token at the same time

NPSTRING state:

	-accept zero or more number of [\n\ \t\f] as the escape sequence

	-transition back to STRING state when another [\\] is encountered

	-any other character is illegal in this case


Global variables defined:

	-lineNum, an int to keep track of the line count for accurate error reporting

	-linePos, an int to keep track of the character count in a line for accurate error reporting

	-strBuilder, a string to build up a string during STRING state

	-strPosition, an int to keep track of the character count of the start of a string

	-uncloseStr, a Boolean to report any unclosed string

	-cmCount, an int to keep a comment count for handling nested comments


Other modifications:

	-we modified driver.sml to include some steps for better error handling: our error report contains the specific file name for the file that causes the error, and we reset the lineNum and linePos in ErrorMsg before every run of parsing.

	-we modified the eof() function to report any unclosed string or unclosed comment, and if everything is right, return and print out the EOF token with a position count

Known bugs or problems:

	-no bugs
	
	-if you enter \ddd in a string literal with an illegal ASCII code in decimal, the lexer throws an uncaught exception 
