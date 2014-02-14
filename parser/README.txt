Compiler component: Parser

Group: Ang Li, Dan Deng, Kuang Han

Comments:
On top of all the grammar rules for the Tiger Language as described by the book, we added precedence directives for some of the keywords in the language so that shift/reduce conflicts may be resolved. An example of a precedence directive we added is:

%nonassoc ELSE

We tell ML-yacc that this terminal binds less tightly than arithmetic and boolean operators so that the expression:

IF exp THEN exp ELSE exp

can be parsed unambiguously. Other precedence directives were used in a similar way.