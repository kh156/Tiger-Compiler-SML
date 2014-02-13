state 21: shifting LPAREN or LBRACK or LBRACE is the correct way because the ID can't be part of a lvalue anymore

state 63, 93, 94, 121, 124, 134: if any operator is trailing an exp, the exp is not finished. Shifting is the right thing to do in these cases.
