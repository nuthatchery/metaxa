/*
Extension of exp1.rsc; now with both addition and multiplication, and brackets.
*/
module examples::exp2

lexical IntLit = [0-9]+;

start syntax Exp 
	= IntLit 
	| bracket "(" Exp ")"
	> left Exp "*" Exp
	> left Exp "+" Exp
	; 

