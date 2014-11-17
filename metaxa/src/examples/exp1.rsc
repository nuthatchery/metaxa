module examples::exp1

lexical IntLit = [0-9]+;

start syntax Exp 
	= IntLit 
	| left Exp "+" Exp
	; 

