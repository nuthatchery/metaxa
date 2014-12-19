module examples::exp1

lexical IntLit = [0-9]+;

start syntax Exp 
	= const: IntLit 
	| left exp: Exp "+" Exp
	; 

