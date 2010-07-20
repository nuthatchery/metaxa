module MagnoliaCxxAST

data AST = Name(AST);                                                   // Identifier ::= ID
data AST = Name(AST);                                                   // Name ::= ID
data AST = QName(AST, AST);                                             // Name ::= Name "::" ID
data AST = Type(AST);                                                   // Type ::= Name
data AST = NilType();                                                   // Type ::= "void"
data AST = Struct(AST);                                                 // Type ::= "struct" DeclBody
data AST = Int(AST);                                                    // Literal ::= DecNumeral
data AST = Real(AST);                                                   // Literal ::= FloatNumeral
data AST = String(AST);                                                 // StringLiteral ::= """ String """
data AST = Hex(AST);                                                    // Literal ::= HexNumeral
data AST = Bin(AST);                                                    // Literal ::= BinNumeral
data AST = Oct(AST);                                                    // Literal ::= OctNumeral
data AST = Block(AST);                                                  // BlockStat ::= "{" Stat* "}"
data AST = Nop();                                                       // Stat ::= ";"
data AST = If(AST, AST, AST);                                           // Stat ::= "if" "(" Expr ")" "{" Stat* "}" "else" "{" Stat* "}"
data AST = While(AST, AST);                                             // Stat ::= "while" "(" Expr ")" "{" Stat* "}"
data AST = CxxFor(AST, AST, AST);                                       // Stat ::= "for" "(" Decl Expr ";" Expr ")"
data AST = Call(AST, AST);                                              // Stat ::= Proc "(" {Expr ","}* ")" ";"
data AST = Proc(AST);                                                   // Proc ::= ProcName
data AST = Yield(AST);                                                  // Stat ::= "return" Expr ";"
data AST = Break();                                                     // Stat ::= "break" ";"
data AST = Return();                                                    // Stat ::= "return" ";"
data AST = Assign(AST, AST);                                            // Stat ::= Identifier "=" Expr ";"
data AST = Let(AST, AST);                                               // Stat ::= "{" LetClause* Stat* "}"
data AST = VarDef(AST, AST, AST);                                       // LetClause ::= Type Identifier "=" Expr ";"
data AST = Assert(AST, AST);                                            // Stat ::= "assert" "(" Expr AssertClause* ")" ";"
data AST = By(AST);                                                     // AssertClause ::= "," Expr
data AST = By(AST);                                                     // AssertClause ::= "," "simplify" "(" Expr ")"
data AST = QED();                                                       // AssertClause ::= "qed" "(" ")"
data AST = Undefined();                                                 // Expr ::= "_"
data AST = Var(AST);                                                    // Var ::= Name
data AST = Literal(AST);                                                // Expr ::= Literal
data AST = Apply(AST, AST);                                             // Expr ::= Fun "(" {Expr ","}* ")"
data AST = Fun(AST);                                                    // Fun ::= FunName
data AST = IfThenElseExpr(AST, AST, AST);                               // Expr ::= Expr "?" Expr ":" Expr
data AST = ListCons(AST);                                               // Expr ::= "list" "(" {Expr ","}* ")"
data AST = ListCons(AST, AST);                                          // Expr ::= "list" "(" {Expr ","}* "," Expr ")"
data AST = DefDecl(AST, AST, AST, AST);                                 // BraceDecl ::= Modifier* StatDeclarative SubClause* BlockStat
data AST = DefDecl(AST, AST, AST, AST);                                 // SemiDecl ::= Modifier* ExprDeclarative SubClause* "{" "return" Expr ";" "}"
data AST = DefDecl(AST, AST, AST, AST);                                 // Decl ::= Modifier* "typedef" TypeDeclarative SubClause* "=" Type ";"
data AST = DefDecl(AST, AST, AST, AST);                                 // BraceDecl ::= Modifier* DeclDeclarative SubClause* DeclBody
data AST = NoDefDecl(AST, AST, AST);                                    // Decl ::= Modifier* StatDeclarative SubClause* ";"
data AST = NoDefDecl(AST, AST, AST);                                    // Decl ::= Modifier* ExprDeclarative SubClause* ";"
data AST = NoDefDecl(AST, AST, AST);                                    // Decl ::= Modifier* "struct" TypeDeclarative SubClause* ";"
data AST = DeclBody(AST);                                               // DeclBody ::= "{" Decl* "}"
data AST = ProcClause(AST, AST);                                        // ProcClause ::= "void" ProcIdentifier ProcedureParamList
data AST = Assign();                                                    // ProcIdentifier ::= "operator" "="
data AST = FunClause(AST, AST, AST);                                    // FunClause ::= Type FunIdentifier FunctionParamList
data AST = Guard(AST);                                                  // GuardClause ::= "guard" Expr
data AST = PredClause(AST, AST);                                        // PredClause ::= "predicate" FunIdentifier FunctionParamList
data AST = AxiomClause(AST, AST);                                       // AxiomClause ::= "axiom" Identifier FunctionParamList
data AST = AxiomClause(AST, AST);                                       // AxiomClause ::= "theorem" Identifier FunctionParamList
data AST = AxiomClause(AST, AST);                                       // AxiomClause ::= "proof" Identifier FunctionParamList
data AST = Dummy(AST);                                                  // FunctionParamList ::= "(" {FunctionParam ","}* ")"
data AST = Dummy(AST);                                                  // ProcedureParamList ::= "(" {ProcedureParam ","}* ")"
data AST = Param(AST, AST);                                             // FunctionParam ::= Type VarIdentifier
data AST = Param(AST, AST);                                             // ProcedureParam ::= Type VarIdentifier
data AST = ObsParam(AST, AST);                                          // ProcedureParam ::= Type Identifier
data AST = AnonParam(AST);                                              // ProcedureParam ::= Type
data AST = VarClause(AST, AST);                                         // VarClause ::= Type VarIdentifier
data AST = TypeClause(AST);                                             // TypeClause ::= "struct" TypeIdentifier
data AST = DotOp(AST, AST);                                             // Expr ::= Expr "." Identifier
data AST = DotOp(AST, AST);                                             // Expr ::= Expr "." DecNumeral
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr MULOP Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr ADDOP Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr SHFOP Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr RNGOP Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr CMPOP Expr
data AST = In(AST, AST);                                                // Expr ::= Expr "in" Expr
data AST = NotIn(AST, AST);                                             // Expr ::= Expr "not" "in" Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr EQUOP Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr BTAND Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr BTXOR Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr BITOR Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr SUBST Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr LGAND Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr LOGOR Expr
data AST = BinOp(AST, AST, AST);                                        // Expr ::= Expr LGIMP Expr
data AST = Index(AST, AST);                                             // Expr ::= Expr "[" {Expr ","}* "]"
data AST = PreOp(AST, AST);                                             // Expr ::= NEGOP Expr
data AST = PreOp(AST, AST);                                             // Expr ::= BTNOT Expr
data AST = PreOp(AST, AST);                                             // Expr ::= LGNOT Expr
data AST = CxxTree(AST);                                                // Program ::= Decl*

