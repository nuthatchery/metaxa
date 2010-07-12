module MagnoliaAST

data AST = MagnoliaTree();                                              // Program ::=  
data AST = ModuleHead();                                                // ModuleHead ::= "module"   ";"
data AST = Name();                                                      // Identifier ::= 
data AST = Name();                                                      // Name ::= 
data AST = QName();                                                     // Name ::=  "." 
data AST = Type();                                                      // Type ::= 
data AST = NilType();                                                   // Type ::= "(" ")"
data AST = AltType();                                                   // Type ::=  "|" 
data AST = ProdType();                                                  // ProdType ::=  "," 
data AST = Dummy();                                                     // Type ::= "("  ")"
data AST = Struct();                                                    // Type ::= "struct" 
data AST = Int();                                                       // Literal ::= 
data AST = Real();                                                      // Literal ::= 
data AST = String();                                                    // StringLiteral ::= """  """
data AST = Block();                                                     // BlockStat ::= "{"  "}"
data AST = Nop();                                                       // Stat ::= ";"
data AST = If();                                                        // Stat ::= "if"  "then"  "else"  "end"
data AST = While();                                                     // Stat ::= "while"  "do"  "end"
data AST = Open();                                                      // Stat ::= "open"  "in"  "end"
data AST = For();                                                       // Stat ::= "for"  "in"  "do"  "end"
data AST = Call();                                                      // Stat ::= "call"  "("  ")" ";"
data AST = Proc();                                                      // ProcName ::= 
data AST = Yield();                                                     // Stat ::= "yield"  ";"
data AST = Break();                                                     // Stat ::= "break" ";"
data AST = Return();                                                    // Stat ::= "return" ";"
data AST = Return();                                                    // Stat ::= "return"  ";"
data AST = Assign();                                                    // Stat ::=  "="  ";"
data AST = Let();                                                       // Stat ::= "let"  "in"  "end"
data AST = VarDef();                                                    // LetClause ::= "var"  ":"  "="  ";"
data AST = Assert();                                                    // Stat ::= "assert"   ";"
data AST = By();                                                        // AssertClause ::= "by" 
data AST = By();                                                        // AssertClause ::= "by" "simplify" 
data AST = QED();                                                       // AssertClause ::= "qed"
data AST = Undefined();                                                 // Expr ::= "_"
data AST = Var();                                                       // Var ::=  ":" 
data AST = Literal();                                                   // Expr ::= 
data AST = Tuple();                                                     // Expr ::= "("  ")"
data AST = TypeExpr();                                                  // Expr ::= ":" 
data AST = Struct();                                                    // Expr ::=  "$" "{"  "}"
data AST = Struct();                                                    // Expr ::=  "{"  "}"
data AST = Field();                                                     // InitSpec ::=  ":=" 
data AST = Apply();                                                     // Expr ::=  "("  ")"
data AST = Fun();                                                       // Fun ::= 
data AST = IfThenElseExpr();                                            // Expr ::= "if"  "then"  "else"  "end"
data AST = BlockExpr();                                                 // Expr ::= "{"  "}"
data AST = ListCons();                                                  // Expr ::= "["  "]"
data AST = ListCons();                                                  // Expr ::= "["  "|"  "]"
data AST = NumRep();                                                    // DataRep ::=  ".."  ";"
data AST = AliasType();                                                 // DataRep ::= "type"  ";"
data AST = StructRep();                                                 // DataRep ::= "struct" "{"  "}"
data AST = UnionRep();                                                  // DataRep ::= "union" "{"  "}"
data AST = TermRep();                                                   // DataRep ::=  ";"
data AST = TermCons0();                                                 // ConsSpec ::= 
data AST = TermCons();                                                  // ConsSpec ::=  "("  ")"
data AST = DefDecl();                                                   // BraceDecl ::=    
data AST = DefDecl();                                                   // SemiDecl ::=    "="  ";"
data AST = DefDecl();                                                   // Decl ::=    "="  ";"
data AST = DefDecl();                                                   // BraceDecl ::=    
data AST = DeclBody();                                                  // DeclBody ::= "{"  "}"
data AST = ProcClause();                                                // ProcClause ::= "procedure"  
data AST = Assign();                                                    // ProcIdentifier ::= "_=_"
data AST = FunClause();                                                 // FunClause ::= "function"   ":" 
data AST = PredClause();                                                // PredClause ::= "predicate"  
data AST = AxiomClause();                                               // AxiomClause ::= "axiom"  
data AST = AxiomClause();                                               // AxiomClause ::= "theorem"  
data AST = AxiomClause();                                               // AxiomClause ::= "proof"  
data AST = Dummy();                                                     // FunctionParamList ::= "("  ")"
data AST = Dummy();                                                     // ProcedureParamList ::= "("  ")"
data AST = Param();                                                     // FunctionParam ::=  ":" 
data AST = Param();                                                     // ProcedureParam ::=   ":" 
data AST = Obs();                                                       // ParamMode ::= "obs"
data AST = Upd();                                                       // ParamMode ::= "upd"
data AST = Out();                                                       // ParamMode ::= "out"
data AST = Exp();                                                       // ParamMode ::= "exp"
data AST = Giv();                                                       // ParamMode ::= "giv"
data AST = Del();                                                       // ParamMode ::= "del"
data AST = Nrm();                                                       // ParamMode ::= "nrm"
data AST = VarClause();                                                 // VarClause ::= "var"  ":" 
data AST = TypeClause();                                                // TypeClause ::= "type" 
data AST = ConceptClause();                                             // Decl ::=  "concept"   "=" 
data AST = ImplClause();                                                // Decl ::=  "implementation"   "=" 
data AST = Library();                                                   // Decl ::=  "library"   "=" 
data AST = ConceptClause();                                             // Decl ::=  "concept"   
data AST = ImplClause();                                                // Decl ::=  "implementation"   
data AST = Attrs();                                                     // AttrClause ::= "["  "]"
data AST = Attr();                                                      // Attribute ::=  "("  ")"
data AST = Guard();                                                     // GuardClause ::= "guard" 
data AST = Requires();                                                  // Decl ::= "requires"  ";"
data AST = Partition();                                                 // Decl ::= "partition"  "by"  ";"
data AST = Generate();                                                  // Decl ::= "generate"  "by"  ";"
data AST = Generate();                                                  // Decl ::= "generate"  "by"  ";"
data AST = Free();                                                      // Decl ::= "free"  "by"  ";"
data AST = ModelsImpl();                                                // Decl ::= "satisfaction"   "="   "models" 
data AST = ModelsImpl();                                                // Decl ::= "satisfaction"   "=" "on"   "models" 
data AST = WithClause();                                                // WithClause ::= "with" 
data AST = Concept();                                                   // ConceptExpr ::= 
data AST = ConceptBody();                                               // ConceptExpr ::= 
data AST = SignatureOf();                                               // ConceptExpr ::= "signature" 
data AST = MorphedConcept();                                            // ConceptExpr ::=  
data AST = Impl();                                                      // ImplExpr ::= 
data AST = ImplMorph();                                                 // ImplExpr ::=  
data AST = ImplBody();                                                  // ImplExpr ::= 
data AST = ExternalLib();                                               // ImplExpr ::= "external"   "defines" 
data AST = ExternalLib();                                               // ImplExpr ::= "external"   "on"  "defines" 
data AST = OnDefines();                                                 // ImplExpr ::= "on"  "defines" 
data AST = Morphism();                                                  // Morphism ::= "morphism" 
data AST = Morphism();                                                  // SubClause ::= "morphism" 
data AST = ImplMorphism();                                              // MorphClause ::= "{"  "}"
data AST = ImplMorphism();                                              // MorphClause ::= "{"  "}"
data AST = Morphisms();                                                 // Morphism ::= "["  "]"
data AST = Rename();                                                    // MorphClause ::=  "=>" 
data AST = Morphism();                                                  // MorphClause ::= 
data AST = ImplMorph();                                                 // MorphClause ::= 
data AST = Protect();                                                   // Morphism ::= "protect"  "guard" 
data AST = Protect();                                                   // MorphClause ::= "protect"  "guard" 
data AST = Protect();                                                   // SubClause ::= "protect"  "guard" 
data AST = Protect();                                                   // Morphism ::= "protect"  "by" 
data AST = Protect();                                                   // MorphClause ::= "protect"  "by" 
data AST = Protect();                                                   // SubClause ::= "protect"  "by" 
data AST = At();                                                        // ImplExpr ::=  "@" 
data AST = AtAt();                                                      // ImplExpr ::=  "@@" 
data AST = Star();                                                      // ImplExpr ::=  "*" 
data AST = StarStar();                                                  // ImplExpr ::=  "**" 
data AST = Plus();                                                      // ImplExpr ::=  "+" 
data AST = PlusPlus();                                                  // ImplExpr ::=  "++" 
data AST = Times();                                                     // ImplExpr ::=  "times" 
data AST = AtAt();                                                      // ConceptExpr ::=  "@@" 
data AST = Star();                                                      // ConceptExpr ::=  "*" 
data AST = StarStar();                                                  // ConceptExpr ::=  "**" 
data AST = Plus();                                                      // ConceptExpr ::=  "+" 
data AST = PlusPlus();                                                  // ConceptExpr ::=  "++" 
data AST = Times();                                                     // ConceptExpr ::=  "times" 
data AST = Hex();                                                       // Literal ::= 
data AST = Bin();                                                       // Literal ::= 
data AST = Oct();                                                       // Literal ::= 
data AST = BaseFor();                                                   // Stat ::= "for"  "in"  
data AST = BasePrintLn();                                               // Stat ::= "print"  ";"
data AST = BasePrint();                                                 // Stat ::= "print"  "," ";"
data AST = BaseVarDefTI();                                              // Stat ::= "var"  ":"  "="  ";"
data AST = BaseVarDefI();                                               // Stat ::= "var"  "="  ";"
data AST = BaseVarDefI();                                               // Stat ::= "var"  ";"
data AST = BaseVarDefT();                                               // Stat ::= "var"  ":"  ";"
data AST = DotOp();                                                     // Expr ::=  "." 
data AST = DotOp();                                                     // Expr ::=  "." 
data AST = BinOp();                                                     // Expr ::=   
data AST = BinOp();                                                     // Expr ::=   
data AST = BinOp();                                                     // Expr ::=   
data AST = BinOp();                                                     // Expr ::=   
data AST = BinOp();                                                     // Expr ::=   
data AST = In();                                                        // Expr ::=  "in" 
data AST = NotIn();                                                     // Expr ::=  "not" "in" 
data AST = BinOp();                                                     // Expr ::=   
data AST = BinOp();                                                     // Expr ::=   
data AST = BinOp();                                                     // Expr ::=   
data AST = BinOp();                                                     // Expr ::=   
data AST = BinOp();                                                     // Expr ::=   
data AST = BinOp();                                                     // Expr ::=   
data AST = BinOp();                                                     // Expr ::=   
data AST = BinOp();                                                     // Expr ::=   
data AST = Index();                                                     // Expr ::=  "["  "]"
data AST = PreOp();                                                     // Expr ::=  
data AST = PreOp();                                                     // Expr ::=  
data AST = PreOp();                                                     // Expr ::=  
data AST = NoDefDecl();                                                 // Decl ::=   ";"
data AST = NoDefDecl();                                                 // Decl ::=   ";"
data AST = NoDefDecl();                                                 // Decl ::=   ";"
data AST = FunClause();                                                 // FunClause ::= "define"  ":" 
data AST = DataInvariant();                                             // FunClause ::= "dataInvariant" 
data AST = DataInvariant();                                             // ProcClause ::= "dataInvariant" 
data AST = Congruence();                                                // FunClause ::= "congruence" 
data AST = Congruence();                                                // ProcClause ::= "congruence" 
data AST = AnonParam();                                                 // FunctionParam ::= 
data AST = ObsParam();                                                  // ProcedureParam ::=  ":" 
data AST = AnonParam();                                                 // ProcedureParam ::=  ":" 
data AST = Attr();                                                      // Attribute ::= 
data AST = Opens();                                                     // SubClause ::= "opens" "("  ")"
data AST = Default();                                                   // SubClause ::= "default" "("  ")"
data AST = SimpleModule();                                              // ModuleHead ::= 
data AST = Language();                                                  // ModuleClause ::= "language" 
data AST = Imports();                                                   // ModuleClause ::= "imports" 
data AST = Requires();                                                  // ModuleClause ::= "requires" 
data AST = CompilePragma();                                             // ModuleClause ::= "compile" """  """ "{"  "}"
data AST = Pragma();                                                    // CompileClause ::=  "("  ")" ";"
data AST = Dummy();                                                     // QuotedString ::= """  """
data AST = ImportRequires();                                            // RequiresClause ::=  "<"  ">"
data AST = ImportAll();                                                 // ImportClause ::= 
data AST = Unresolved();                                                // ConceptClauseArgument ::= 
data AST = ImportModule();                                              // ImportClause ::= "module" 
data AST = ImportRename();                                              // ImportClause ::= "module"  "=" 
data AST = Nop();                                                       // Decl ::= ";"
data AST = UserSyntax0();                                               // Stat ::=  "."  "="  ";"
data AST = UserSyntax1();                                               // Stat ::=  "["  "]" "="  ";"
data AST = UserSyntax2();                                               // Expr ::= 
data AST = UserSyntax3();                                               // Stat ::= "if"  "then"  "end"
data AST = UserSyntax4();                                               // Stat ::= "if" "("  ")" "{"  "}"
data AST = UserSyntax5();                                               // Stat ::= "if" "("  ")" "{"  "}" "else" "{"  "}"

