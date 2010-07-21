package org.magnolialang.xatree;

import java.util.HashMap;
import java.util.Map;
import org.eclipse.imp.pdb.facts.IList;
import static org.magnolialang.xatree.XaTreeFactory.*;

class MagnoliaSkinTable {
	public static Map<String,IList> getMap() {
		Map<String,IList> tbl = new HashMap<String,IList>();

		// Stat ::= "for" Identifier "in" Expr Stat
		tbl.put("BaseFor/3", vf.list(token("for"), space(" "), child(0), space(" "), token("in"), space(" "), child(1), space(" "), child(2)));

		// Modifier ::= "default"
		tbl.put("DefaultModifier/0", vf.list(token("default")));

		// ProdType ::= Type "," ProdType
		tbl.put("ProdType/2", vf.list(child(0), space(" "), token(","), space(" "), child(1)));

		// ExternalExpr ::= Identifier Name "on" InstExpr "defines" InstExpr
		tbl.put("ExternalOnDefines/4", vf.list(child(0), space(" "), child(1), space(" "), token("on"), space(" "), child(2), space(" "), token("defines"), space(" "), child(3)));

		// PredClause ::= "predicate" FunIdentifier FunctionParamList
		tbl.put("PredClause/2", vf.list(token("predicate"), space(" "), child(0), space(" "), child(1)));

		// InstExpr ::= InstExpr "protect" AlgDecl
		tbl.put("Protected/2", vf.list(child(0), space(" "), token("protect"), space(" "), child(1)));

		// Program ::= ModuleHead TopDecl*
		tbl.put("MagnoliaTree/2", vf.list(child(0), space(" "), child(1)));

		// SatisfactionExpr ::= InstExpr "models" InstExpr
		tbl.put("Models/2", vf.list(child(0), space(" "), token("models"), space(" "), child(1)));

		// DataRep ::= "type" Type ";"
		tbl.put("AliasType/1", vf.list(token("type"), space(" "), child(0), space(" "), token(";")));

		// Type ::= "(" ")"
		tbl.put("NilType/0", vf.list(token("("), space(" "), token(")")));

		// SubClause ::= "opens" "(" {Identifier ","}* ")"
		tbl.put("Opens/1", vf.list(token("opens"), space(" "), token("("), space(" "), sep(child(0),","), space(" "), token(")")));

		// Stat ::= "if" Expr "then" Stat* "end"
		tbl.put("UserSyntax3/2", vf.list(token("if"), space(" "), child(0), space(" "), token("then"), space(" "), child(1), space(" "), token("end")));

		// Expr ::= Expr "in" Expr
		tbl.put("In/2", vf.list(child(0), space(" "), token("in"), space(" "), child(1)));

		// AttrClause ::= "[" {Attribute ","}* "]"
		tbl.put("Attrs/1", vf.list(token("["), space(" "), sep(child(0),","), space(" "), token("]")));

		// Literal ::= OctNumeral
		tbl.put("Oct/1", vf.list(child(0)));

		// LetClause ::= "var" Identifier ":" Type "=" Expr ";"
		tbl.put("VarDef/3", vf.list(token("var"), space(" "), child(0), space(" "), token(":"), space(" "), child(1), space(" "), token("="), space(" "), child(2), space(" "), token(";")));

		// Stat ::= "if" Expr "then" Stat* "else" Stat* "end"
		tbl.put("If/3", vf.list(token("if"), space(" "), child(0), space(" "), token("then"), space(" "), child(1), space(" "), token("else"), space(" "), child(2), space(" "), token("end")));

		// Stat ::= "call" Proc "(" {Expr ","}* ")" ";"
		tbl.put("Call/2", vf.list(token("call"), space(" "), child(0), space(" "), token("("), space(" "), sep(child(1),","), space(" "), token(")"), space(" "), token(";")));

		// Stat ::= "var" Identifier "=" Expr ";"
		tbl.put("BaseVarDefI/2", vf.list(token("var"), space(" "), child(0), space(" "), token("="), space(" "), child(1), space(" "), token(";")));

		// Stat ::= "var" Identifier ";"
		tbl.put("BaseVarDefI/1", vf.list(token("var"), space(" "), child(0), space(" "), token(";")));

		// InstExpr ::= "defines" AlgDecl
		tbl.put("Defines/1", vf.list(token("defines"), space(" "), child(0)));

		// Modifier ::= "abstract"
		tbl.put("AbstractModifier/0", vf.list(token("abstract")));

		// Proc ::= ProcName
		tbl.put("Proc/1", vf.list(child(0)));

		// InstExpr ::= InstExpr "declared" Filter
		tbl.put("DeclaredFilter/2", vf.list(child(0), space(" "), token("declared"), space(" "), child(1)));

		// SatisfactionExpr ::= InstExpr "with" InstExpr "models" InstExpr
		tbl.put("WithModels/3", vf.list(child(0), space(" "), token("with"), space(" "), child(1), space(" "), token("models"), space(" "), child(2)));

		// ExternalExpr ::= Identifier Name "extends" InstExpr "on" InstExpr "defines" InstExpr
		tbl.put("ExternalExtendsOnDefines/5", vf.list(child(0), space(" "), child(1), space(" "), token("extends"), space(" "), child(2), space(" "), token("on"), space(" "), child(3), space(" "), token("defines"), space(" "), child(4)));

		// DataRep ::= "union" "{" Decl* "}"
		tbl.put("UnionRep/1", vf.list(token("union"), space(" "), token("{"), space(" "), child(0), space(" "), token("}")));

		// Decl ::= ";"
		tbl.put("Nop/0", vf.list(token(";")));

		// TopDecl ::= Modifier* "library" Identifier SubClause* "=" InstExpr
		tbl.put("LibraryDef/4", vf.list(child(0), space(" "), token("library"), space(" "), child(1), space(" "), child(2), space(" "), token("="), space(" "), child(3)));

		// Stat ::= "open" {Identifier ","}* "in" Stat* "end"
		tbl.put("Open/2", vf.list(token("open"), space(" "), sep(child(0),","), space(" "), token("in"), space(" "), child(1), space(" "), token("end")));

		// Name ::= ID
		tbl.put("Name/1", vf.list(child(0)));

		// Expr ::= "if" Expr "then" Expr "else" Expr "end"
		tbl.put("IfThenElseExpr/3", vf.list(token("if"), space(" "), child(0), space(" "), token("then"), space(" "), child(1), space(" "), token("else"), space(" "), child(2), space(" "), token("end")));

		// Stat ::= "assert" Expr AssertClause* ";"
		tbl.put("Assert/2", vf.list(token("assert"), space(" "), child(0), space(" "), child(1), space(" "), token(";")));

		// SubClause ::= "default" "(" {Identifier ","}* ")"
		tbl.put("Default/1", vf.list(token("default"), space(" "), token("("), space(" "), sep(child(0),","), space(" "), token(")")));

		// Renaming ::= Name "=>" Name
		tbl.put("Rename/2", vf.list(child(0), space(" "), token("=>"), space(" "), child(1)));

		// ParamMode ::= "obs"
		tbl.put("Obs/0", vf.list(token("obs")));

		// InstExpr ::= "on" InstExpr "defines" AlgDecl
		tbl.put("OnDefines/2", vf.list(token("on"), space(" "), child(0), space(" "), token("defines"), space(" "), child(1)));

		// Type ::= Name
		tbl.put("Type/1", vf.list(child(0)));

		// VarClause ::= "var" VarIdentifier ":" Type
		tbl.put("VarClause/2", vf.list(token("var"), space(" "), child(0), space(" "), token(":"), space(" "), child(1)));

		// RequiresClause ::= Identifier "<" {ConceptClauseArgument ","}* ">"
		tbl.put("ImportRequires/2", vf.list(child(0), space(" "), token("<"), space(" "), sep(child(1),","), space(" "), token(">")));

		// Decl ::= "congruence" InstExpr "on" InstExpr ";"
		tbl.put("CongruenceOn/2", vf.list(token("congruence"), space(" "), child(0), space(" "), token("on"), space(" "), child(1), space(" "), token(";")));

		// Stat ::= "while" Expr "do" Stat* "end"
		tbl.put("While/2", vf.list(token("while"), space(" "), child(0), space(" "), token("do"), space(" "), child(1), space(" "), token("end")));

		// Attribute ::= Name "(" {Expr ","}* ")"
		tbl.put("Attr/2", vf.list(child(0), space(" "), token("("), space(" "), sep(child(1),","), space(" "), token(")")));

		// Attribute ::= Identifier
		tbl.put("Attr/1", vf.list(child(0)));

		// InstExpr ::= "signature" InstExpr
		tbl.put("SignatureOf/1", vf.list(token("signature"), space(" "), child(0)));

		// Expr ::= Literal
		tbl.put("Literal/1", vf.list(child(0)));

		// Expr ::= Fun "(" {Expr ","}* ")"
		tbl.put("Apply/2", vf.list(child(0), space(" "), token("("), space(" "), sep(child(1),","), space(" "), token(")")));

		// ModuleClause ::= "imports" {ImportClause ","}*
		tbl.put("Imports/1", vf.list(token("imports"), space(" "), sep(child(0),",")));

		// Decl ::= "generate" {Type ","}+ "by" InstExpr ";"
		tbl.put("GenerateBy/2", vf.list(token("generate"), space(" "), sep(child(0),","), space(" "), token("by"), space(" "), child(1), space(" "), token(";")));

		// InstExpr ::= InstExpr "quotient" InstExpr
		tbl.put("Quotient/2", vf.list(child(0), space(" "), token("quotient"), space(" "), child(1)));

		// ParamMode ::= "nrm"
		tbl.put("Nrm/0", vf.list(token("nrm")));

		// InstExpr ::= InstExpr "morphism" InstExpr
		tbl.put("Morphed/2", vf.list(child(0), space(" "), token("morphism"), space(" "), child(1)));

		// ParamMode ::= "upd"
		tbl.put("Upd/0", vf.list(token("upd")));

		// Stat ::= "if" "(" Expr ")" "{" Stat* "}"
		tbl.put("UserSyntax4/2", vf.list(token("if"), space(" "), token("("), space(" "), child(0), space(" "), token(")"), space(" "), token("{"), space(" "), child(1), space(" "), token("}")));

		// Expr ::= LGNOT Expr
		tbl.put("PreOp/2", vf.list(child(0), space(" "), child(1)));

		// Stat ::= Expr "." Identifier "=" Expr ";"
		tbl.put("UserSyntax0/3", vf.list(child(0), space(" "), token("."), space(" "), child(1), space(" "), token("="), space(" "), child(2), space(" "), token(";")));

		// ProcedureParam ::= ParamMode ":" Type
		tbl.put("AnonParam/2", vf.list(child(0), space(" "), token(":"), space(" "), child(1)));

		// FunctionParam ::= Type
		tbl.put("AnonParam/1", vf.list(child(0)));

		// InstExpr ::= InstExpr "**" InstExpr
		tbl.put("TimesTimes/2", vf.list(child(0), space(" "), token("**"), space(" "), child(1)));

		// SingleAlgDecl ::= Modifier* StatDeclarative SubClause*
		tbl.put("StatDecl/3", vf.list(child(0), space(" "), child(1), space(" "), child(2)));

		// FilterExpr ::= "retain" InstExpr
		tbl.put("RetainFilter/1", vf.list(token("retain"), space(" "), child(0)));

		// ParamMode ::= "exp"
		tbl.put("Exp/0", vf.list(token("exp")));

		// AssertClause ::= "by" Expr
		tbl.put("By/1", vf.list(token("by"), space(" "), child(0)));

		// InstExpr ::= "on" InstExpr
		tbl.put("OnOf/1", vf.list(token("on"), space(" "), child(0)));

		// Expr ::= Type "$" "{" {InitSpec ","}* "}"
		tbl.put("Struct/2", vf.list(child(0), space(" "), token("$"), space(" "), token("{"), space(" "), sep(child(1),","), space(" "), token("}")));

		// Type ::= "struct" DeclBody
		tbl.put("Struct/1", vf.list(token("struct"), space(" "), child(0)));

		// SingleAlgDecl ::= Modifier* StatDeclarative SubClause* "=" BlockStat
		tbl.put("StatDef/4", vf.list(child(0), space(" "), child(1), space(" "), child(2), space(" "), token("="), space(" "), child(3)));

		// ParamMode ::= "giv"
		tbl.put("Giv/0", vf.list(token("giv")));

		// SingleAlgDecl ::= Modifier* ExprDeclarative SubClause* "=" Expr
		tbl.put("ExprDef/4", vf.list(child(0), space(" "), child(1), space(" "), child(2), space(" "), token("="), space(" "), child(3)));

		// Decl ::= Modifier* TypeDeclarative SubClause* ";"
		tbl.put("NoDefDecl/3", vf.list(child(0), space(" "), child(1), space(" "), child(2), space(" "), token(";")));

		// AssertClause ::= "qed"
		tbl.put("QED/0", vf.list(token("qed")));

		// Expr ::= ":" Type
		tbl.put("TypeExpr/1", vf.list(token(":"), space(" "), child(0)));

		// ParamMode ::= "out"
		tbl.put("Out/0", vf.list(token("out")));

		// Decl ::= "preserve" InstExpr "on" InstExpr ";"
		tbl.put("PreserveOn/2", vf.list(token("preserve"), space(" "), child(0), space(" "), token("on"), space(" "), child(1), space(" "), token(";")));

		// Decl ::= "homomorphism" InstExpr "on" InstExpr "with" InstExpr ";"
		tbl.put("HomomorphismOnWith/3", vf.list(token("homomorphism"), space(" "), child(0), space(" "), token("on"), space(" "), child(1), space(" "), token("with"), space(" "), child(2), space(" "), token(";")));

		// AlgDecl ::= DeclBody
		tbl.put("Decls/1", vf.list(child(0)));

		// Expr ::= "(" {Expr ","}* ")"
		tbl.put("Tuple/1", vf.list(token("("), space(" "), sep(child(0),","), space(" "), token(")")));

		// InstExpr ::= InstExpr "times" InstExpr
		tbl.put("Times/2", vf.list(child(0), space(" "), token("times"), space(" "), child(1)));

		// ImportClause ::= "module" Name "=" Name
		tbl.put("ImportRename/2", vf.list(token("module"), space(" "), child(0), space(" "), token("="), space(" "), child(1)));

		// Type ::= Type "|" Type
		tbl.put("AltType/2", vf.list(child(0), space(" "), token("|"), space(" "), child(1)));

		// SingleAlgDecl ::= Modifier* ExprDeclarative SubClause*
		tbl.put("ExprDecl/3", vf.list(child(0), space(" "), child(1), space(" "), child(2)));

		// Fun ::= FunName
		tbl.put("Fun/1", vf.list(child(0)));

		// ConsSpec ::= Identifier "(" {DataRep ","}* ")"
		tbl.put("TermCons/2", vf.list(child(0), space(" "), token("("), space(" "), sep(child(1),","), space(" "), token(")")));

		// ImportClause ::= Name
		tbl.put("ImportAll/1", vf.list(child(0)));

		// InstExpr ::= InstExpr "on" Filter
		tbl.put("OnFilter/2", vf.list(child(0), space(" "), token("on"), space(" "), child(1)));

		// DataRep ::= DecNumeral ".." DecNumeral ";"
		tbl.put("NumRep/2", vf.list(child(0), space(" "), token(".."), space(" "), child(1), space(" "), token(";")));

		// InstExpr ::= InstExpr "dataInvariant" InstExpr
		tbl.put("DataInvariant/2", vf.list(child(0), space(" "), token("dataInvariant"), space(" "), child(1)));

		// ProcClause ::= "dataInvariant" ProcedureParamList
		tbl.put("DataInvariant/1", vf.list(token("dataInvariant"), space(" "), child(0)));

		// SingleAlgDecl ::= Modifier* DeclDeclarative SubClause*
		tbl.put("DeclDecl/3", vf.list(child(0), space(" "), child(1), space(" "), child(2)));

		// InstExpr ::= InstExpr "@" InstExpr
		tbl.put("At/2", vf.list(child(0), space(" "), token("@"), space(" "), child(1)));

		// InstExpr ::= "declared" InstExpr
		tbl.put("DeclaredOf/1", vf.list(token("declared"), space(" "), child(0)));

		// SingleAlgDecl ::= Modifier* TypeDeclarative SubClause* "=" Type
		tbl.put("TypeDef/4", vf.list(child(0), space(" "), child(1), space(" "), child(2), space(" "), token("="), space(" "), child(3)));

		// ModuleHead ::= "module" Name ModuleClause* ";"
		tbl.put("ModuleHead/2", vf.list(token("module"), space(" "), child(0), space(" "), child(1), space(" "), token(";")));

		// DeclBody ::= "{" Decl* "}"
		tbl.put("DeclBody/1", vf.list(token("{"), space(" "), child(0), space(" "), token("}")));

		// Stat ::= "if" "(" Expr ")" "{" Stat* "}" "else" "{" Stat* "}"
		tbl.put("UserSyntax5/3", vf.list(token("if"), space(" "), token("("), space(" "), child(0), space(" "), token(")"), space(" "), token("{"), space(" "), child(1), space(" "), token("}"), space(" "), token("else"), space(" "), token("{"), space(" "), child(2), space(" "), token("}")));

		// Type ::= "(" ProdType ")"
		tbl.put("Dummy/1", vf.list(token("("), space(" "), child(0), space(" "), token(")")));

		// InstExpr ::= InstExpr "homomorphism" InstExpr "on" InstExpr "with" InstExpr
		tbl.put("Homomorphism/4", vf.list(child(0), space(" "), token("homomorphism"), space(" "), child(1), space(" "), token("on"), space(" "), child(2), space(" "), token("with"), space(" "), child(3)));

		// Stat ::= Identifier "[" {Expr ","}+ "]" "=" Expr ";"
		tbl.put("UserSyntax1/3", vf.list(child(0), space(" "), token("["), space(" "), sep(child(1),","), space(" "), token("]"), space(" "), token("="), space(" "), child(2), space(" "), token(";")));

		// Decl ::= "partition" {Type ","}+ "by" InstExpr ";"
		tbl.put("PartitionBy/2", vf.list(token("partition"), space(" "), sep(child(0),","), space(" "), token("by"), space(" "), child(1), space(" "), token(";")));

		// Expr ::= "{" Stat* "}"
		tbl.put("BlockExpr/1", vf.list(token("{"), space(" "), child(0), space(" "), token("}")));

		// Name ::= Name "." ID
		tbl.put("QName/2", vf.list(child(0), space(" "), token("."), space(" "), child(1)));

		// TopDecl ::= Modifier* "satisfaction" Identifier SubClause* "=" SatisfactionExpr
		tbl.put("SatisfactionDef/4", vf.list(child(0), space(" "), token("satisfaction"), space(" "), child(1), space(" "), child(2), space(" "), token("="), space(" "), child(3)));

		// ModuleClause ::= "language" {Name ","}*
		tbl.put("Language/1", vf.list(token("language"), space(" "), sep(child(0),",")));

		// InstExpr ::= InstExpr FilterExpr
		tbl.put("Filtered/2", vf.list(child(0), space(" "), child(1)));

		// InstExpr ::= InstExpr "++" InstExpr
		tbl.put("PlusPlus/2", vf.list(child(0), space(" "), token("++"), space(" "), child(1)));

		// BraceDecl ::= Modifier* DeclDeclarative SubClause* DeclBody
		tbl.put("DefDecl/4", vf.list(child(0), space(" "), child(1), space(" "), child(2), space(" "), child(3)));

		// ConceptClauseArgument ::= Type
		tbl.put("Unresolved/1", vf.list(child(0)));

		// ModuleClause ::= "compile" """ String """ "{" CompileClause* "}"
		tbl.put("CompilePragma/2", vf.list(token("compile"), space(" "), token("\""), space(" "), child(0), space(" "), token("\""), space(" "), token("{"), space(" "), child(1), space(" "), token("}")));

		// ExternalExpr ::= Identifier Name "defines" InstExpr
		tbl.put("ExternalDefines/3", vf.list(child(0), space(" "), child(1), space(" "), token("defines"), space(" "), child(2)));

		// Decl ::= "preserve" InstExpr ";"
		tbl.put("Preserve/1", vf.list(token("preserve"), space(" "), child(0), space(" "), token(";")));

		// Expr ::= Expr "." DecNumeral
		tbl.put("DotOp/2", vf.list(child(0), space(" "), token("."), space(" "), child(1)));

		// Stat ::= "for" Identifier "in" Expr "do" Stat* "end"
		tbl.put("For/3", vf.list(token("for"), space(" "), child(0), space(" "), token("in"), space(" "), child(1), space(" "), token("do"), space(" "), child(2), space(" "), token("end")));

		// TopDecl ::= Modifier* "implementation" Identifier SubClause* "=" InstExpr
		tbl.put("ImplDef/4", vf.list(child(0), space(" "), token("implementation"), space(" "), child(1), space(" "), child(2), space(" "), token("="), space(" "), child(3)));

		// DataRep ::= "struct" "{" Decl* "}"
		tbl.put("StructRep/1", vf.list(token("struct"), space(" "), token("{"), space(" "), child(0), space(" "), token("}")));

		// InstExpr ::= InstExpr "@@" InstExpr
		tbl.put("AtAt/2", vf.list(child(0), space(" "), token("@@"), space(" "), child(1)));

		// Stat ::= "var" Identifier ":" Type "=" Expr ";"
		tbl.put("BaseVarDefTI/3", vf.list(token("var"), space(" "), child(0), space(" "), token(":"), space(" "), child(1), space(" "), token("="), space(" "), child(2), space(" "), token(";")));

		// InstExpr ::= "full" InstExpr
		tbl.put("FullOf/1", vf.list(token("full"), space(" "), child(0)));

		// TopDecl ::= Modifier* "concept" Identifier SubClause* "=" InstExpr
		tbl.put("ConceptDef/4", vf.list(child(0), space(" "), token("concept"), space(" "), child(1), space(" "), child(2), space(" "), token("="), space(" "), child(3)));

		// Stat ::= "print" {Expr ","}* ";"
		tbl.put("BasePrintLn/1", vf.list(token("print"), space(" "), sep(child(0),","), space(" "), token(";")));

		// CompileClause ::= ID "(" {QuotedString ","}* ")" ";"
		tbl.put("Pragma/2", vf.list(child(0), space(" "), token("("), space(" "), sep(child(1),","), space(" "), token(")"), space(" "), token(";")));

		// ModuleHead ::= 
		tbl.put("SimpleModule/0", vf.list());

		// InitSpec ::= Identifier ":=" Expr
		tbl.put("Field/2", vf.list(child(0), space(" "), token(":="), space(" "), child(1)));

		// Decl ::= "congruence" InstExpr ";"
		tbl.put("Congruence/1", vf.list(token("congruence"), space(" "), child(0), space(" "), token(";")));

		// ImportClause ::= "module" Name
		tbl.put("ImportModule/1", vf.list(token("module"), space(" "), child(0)));

		// Stat ::= Expr "=" Expr ";"
		tbl.put("Assign/2", vf.list(child(0), space(" "), token("="), space(" "), child(1), space(" "), token(";")));

		// Stat ::= "return" Expr ";"
		tbl.put("Return/1", vf.list(token("return"), space(" "), child(0), space(" "), token(";")));

		// Stat ::= "return" ";"
		tbl.put("Return/0", vf.list(token("return"), space(" "), token(";")));

		// ProcedureParam ::= Identifier ":" Type
		tbl.put("ObsParam/2", vf.list(child(0), space(" "), token(":"), space(" "), child(1)));

		// Decl ::= "requires" {InstExpr ","}+ ";"
		tbl.put("Requires/1", vf.list(token("requires"), space(" "), sep(child(0),","), space(" "), token(";")));

		// ProcIdentifier ::= "_=_"
		tbl.put("Assign/0", vf.list(token("_=_")));

		// Var ::= Name ":" Type
		tbl.put("Var/2", vf.list(child(0), space(" "), token(":"), space(" "), child(1)));

		// InstExpr ::= InstExpr "[" {Renaming ","}* "]"
		tbl.put("Renamed/2", vf.list(child(0), space(" "), token("["), space(" "), sep(child(1),","), space(" "), token("]")));

		// Stat ::= "var" Identifier ":" Type ";"
		tbl.put("BaseVarDefT/2", vf.list(token("var"), space(" "), child(0), space(" "), token(":"), space(" "), child(1), space(" "), token(";")));

		// ProcClause ::= "procedure" ProcIdentifier ProcedureParamList
		tbl.put("ProcClause/2", vf.list(token("procedure"), space(" "), child(0), space(" "), child(1)));

		// GuardClause ::= "guard" Expr
		tbl.put("Guard/1", vf.list(token("guard"), space(" "), child(0)));

		// Expr ::= Name
		tbl.put("UserSyntax2/1", vf.list(child(0)));

		// Decl ::= "free" {Type ","}+ "by" InstExpr ";"
		tbl.put("FreeBy/2", vf.list(token("free"), space(" "), sep(child(0),","), space(" "), token("by"), space(" "), child(1), space(" "), token(";")));

		// FilterExpr ::= "remove" InstExpr
		tbl.put("RemoveFilter/1", vf.list(token("remove"), space(" "), child(0)));

		// Stat ::= "print" {Expr ","}* "," ";"
		tbl.put("BasePrint/1", vf.list(token("print"), space(" "), sep(child(0),","), space(" "), token(","), space(" "), token(";")));

		// Stat ::= "let" LetClause* "in" Stat* "end"
		tbl.put("Let/2", vf.list(token("let"), space(" "), child(0), space(" "), token("in"), space(" "), child(1), space(" "), token("end")));

		// Literal ::= HexNumeral
		tbl.put("Hex/1", vf.list(child(0)));

		// InstExpr ::= InstExpr "+" InstExpr
		tbl.put("Plus/2", vf.list(child(0), space(" "), token("+"), space(" "), child(1)));

		// Expr ::= Expr "not" "in" Expr
		tbl.put("NotIn/2", vf.list(child(0), space(" "), token("not"), space(" "), token("in"), space(" "), child(1)));

		// Expr ::= Expr "[" {Expr ","}* "]"
		tbl.put("Index/2", vf.list(child(0), space(" "), token("["), space(" "), sep(child(1),","), space(" "), token("]")));

		// ConsSpec ::= Identifier
		tbl.put("TermCons0/1", vf.list(child(0)));

		// DataRep ::= {ConsSpec ","}* ";"
		tbl.put("TermRep/1", vf.list(sep(child(0),","), space(" "), token(";")));

		// FunClause ::= "function" FunIdentifier FunctionParamList ":" Type
		tbl.put("FunClause/3", vf.list(token("function"), space(" "), child(0), space(" "), child(1), space(" "), token(":"), space(" "), child(2)));

		// FunClause ::= "define" FunName ":" Type
		tbl.put("FunClause/2", vf.list(token("define"), space(" "), child(0), space(" "), token(":"), space(" "), child(1)));

		// Expr ::= "_"
		tbl.put("Undefined/0", vf.list(token("_")));

		// ProcedureParam ::= ParamMode VarIdentifier ":" Type
		tbl.put("Param/3", vf.list(child(0), space(" "), child(1), space(" "), token(":"), space(" "), child(2)));

		// FunctionParam ::= VarIdentifier ":" Type
		tbl.put("Param/2", vf.list(child(0), space(" "), token(":"), space(" "), child(1)));

		// BlockStat ::= "{" Stat* "}"
		tbl.put("Block/1", vf.list(token("{"), space(" "), child(0), space(" "), token("}")));

		// Stat ::= "yield" Expr ";"
		tbl.put("Yield/1", vf.list(token("yield"), space(" "), child(0), space(" "), token(";")));

		// InstExpr ::= "external" ExternalExpr
		tbl.put("External/1", vf.list(token("external"), space(" "), child(0)));

		// Expr ::= Expr LGIMP Expr
		tbl.put("BinOp/3", vf.list(child(0), space(" "), child(1), space(" "), child(2)));

		// ParamMode ::= "del"
		tbl.put("Del/0", vf.list(token("del")));

		// TypeClause ::= "type" TypeIdentifier
		tbl.put("TypeClause/1", vf.list(token("type"), space(" "), child(0)));

		// Literal ::= BinNumeral
		tbl.put("Bin/1", vf.list(child(0)));

		// SingleAlgDecl ::= Modifier* TypeDeclarative SubClause*
		tbl.put("TypeDecl/3", vf.list(child(0), space(" "), child(1), space(" "), child(2)));

		// Modifier ::= "protect"
		tbl.put("ProtectModifier/0", vf.list(token("protect")));

		// AxiomClause ::= "axiom" Identifier FunctionParamList
		tbl.put("AxiomClause/2", vf.list(token("axiom"), space(" "), child(0), space(" "), child(1)));

		// InstExpr ::= "[" {Renaming ","}* "]"
		tbl.put("RenameImpl/1", vf.list(token("["), space(" "), sep(child(0),","), space(" "), token("]")));

		// SingleAlgDecl ::= Modifier* DeclDeclarative SubClause* "=" DeclBody
		tbl.put("DeclDef/4", vf.list(child(0), space(" "), child(1), space(" "), child(2), space(" "), token("="), space(" "), child(3)));

		// Stat ::= "break" ";"
		tbl.put("Break/0", vf.list(token("break"), space(" "), token(";")));

		return tbl;
	}
}
