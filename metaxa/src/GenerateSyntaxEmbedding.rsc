module GenerateSyntaxEmbedding
import IO;

public void generateMetaMagnolia() {
	sorts = ["Expr", "Stat", "Decl", "Type", "Var", "Identifier", "Name", "FunName", "ProcName", "SubClause", "ConceptExpr", "ImplExpr", "Morphism"];
	listSeps = ("Expr":",", "Var":",", "Identifier":",", "Name":",", "Type":",");
	
	generate(|project://MetaXa/src/Meta/MetaMagnoliaEmbedding.sdf|, "Meta/MetaMagnoliaEmbedding", sorts, listSeps);
}

public void generate(loc file, str moduleName, list[str] sorts, map[str,str] listSeps) {
	str prods = "";
	str lexprods = "";
	str lexrestr = "";
	
	for(sort <- sorts) {
		prods += "	\"(;\" <sort>[[SQCtx]] \";)\"				-\>	SQuoted[[Ctx]]	{cons(\"SQ<sort>\"),sortname(\"<sort>\"),quotes(\"single\")}\n";
		prods += "	\"(:\" <sort>[[DQCtx]] \":)\"				-\>	DQuoted[[Ctx]]	{cons(\"DQ<sort>\"),sortname(\"<sort>\"),quotes(\"double\")}\n";
		prods += "	\"(:\" <sort>[[DQCtx]] \":)\"				-\>	<sort>[[SQCtx]]	{cons(\"DQ<sort>\"),sortname(\"<sort>\"),quotes(\"double\")}\n";
		prods += "	\"<sort>\" \"(;\" <sort>[[SQCtx]] \";)\"			-\>	SQuoted[[Ctx]]	{cons(\"SQ<sort>\"),sortname(\"<sort>\"),quotes(\"single\")}\n";
		prods += "	\"<sort>\" \"(:\" <sort>[[DQCtx]] \":)\"			-\>	DQuoted[[Ctx]]	{cons(\"DQ<sort>\"),sortname(\"<sort>\"),quotes(\"double\")}\n";
		prods += "	\"<sort>\" \"(:\" <sort>[[DQCtx]] \":)\"			-\>	<sort>[[SQCtx]]	{cons(\"DQ<sort>\"),sortname(\"<sort>\"),quotes(\"double\")}\n";
		prods += "	\"<sort>\" \"*\" \"(;\" <makelist(sort, "[[SQCtx]]","*",listSeps)> \";)\"		-\>	SQuoted[[Ctx]]	{cons(\"SQ<sort>\"),sortname(\"<sort>\"),quotes(\"single\"),list}\n";
		prods += "	\"<sort>\" \"*\" \"(:\" <makelist(sort, "[[DQCtx]]","*",listSeps)> \":)\"		-\>	DQuoted[[Ctx]]	{cons(\"DQ<sort>\"),sortname(\"<sort>\"),quotes(\"double\"),list}\n";
		prods += "	\"<sort>\" \"*\" \"(:\" <makelist(sort, "[[DQCtx]]","*",listSeps)> \":)\"		-\>	<sort>[[SQCtx]]	{cons(\"DQ<sort>\"),sortname(\"<sort>\"),quotes(\"double\"),list}\n";
		prods += "\n";
		prods += "	<sort>METAID		-\>	<sort>MetaVar\n";
		prods += "	<sort>METAID \"*\"		-\> <sort>MetaVarList\n";
		prods += "	<sort>METAID \"+\"		-\> <sort>MetaVarList\n";
		prods += "	<sort>MetaVar		-\>	<sort>[[SQCtx]]	{cons(\"MetaVar\"), prefer, category(\"MetaVar\")}\n";
		prods += "	<sort>MetaVar		-\>	<sort>[[DQCtx]]	{cons(\"MetaVar\"), prefer, category(\"MetaVar\")}\n";
		prods += "	<sort>MetaVarList		-\>	<sort>[[SQCtx]]	{cons(\"MetaVar\"), prefer, category(\"MetaVar\"),list}\n";
		prods += "	<sort>MetaVarList		-\>	<sort>[[DQCtx]] {cons(\"MetaVar\"), prefer, category(\"MetaVar\"),list}\n";
		prods += "\n";
		prods += "	<sort>METAID		-\> MXaVar {cons(\"<sort>\"), category(\"MetaVar\")}\n";
		prods += "	<sort>METAID		-\> VarToken {cons(\"<sort>\"), category(\"MetaVar\")}\n";
		prods += "	<sort>METAID MXaREP	-\> ListVarToken	{cons(\"<sort>\"),prefer, category(\"MetaVar\")}\n";
		prods += "\n";
		
		lexprods += "	\"<sort>\"[s]*[0-9]*[\\\']*SUBID? -\> <sort>METAID\n";
		lexprods += "	<sort>METAID -\> ID[[SQCtx]] {reject}\n";
		lexprods += "	<sort>METAID -\> ID[[DQCtx]] {reject}\n";
		lexprods += "	<sort>METAID -\> MXaVarID {reject}\n";
		lexrestr += "	<sort>METAID -/- [A-Za-z\\\'\\_0-9]\n";
	}
	prods += "	SQuoted[[Ctx]] -\> Quoted[[Ctx]]\n";
	prods += "	DQuoted[[Ctx]] -\> Quoted[[Ctx]]\n";
	
	str result = "module <moduleName>[Ctx SQCtx DQCtx]\n\nexports\n\ncontext-free syntax\n\n<prods>\nlexical syntax\n\n<lexprods>\nlexical restrictions\n\n<lexrestr>";

	println(result);
	writeFile(file, result);
}

str makelist(str sort, str param, str iter, map[str,str] listSeps) {
	if(sort in listSeps) 
		return "{<sort><param> \"<listSeps[sort]>\"}<iter>";
	else
		return "<sort><param><iter>";
}
