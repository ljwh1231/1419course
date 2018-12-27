package MiniC.Parser;

import MiniC.Scanner.Scanner;
import MiniC.Scanner.Token;
import MiniC.Scanner.SourcePos;
import MiniC.Parser.SyntaxError;
import MiniC.ErrorReporter;
import MiniC.AstGen.*;


public class Parser {

    private Scanner scanner;
    private ErrorReporter errorReporter;
    private Token currentToken;
    private SourcePos previousTokenPosition;

    public Parser(Scanner lexer, ErrorReporter reporter) {
	scanner = lexer;
        errorReporter = reporter;
    }

    // accept() checks whether the current token matches tokenExpected.
    // If so, it fetches the next token.
    // If not, it reports a syntax error.
    void accept (int tokenExpected) throws SyntaxError {
	if (currentToken.kind == tokenExpected) {
            previousTokenPosition = currentToken.GetSourcePos();
	    currentToken = scanner.scan();
	} else {
	    syntaxError("\"%\" expected here", Token.spell(tokenExpected));
	}
    }

    // acceptIt() unconditionally accepts the current token
    // and fetches the next token from the scanner.
    void acceptIt() {
        previousTokenPosition = currentToken.GetSourcePos();
	currentToken = scanner.scan();
    }

    // start records the position of the start of a phrase.
    // This is defined to be the position of the first
    // character of the first token of the phrase.
    void start(SourcePos position) {
        position.StartCol = currentToken.GetSourcePos().StartCol;
        position.StartLine = currentToken.GetSourcePos().StartLine;
    }

    // finish records the position of the end of a phrase.
    // This is defined to be the position of the last
    // character of the last token of the phrase.
    void finish(SourcePos position) {
        position.EndCol = previousTokenPosition.EndCol;
        position.EndLine = previousTokenPosition.EndLine;
    }

    void syntaxError(String messageTemplate, String tokenQuoted) throws SyntaxError {
	SourcePos pos = currentToken.GetSourcePos();
	errorReporter.reportError(messageTemplate, tokenQuoted, pos);
	throw(new SyntaxError());
    }

    boolean isTypeSpecifier(int token) {
	if(token == Token.VOID ||
           token == Token.INT  ||
           token == Token.BOOL ||
           token == Token.FLOAT) {
	    return true;
	} else {
	    return false;
	}
    }

	boolean isUnaryExpr(int token) {
		if(token == Token.PLUS ||
			token == Token.MINUS ||
			token == Token.NOT) {
			return true;
		} else {
			return false;
		}
	}

	boolean isExpr(int token) {
		if(isUnaryExpr(token) ||
			token == Token.ID ||
			token == Token.LEFTPAREN ||
			token == Token.INTLITERAL ||
			token == Token.BOOLLITERAL ||
			token == Token.FLOATLITERAL ||
			token == Token.STRINGLITERAL) {
			return true;
		} else {
			return false;
		}
	}

	boolean isStmt(int token) {
		if(token == Token.LEFTBRACE ||
			token == Token.IF ||
			token == Token.WHILE ||
			token == Token.FOR ||
			token == Token.RETURN ||
			token == Token.ID) {
			return true;
		} else {
			return false;
		}
	}


    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseArrayIndexDecl (Type T):
    //
    // Take [INTLITERAL] and generate an ArrayType
    //
    ///////////////////////////////////////////////////////////////////////////////

    public ArrayType parseArrayIndexDecl(Type T) throws SyntaxError {
	IntLiteral L;
	IntExpr IE;
	accept(Token.LEFTBRACKET);
        SourcePos pos = currentToken.GetSourcePos();
	L = new IntLiteral(currentToken.GetLexeme(), pos);
	accept(Token.INTLITERAL);
	accept(Token.RIGHTBRACKET);
	IE = new IntExpr (L, pos);
	return new ArrayType (T, IE, previousTokenPosition);
    }


    ///////////////////////////////////////////////////////////////////////////////
    //
    // toplevel parse() routine:
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Program parse() {

        Program ProgramAST = null;

        previousTokenPosition = new SourcePos();
        previousTokenPosition.StartLine = 0;
        previousTokenPosition.StartCol = 0;
        previousTokenPosition.EndLine = 0;
        previousTokenPosition.EndCol = 0;

	currentToken = scanner.scan(); // get first token from scanner...

	try {
            ProgramAST = parseProgram();
	    if (currentToken.kind != Token.EOF) {
		syntaxError("\"%\" not expected after end of program",
			       currentToken.GetLexeme());
	    }
	}
	catch (SyntaxError s) { return null; }
	return ProgramAST;
    }

    
    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseProgram():
    //
    // program ::= ( (VOID|INT|BOOL|FLOAT) ID ( FunPart | VarPart ) )* ";"
    //
    ///////////////////////////////////////////////////////////////////////////////

    // parseProgDecls: recursive helper function to facilitate AST construction.
    public Decl parseProgDecls () throws SyntaxError {
        if (! isTypeSpecifier(currentToken.kind)) {
           return new EmptyDecl (previousTokenPosition);
        }
        SourcePos pos = new SourcePos();
        start(pos);
        Type T = parseTypeSpecifier();
		ID Ident = parseID();
		if(currentToken.kind == Token.LEFTPAREN) {
	   		Decl newD = parseFunPart(T, Ident, pos);
           	return new DeclSequence (newD, parseProgDecls(), previousTokenPosition);
		} else {
           	DeclSequence Vars = parseVarPart(T, Ident);
           	DeclSequence VarsTail = Vars.GetRightmostDeclSequenceNode();
           	Decl RemainderDecls = parseProgDecls();
           	VarsTail.SetRightSubtree (RemainderDecls);
           	return Vars;
		}
    }

    public Program parseProgram() throws SyntaxError {
        SourcePos pos = new SourcePos();
        start(pos);
        Decl D = parseProgDecls();
        finish(pos);
        Program P = new Program (D, pos);
        return P;
    }


    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseFunPart():
    //
    // FunPart ::= ( "(" ParamsList? ")" CompoundStmt )
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Decl parseFunPart(Type T, ID Ident, SourcePos pos) throws SyntaxError {

        // We already know that the current token is "(".
        // Otherwise use accept() !
        acceptIt();
		Decl PDecl = parseParamsList(); // can also be empty...
		accept(Token.RIGHTPAREN);
		CompoundStmt CStmt = parseCompoundStmt();
        finish(pos);
		return new FunDecl (T, Ident, PDecl, CStmt, pos);
    }


    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseParamsList():
    //
    // ParamsList ::= ParameterDecl ( "," ParameterDecl ) *
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Decl parseParamsList() throws SyntaxError {
        if (!isTypeSpecifier(currentToken.kind)) {
           return new EmptyFormalParamDecl(previousTokenPosition);
        }
        Decl PDecl = parseParameterDecl();
		if (currentToken.kind == Token.COMMA) {
            acceptIt();
        }
        return new FormalParamDeclSequence (PDecl,
                        parseParamsList(), previousTokenPosition);
    } 


    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseParameterDecl():
    //
    // ParameterDecl ::= (VOID|INT|BOOL|FLOAT) Declarator
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Decl parseParameterDecl() throws SyntaxError {
        Type T = null;
        Decl D = null;

        SourcePos pos = new SourcePos();
        start(pos);
	if (isTypeSpecifier(currentToken.kind)) {
	    T = parseTypeSpecifier();
	} else {
	    syntaxError("Type specifier instead of % expected",
			Token.spell(currentToken.kind));
	}
	D = parseDeclarator(T, pos);
	return D;
    }


    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseDeclarator():
    //
    // Declarator ::= ID ( "[" INTLITERAL "]" )?
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Decl parseDeclarator(Type T, SourcePos pos) throws SyntaxError {
	ID Ident = parseID();
	if (currentToken.kind == Token.LEFTBRACKET) {
	    ArrayType ArrT = parseArrayIndexDecl(T);
            finish(pos);
	    return new FormalParamDecl (ArrT, Ident, pos);
	}
        finish(pos);
	return new FormalParamDecl (T, Ident, pos);
    }


    //////////////////////////////////
    //
    // parseVarPart():
    //
    // VarPart ::= ( "[" INTLITERAL "]" )?  ( "=" initializer ) ? ( "," init_decl)* ";"
    //
    ///////////////////////////////////////////////////////////////////////////////

    public DeclSequence parseVarPart(Type T, ID Ident) throws SyntaxError {
	Type theType = T;
        Decl D;
        DeclSequence Seq = null;
        Expr E = new EmptyExpr(previousTokenPosition);
	if (currentToken.kind == Token.LEFTBRACKET) {
	    theType = parseArrayIndexDecl(T);
	}
	if (currentToken.kind == Token.ASSIGN) {
	    acceptIt();
		//You can use the following code after implementing of parseInitializer();
	    E = parseInitializer();
	}
        D = new VarDecl (theType, Ident, E, previousTokenPosition);
        // You can use the following code after implementatin of parseInitDecl():
	if (currentToken.kind == Token.COMMA) {
	    acceptIt();
	    Seq = new DeclSequence (D, parseInitDecl(T), previousTokenPosition);
	} else {
        Seq = new DeclSequence (D, new EmptyDecl (previousTokenPosition),
                                     previousTokenPosition);
    }
	accept (Token.SEMICOLON);
	return Seq;
    }




    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseUnaryExpr():
    //
    // UnaryExpr ::= ("+"|"-"|"!")* PrimaryExpr
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Expr parseUnaryExpr() throws SyntaxError {
	while (currentToken.kind == Token.PLUS ||
               currentToken.kind == Token.MINUS ||
               currentToken.kind == Token.NOT) {
	    Operator opAST = new Operator (currentToken.GetLexeme(),
					   previousTokenPosition);
	    acceptIt();
	    return new UnaryExpr (opAST, parseUnaryExpr(), previousTokenPosition);
	}
	return parsePrimaryExpr();
    }


    ///////////////////////////////////////////////////////////////////////////////
    //
    // parsePrimaryExpr():
    //
    // PrimaryExpr ::= ID arglist?
    //              |  ID "[" expr "]"
    //              |  "(" expr ")"
    //              |  INTLITERAL | BOOLLITERAL | FLOATLITERAL | STRINGLITERAL
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Expr parsePrimaryExpr() throws SyntaxError {
		Expr retExpr = null;
		switch(currentToken.kind) {
			case Token.ID:
				ID Ident = parseID();
				if(currentToken.kind == Token.LEFTPAREN) {
					retExpr = new CallExpr(Ident, parseArgList(), previousTokenPosition);
				}
				else if(currentToken.kind == Token.LEFTBRACKET) {
					acceptIt();
					VarExpr ve = new VarExpr(Ident, previousTokenPosition);
					retExpr = new ArrayExpr(ve, parseExpr(), previousTokenPosition);
					accept(Token.RIGHTBRACKET);
				} else {
					retExpr = new VarExpr(Ident, previousTokenPosition);
				}
				break;
			case Token.LEFTPAREN :
				acceptIt();
				retExpr = parseExpr();
				accept(Token.RIGHTPAREN);
				break;
			case Token.INTLITERAL :
				retExpr = new IntExpr(new IntLiteral(currentToken.GetLexeme(), previousTokenPosition), previousTokenPosition);
				acceptIt();
				break;
			case Token.BOOLLITERAL :
				retExpr = new BoolExpr(new BoolLiteral(currentToken.GetLexeme(), previousTokenPosition), previousTokenPosition);
				acceptIt();
				break;
			case Token.FLOATLITERAL :
				retExpr = new FloatExpr(new FloatLiteral(currentToken.GetLexeme(), previousTokenPosition), previousTokenPosition);
				acceptIt();
				break;
			case Token.STRINGLITERAL :
				retExpr = new StringExpr(new StringLiteral(currentToken.GetLexeme(), previousTokenPosition), previousTokenPosition);
				acceptIt();
				break;
			default :
				break;
		}				
        // your code goes here...

		return retExpr;
    }


    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseCompoundStmt():
    //
    // CompoundStmt ::= "{" VariableDef* Stmt* "}"
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Decl parseCompoundDecls () throws SyntaxError {
        if (!isTypeSpecifier(currentToken.kind)) {
           return new EmptyDecl (previousTokenPosition);
        }
        Type T = parseTypeSpecifier();
	ID Ident = parseID();
        DeclSequence Vars = parseVarPart(T, Ident);
        DeclSequence VarsTail = Vars.GetRightmostDeclSequenceNode();
        Decl RemainderDecls = parseCompoundDecls();
        VarsTail.SetRightSubtree (RemainderDecls);
        return Vars;       
    }

    public Stmt parseCompoundStmts () throws SyntaxError {
	if (! (currentToken.kind == Token.LEFTBRACE ||
               currentToken.kind == Token.IF ||
               currentToken.kind == Token.WHILE ||
               currentToken.kind == Token.FOR ||
               currentToken.kind == Token.RETURN ||
               currentToken.kind == Token.ID)
	    ) {
	    return new EmptyStmt(previousTokenPosition);
	}
        Stmt S = null;
        // You can use the following code after implementation of parseStmt():
        S = parseStmt();
        return new StmtSequence (S, parseCompoundStmts(), previousTokenPosition);
    }

    public CompoundStmt parseCompoundStmt() throws SyntaxError {
        SourcePos pos = new SourcePos();
        start(pos);
	accept(Token.LEFTBRACE);
        Decl D = parseCompoundDecls();
        Stmt S = parseCompoundStmts();
	accept(Token.RIGHTBRACE);
        finish(pos);
        if ( (D.getClass() == EmptyDecl.class) &&
             (S.getClass() == EmptyStmt.class)) {
           return new EmptyCompoundStmt (previousTokenPosition);
        } else {
	   return new CompoundStmt (D, S, pos);
        }
    }



    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseArgList():
    //
    // ArgList ::= "(" ( arg ( "," arg )* )? ")"
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Expr parseArgs() throws SyntaxError {
		if (currentToken.kind == Token.RIGHTPAREN) {
	    	return new  EmptyActualParam (previousTokenPosition);
        	} 
		Expr Params = null;
  		//You can use the following code after you have implemented parseExpr() aso.:
        Params = new ActualParam (parseExpr(), previousTokenPosition);
		if (currentToken.kind == Token.COMMA) {
			acceptIt();
        }
		return new ActualParamSequence (Params, parseArgs(), previousTokenPosition);
    }

    public Expr parseArgList() throws SyntaxError {
	accept(Token.LEFTPAREN);
        Expr Params = parseArgs();
	accept(Token.RIGHTPAREN);
	return Params;
    }


    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseID():
    //
    // ID (terminal)
    //
    ///////////////////////////////////////////////////////////////////////////////

    public ID parseID() throws SyntaxError {
	ID Ident = new ID(currentToken.GetLexeme(), currentToken.GetSourcePos());
	accept(Token.ID);
	return Ident;
    }


    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseTypeSpecifier():
    //
    // VOID | INT | FLOAT | BOOL (all terminals)
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Type parseTypeSpecifier() throws SyntaxError {
	Type T = null;
	switch (currentToken.kind) {
	case Token.INT:
	    T = new IntType(currentToken.GetSourcePos());
	    break;
	case Token.FLOAT:
	    T = new FloatType(currentToken.GetSourcePos());
	    break;
	case Token.BOOL:
	    T = new BoolType(currentToken.GetSourcePos());
	    break;
	case Token.VOID:
	    T = new VoidType(currentToken.GetSourcePos());
	    break;
	default:
	    syntaxError("Type specifier expected", "");
	}
	acceptIt();
	return T;
    }

	public Expr recursiveParser() throws SyntaxError {
		Expr retExpr = new ExprSequence(parseExpr(), new EmptyExpr(previousTokenPosition), previousTokenPosition);
		if(currentToken.kind == Token.COMMA) {
			acceptIt();
			retExpr = new ExprSequence(retExpr, recursiveParser(), previousTokenPosition);
		}
		return retExpr;
    }

	public Expr parseInitializer() throws SyntaxError {
		Expr retExpr = null;
		if(currentToken.kind != Token.LEFTBRACE) {
			retExpr = parseExpr();
		} else {
			acceptIt();
			retExpr = recursiveParser();
			accept(Token.RIGHTBRACE);
		}
		return retExpr;
	}

	public Expr parseExpr() throws SyntaxError {
		Expr retExpr = null;
		retExpr = parseOr();
		return retExpr;
	}
	public Expr parseOr() throws SyntaxError {
		Expr retExpr = parseAnd();
		while(currentToken.kind == Token.OR) {
			Operator opAST = new Operator(currentToken.GetLexeme(), previousTokenPosition);
			acceptIt();
			retExpr = new BinaryExpr(retExpr, opAST, parseAnd(), previousTokenPosition);
		}
		return retExpr;
	}
	
	public Expr parseAnd() throws SyntaxError {
		Expr retExpr = parseRel();
		while(currentToken.kind == Token.AND) {
			Operator opAST = new Operator(currentToken.GetLexeme(), previousTokenPosition);
			acceptIt();
			retExpr = new BinaryExpr(retExpr, opAST, parseRel(), previousTokenPosition);
		}
		return retExpr;
	}

	public Expr parseRel() throws SyntaxError {
		Expr retExpr = parseAdd();
		while(currentToken.kind == Token.EQ ||
				currentToken.kind == Token.NOTEQ ||
				currentToken.kind == Token.LESS ||
				currentToken.kind == Token.LESSEQ ||
				currentToken.kind == Token.GREATER ||
				currentToken.kind == Token.GREATEREQ) {
			Operator opAST = new Operator(currentToken.GetLexeme(), previousTokenPosition);
			acceptIt();
			retExpr = new BinaryExpr(retExpr, opAST, parseAdd(), previousTokenPosition);
		}
		return retExpr;
	}

	public Expr parseAdd() throws SyntaxError {
		Expr retExpr = parseMult();
		while(currentToken.kind == Token.PLUS ||
				currentToken.kind == Token.MINUS) {
			Operator opAST = new Operator(currentToken.GetLexeme(), previousTokenPosition);
			acceptIt();
			retExpr = new BinaryExpr(retExpr, opAST, parseMult(), previousTokenPosition);
		}
		return retExpr;
	}

	public Expr parseMult() throws SyntaxError {
		Expr retExpr = parseUnaryExpr();
		while(currentToken.kind == Token.TIMES ||
				currentToken.kind == Token.DIV) {
			Operator opAST = new Operator(currentToken.GetLexeme(), previousTokenPosition);
			acceptIt();
			retExpr = new BinaryExpr(retExpr, opAST, parseUnaryExpr(), previousTokenPosition);
		}
		return retExpr;
	}

	public Stmt parseStmt() throws SyntaxError {
		Stmt s = null;
		switch(currentToken.kind) {
			case Token.LEFTBRACE :
				s = parseCompoundStmt();
				break;
			case Token.IF :
				s = parseIf();
				break;
			case Token.WHILE :
				s = parseWhile();
				break;
			case Token.FOR :
				s = parseFor();
				break;
			case Token.RETURN :
				acceptIt();
				Expr e = null;
				if(currentToken.kind == Token.SEMICOLON) {
					e = new EmptyExpr(previousTokenPosition);
					acceptIt();
				} else {
					e = parseExpr();
					accept(Token.SEMICOLON);
				}
				s = new ReturnStmt(e, previousTokenPosition);
				break;
			case Token.ID :
				ID Ident = parseID();
				Expr ex = null;
				if(currentToken.kind == Token.LEFTPAREN) {
					ex = parseArgList();
					s = new CallStmt(new CallExpr(Ident, ex, previousTokenPosition), previousTokenPosition);
					accept(Token.SEMICOLON);
				} else {
					ex = new VarExpr(Ident, previousTokenPosition);
					if(currentToken.kind == Token.LEFTBRACKET) {
						acceptIt();
						ex = new ArrayExpr(ex, parseExpr(), previousTokenPosition);
						accept(Token.RIGHTBRACKET);
					}
					accept(Token.ASSIGN);
					s = new AssignStmt(ex, parseExpr(), previousTokenPosition);
					accept(Token.SEMICOLON);
				}
				break;
			default :
				break;
		}
		return s;		
	}

	public Stmt parseIf() throws SyntaxError {
		Expr e = null;
		Stmt s = null;
		accept(Token.IF);
		accept(Token.LEFTPAREN);
		e = parseExpr();
		accept(Token.RIGHTPAREN);
		s = parseStmt();
		if(currentToken.kind != Token.ELSE) {
			return new IfStmt(e, s, previousTokenPosition);
		} else {
			acceptIt();
			s = new IfStmt(e, s, parseStmt(), previousTokenPosition);
			return s;
		}
	}

	public Stmt parseWhile() throws SyntaxError {
		Expr e = null;
		Stmt s = null;
		accept(Token.WHILE);
		accept(Token.LEFTPAREN);
		e = parseExpr();
		accept(Token.RIGHTPAREN);
		s = new WhileStmt(e, parseStmt(), previousTokenPosition);
		return s;
	}
	
	public Stmt parseFor() throws SyntaxError {
		Expr e1, e2, e3;
		Stmt s;
		accept(Token.FOR);
		accept(Token.LEFTPAREN);
		if(isExpr(currentToken.kind)) {
			Expr temp = parseExpr();
			accept(Token.ASSIGN);
			e1 = new AssignExpr(temp, parseExpr(), previousTokenPosition);
		} else {
			e1 = new EmptyExpr(previousTokenPosition);
		}
		accept(Token.SEMICOLON);
		if(isExpr(currentToken.kind)) {
			e2 = parseExpr();
		} else {
			e2 = new EmptyExpr(previousTokenPosition);
		}
		accept(Token.SEMICOLON);
		if(isExpr(currentToken.kind)) {
			Expr temp = parseExpr();
			accept(Token.ASSIGN);
			e3 = new AssignExpr(temp, parseExpr(), previousTokenPosition);
		} else {
			e3 = new EmptyExpr(previousTokenPosition);
		}
		accept(Token.RIGHTPAREN);
		s = new ForStmt(e1, e2, e3, parseStmt(), previousTokenPosition);
		return s;
	}

	public Decl parseInitDecl(Type T) throws SyntaxError {
		FormalParamDecl d = null;
		Expr e = new EmptyExpr(previousTokenPosition);
		SourcePos pos = new SourcePos();
		ID Ident = parseID();
		if(currentToken.kind == Token.LEFTBRACKET) {
			ArrayType ArrT = parseArrayIndexDecl(T);
			finish(pos);
			d = new FormalParamDecl(ArrT, Ident, pos);
		} else {
			finish(pos);
			d = new FormalParamDecl(T, Ident, pos);
		}
		if(currentToken.kind == Token.ASSIGN) {
			acceptIt();
			e = parseInitializer();
		}
		if (currentToken.kind == Token.COMMA) {
			acceptIt();
			return new DeclSequence(new VarDecl(d.astType, d.astIdent, e, previousTokenPosition), parseInitDecl(T), previousTokenPosition);
		}
		return new DeclSequence(new VarDecl(d.astType, d.astIdent, e, previousTokenPosition), new EmptyDecl(previousTokenPosition), previousTokenPosition);
	}
}
