package MiniC.Parser;


import MiniC.Scanner.Token;
import MiniC.Scanner.SourcePos;
import MiniC.Parser.SyntaxError;
import MiniC.Scanner.Scanner;
import MiniC.ErrorReporter;

public class Parser {

    private Scanner scanner;
    private ErrorReporter errorReporter;
    private Token currentToken;

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
    // toplevel parse() routine:
    //
    ///////////////////////////////////////////////////////////////////////////////

    public void parse() {

	currentToken = scanner.scan(); // get first token from scanner...

	try {
	    parseProgram();
	    if (currentToken.kind != Token.EOF) {
		syntaxError("\"%\" not expected after end of program",
			       currentToken.GetLexeme());
	    }
	}
	catch (SyntaxError s) {return; /* to be refined in Assignment 3...*/ }
	return;
    }

    
    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseProgram():
    //
    // program ::= ( (VOID|INT|BOOL|FLOAT) ID ( FunPart | VarPart ) )*
    //
    ///////////////////////////////////////////////////////////////////////////////

    public void parseProgram() throws SyntaxError {
	while (isTypeSpecifier(currentToken.kind)) {
            acceptIt();
	    accept(Token.ID);
	    if(currentToken.kind == Token.LEFTPAREN) {
		parseFunPart();
	    } else {
		parseVarPart();
	    }
	}
    }


    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseFunPart():
    //
    // FunPart ::= ( "(" ParamsList? ")" CompoundStmt )
    //
    ///////////////////////////////////////////////////////////////////////////////

    public void parseFunPart() throws SyntaxError {
        // We already know that the current token is "(".
        // Otherwise use accept() !
        acceptIt();
        if (isTypeSpecifier(currentToken.kind)) {
	    parseParamsList();
	}
	accept(Token.RIGHTPAREN);
	parseCompoundStmt();
    }


    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseParamsList():
    //
    // ParamsList ::= ParamsDecl ( "," ParamsDecl ) *
    //
    ///////////////////////////////////////////////////////////////////////////////

    public void parseParamsList() throws SyntaxError {
	// to be completed by you...
		parseParamsDecl();		
		while(currentToken.kind == Token.COMMA) {
			acceptIt();
			parseParamsDecl();
		}
    } 


    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseCompoundStmt():
    //
    // CompoundStmt ::= "{" VariableDefinition* Stmt* "}"
    //
    ///////////////////////////////////////////////////////////////////////////////

    public void parseCompoundStmt() throws SyntaxError {
	// to be completed by you...
		accept(Token.LEFTBRACE);
		while(isTypeSpecifier(currentToken.kind))
			parseVarDef();
		while(isStmt(currentToken.kind))
			parseStmt();
		accept(Token.RIGHTBRACE);
    } 


    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseVarPart():
    //
    // VarPart ::= ( "[" INTLITERAL "]" )?  ( "=" initializer ) ? ( "," init_decl)* ";"
    //
    ///////////////////////////////////////////////////////////////////////////////

    public void parseVarPart() throws SyntaxError {
	// to be completed by you...
		if(currentToken.kind == Token.LEFTBRACKET) {
			acceptIt();
			accept(Token.INTLITERAL);
			accept(Token.RIGHTBRACKET);
		}
		if(currentToken.kind == Token.ASSIGN) {
			acceptIt();
			parseInit();
		}

		while(currentToken.kind == Token.COMMA) {
			acceptIt();
			parseDec();
			if(currentToken.kind == Token.ASSIGN) {
				acceptIt();
				parseInit();
			}
		}
		accept(Token.SEMICOLON);

    }
	public void parseParamsDecl() throws SyntaxError {
		if(isTypeSpecifier(currentToken.kind)) {
			acceptIt();
		}
		parseDec();
	}

	public void parseVarDef() throws SyntaxError {
		if(isTypeSpecifier(currentToken.kind)) {
			acceptIt();
		}
		accept(Token.ID);
		parseVarPart();
	}

	public void parseIf() throws SyntaxError {
		accept(Token.IF);
		accept(Token.LEFTPAREN);
		parseExpr();
		accept(Token.RIGHTPAREN);
		parseStmt();
		if(currentToken.kind == Token.ELSE) {
			acceptIt();
			parseStmt();
		}
	}

	public void parseWhile() throws SyntaxError {
		accept(Token.WHILE);
		accept(Token.LEFTPAREN);
		parseExpr();
		accept(Token.RIGHTPAREN);
		parseStmt();
	}
	
	public void parseFor() throws SyntaxError {
		accept(Token.FOR);
		accept(Token.LEFTPAREN);
		if(currentToken.kind == Token.ID) {
			acceptIt();
			accept(Token.ASSIGN);
			parseExpr();
		}
		accept(Token.SEMICOLON);
		if(isExpr(currentToken.kind)) {
			parseExpr();
		}
		accept(Token.SEMICOLON);
		if(currentToken.kind == Token.ID) {
			acceptIt();
			accept(Token.ASSIGN);
			parseExpr();
		}
		accept(Token.RIGHTPAREN);
		parseStmt();
	}

	public void parseExpr() throws SyntaxError {
		orExpr();
	}

	public void parseInit() throws SyntaxError {
		if(isExpr(currentToken.kind)) {
			parseExpr();
		}
		else if(currentToken.kind == Token.LEFTBRACE) {
			acceptIt();
		 	parseExpr();
			while(currentToken.kind == Token.COMMA) {
				acceptIt();
				parseExpr();
			}
			accept(Token.RIGHTBRACE);
		}	
	}
	
	public void parseDec() throws SyntaxError {
		accept(Token.ID);
		if(currentToken.kind == Token.LEFTBRACKET) {
			acceptIt();
			accept(Token.INTLITERAL);
			accept(Token.RIGHTBRACKET);
		}
	}
	
	public void parseStmt() throws SyntaxError {
		switch(currentToken.kind) {
			case Token.LEFTBRACE :
				parseCompoundStmt();
				break;
			case Token.IF :
				parseIf();
				break;
			case Token.WHILE :
				parseWhile();
				break;
			case Token.FOR :
				parseFor();
				break;
			case Token.RETURN :
				acceptIt();
				if(isExpr(currentToken.kind)) {
					parseExpr();
				}
				accept(Token.SEMICOLON);
				break;
			case Token.ID :
				acceptIt();
				if(currentToken.kind == Token.LEFTPAREN) {
					acceptIt();
					if(isExpr(currentToken.kind)) {
						parseExpr();
						while(currentToken.kind == Token.COMMA) {
							acceptIt();
							parseExpr();
						}
					}
					accept(Token.RIGHTPAREN);
				}
				else {
					if(currentToken.kind==Token.LEFTBRACKET) {
						acceptIt();
						parseExpr();
						accept(Token.RIGHTBRACKET);
					}
					accept(Token.ASSIGN);
					parseExpr();
				}
				accept(Token.SEMICOLON);
				break;
			default:
				break;
		}
	}

	public void orExpr() throws SyntaxError {
		andExpr();
		while(currentToken.kind == Token.OR) {
			acceptIt();
			andExpr();
		}
	}

	public void andExpr() throws SyntaxError {
		relExpr();
		while(currentToken.kind == Token.AND) {
			acceptIt();
			relExpr();
		}
	}

	public void relExpr() throws SyntaxError {
		addExpr();
		if(currentToken.kind == Token.EQ ||
				currentToken.kind == Token.NOTEQ ||
				currentToken.kind == Token.LESS ||
				currentToken.kind == Token.LESSEQ ||
				currentToken.kind == Token. GREATER ||
				currentToken.kind == Token.GREATEREQ) {
			acceptIt();
			addExpr();
		}

	}

	public void addExpr() throws SyntaxError {
		multExpr();
		while(currentToken.kind == Token.PLUS ||
				currentToken.kind ==Token.MINUS) {
			acceptIt();
			multExpr();
		}
	}

	public void multExpr() throws SyntaxError {
		unaryExpr();
		while(currentToken.kind == Token.TIMES ||
				currentToken.kind == Token.DIV) {
			acceptIt();
			unaryExpr();
		}
	}

	public void unaryExpr() throws SyntaxError {
		while(currentToken.kind == Token.PLUS ||
				currentToken.kind == Token.MINUS ||
				currentToken.kind == Token.NOT) {
			acceptIt();
		}
		primaryExpr();
	}

	public void primaryExpr() throws SyntaxError {
		switch(currentToken.kind) {
			case Token.ID :
				acceptIt();
				if(currentToken.kind == Token.LEFTPAREN) {
					acceptIt();
					if(isExpr(currentToken.kind)) {
						parseExpr();
						while(currentToken.kind == Token.COMMA) {
							acceptIt();
							parseExpr();
						}
					}
					accept(Token.RIGHTPAREN);
				}
				if(currentToken.kind == Token.LEFTBRACKET) {
					acceptIt();
					parseExpr();
					accept(Token.RIGHTBRACKET);
				}
				break;
			case Token.LEFTPAREN :
				acceptIt();
				parseExpr();
				accept(Token.RIGHTPAREN);
				break;
			case Token.INTLITERAL :
				acceptIt();
				break;
			case Token.BOOLLITERAL :
				acceptIt();
				break;
			case Token.FLOATLITERAL :
				acceptIt();
				break;
			case Token.STRINGLITERAL :
				acceptIt();
				break;
			default :
				break;
		}
	}
    // to be completed by you...

}
