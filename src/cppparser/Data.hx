package cppparser;

import cppparser.Data.ExprDef;
import hxparse.Position;

enum Keyword {
	KwdClass;
	KwdTypedef;
	KwdEnum;
	
	KwdTrue;
	KwdFalse;
	
	KwdIf;
	KwdElse;
	KwdWhile;
	KwdDo;
	KwdFor;
	
	KwdBreak;
	KwdContinue;
	KwdReturn;
	
	KwdSwitch;
	KwdCase;
	KwdDefault;
	
	KwdPublic;
	KwdProtected;
	KwdPrivate;
	
	KwdStatic;
	
	KwdConst;
	KwdVirtual;
	KwdExtern;
	KwdFriend;
	
	KwdSigned;
	KwdUnsigned;
	
	KwdNew;
}

enum Constant {
	CInt(v:String);
	CLong(v:String);
	CFloat(f:String);
	CString(s:String);
	CIdent(s:String);
}

enum Unop {
	OpIncrement; // ++
	OpDecrement; // --
	OpNot;       // !
	OpTilde;     // ~
}

enum Binop {
	OpAdd;     // +
	OpMult;    // *
	OpDiv;     // /
	OpSub;     // -
	OpAssign;  // =
	OpEq;      // ==
	OpNotEq;   // !=
	OpGt;      // >
	OpGte;     // >=
	OpLt;      // <
	OpLte;     // <=
	OpAnd;     // &
	OpOr;      // |
	OpXor;     // ^
	OpBoolAnd; // &&
	OpBoolOr;  // ||
	OpShl;     // <<
	OpShr;     // >>
	OpUShr;    // >>>
	OpMod;     // %
	OpAssignOp(op:Binop); // op=
	
	OpPreConcat; // ##
}

enum TokenDef {
	Kwd(k:Keyword);
	Const(c:Constant);
	
	Sharp(s:String);
	//Define(key:String, value:String);
	Pragma(s:String);
	Include(file:String, system:Bool);
	
	Unop(op:Unop);
	Binop(op:Binop);
	
	Comment(s:String);
	CommentLine(s:String);
	
	Semicolon;
	Comma;
	Dot;
	Colon;
	DoubleColon;
	Question;
	
	Arrow;
	
	BkOpen;
	BkClose;
	BrOpen;
	BrClose;
	POpen;
	PClose;
	
	Newline;
	
	Eof;
}

class Token {
	public var tok:TokenDef;
	public var pos:Position;

	public function new(tok, pos) {
		this.tok = tok;
		this.pos = pos;
	}

	public function toString() {
		return TokenDefPrinter.print(tok);
	}
}

enum ExprDef {
	EConst(c:Constant);
	EBinop(op:Binop, a:Expr, b:Expr);
	EParenthesis(e:Expr);
	EUnop(op:Unop, postFix:Bool, e:Expr);
	ECall(e:Expr, params:Array<Expr>);
}

typedef Expr = {
	expr:ExprDef,
	pos:Position
}

class TokenDefPrinter {
	static public function printUnop(op:Unop) return switch(op) {
		case OpIncrement: "++";
		case OpDecrement: "--";
		case OpNot:       "!";
		case OpTilde:     "~";
	}
	static public function printBinop(op:Binop) return switch(op) {
		case OpAdd:     "+";
		case OpMult:    "*";
		case OpDiv:     "/";
		case OpSub:     "-";
		case OpAssign:  "=";
		case OpEq:      "==";
		case OpNotEq:   "!=";
		case OpGt:      ">";
		case OpGte:     ">=";
		case OpLt:      "<";
		case OpLte:     "<=";
		case OpAnd:     "&";
		case OpOr:      "|";
		case OpXor:     "^";
		case OpBoolAnd: "&&";
		case OpBoolOr:  "||";
		case OpShl:     "<<";
		case OpShr:     ">>";
		case OpUShr:    ">>>";
		case OpMod:     "%";
		case OpAssignOp(op): printBinop(op) + "=";
		case OpPreConcat: "##";
	}
	static public function print(def:TokenDef) {
		return switch(def) {
			case Kwd(k): k.getName().substr(3).toLowerCase();
			
			case Const(CInt(s) | CLong(s) | CFloat(s) | CIdent(s)): s;
			case Const(CString(s)): '"$s"';
			
			case Sharp(s): '#$s';
			//case Define(k, v): '#define $k $v';
			case Pragma(s): '#pragma $s';
			case Include(s, false): '#include "$s"';
			case Include(s, true): '#include <$s>';
			
			case Unop(op): printUnop(op);
			case Binop(op): printBinop(op);
			
			case Comment(s): '/*$s/*';
			case CommentLine(s): '//$s';
			
			case Semicolon: ";";
			case Comma: ",";
			case Dot: ".";
			case Colon: ":";
			case DoubleColon: "::";
			case Question: "?";
			
			case Arrow: "->";
			
			case BkOpen: "[";
			case BkClose: "]";
			case BrOpen: "{";
			case BrClose: "}";
			case POpen: "(";
			case PClose: ")";
			
			case Newline: "\n";
			
			case Eof: "<eof>";
		}
	}
}