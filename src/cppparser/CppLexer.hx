package cppparser;

import haxe.io.Eof;
import hxparse.Lexer;
import hxparse.Position;
import hxparse.RuleBuilder;

import cppparser.Data;

using StringTools;

enum LexerErrorMsg {
	UnterminatedString;
	UnterminatedRegExp;
	UnclosedComment;
	DefineMissingIdentifier;
}
 
class LexerError {
	public var msg:LexerErrorMsg;
	public var pos:Position;
	public function new(msg, pos) {
		this.msg = msg;
		this.pos = pos;
	}
}
 
class CppLexer extends Lexer implements RuleBuilder {
	
	static function mk(lexer:Lexer, tokenDef:TokenDef) {
		return new Token(tokenDef, lexer.curPos());
	}
	
	static var keywords = @:mapping(3) Data.Keyword;
	
	static var buf = new StringBuf();
	
	static var ident = "_*[a-z][a-zA-Z0-9_]*|_+|_+[0-9][_a-zA-Z0-9]*";
	static var idtype = "_*[A-Z][a-zA-Z0-9_]*";
	static var backslashSkip = "\\\\\r?\n";
	
	static public var tok = @:rule [
		"" => mk(lexer, Eof),
		
		// Whitespace
		"[\r\n\t ]" => lexer.token(tok),
		
		backslashSkip => lexer.token(tok),
		
		// Preprocessor concatenation
		" *## *" => mk(lexer, Binop(OpPreConcat)),
		
		// Numbers
		"-?0x[0-9a-fA-F]+" => mk(lexer, Const(CInt(lexer.current))),
		"-?[0-9]+" => mk(lexer, Const(CInt(lexer.current))),
		"-?[0-9]+[Ll]" => mk(lexer, Const(CLong(lexer.current))),
		"-?[0-9]+\\.[0-9]+" => mk(lexer, Const(CFloat(lexer.current))),
		"-?\\.[0-9]+" => mk(lexer, Const(CFloat(lexer.current))),
		"-?[0-9]+[eE][\\+\\-]?[0-9]+" => mk(lexer,Const(CFloat(lexer.current))),
		"-?[0-9]+\\.[0-9]*[eE][\\+\\-]?[0-9]+" => mk(lexer,Const(CFloat(lexer.current))),
		
		// Comments
		"//[^\n\r]*" => mk(lexer, CommentLine(lexer.current.substr(2))),
		"/\\*" => {
			buf = new StringBuf();
			var pmin = lexer.curPos();
			var pmax = try lexer.token(comment) catch (e:Eof) throw new LexerError(UnclosedComment, pmin);
			var token = mk(lexer, Comment(buf.toString()));
			token.pos.pmin = pmin.pmin;
			token;
		},
		
		// Unary
		"\\+\\+" => mk(lexer,Unop(OpIncrement)),
		"--" => mk(lexer,Unop(OpDecrement)),
		"!" => mk(lexer, Unop(OpNot)),
		"~" => mk(lexer, Unop(OpTilde)),
		
		// Assign binops
		"%=" => mk(lexer,Binop(OpAssignOp(OpMod))),
		"&=" => mk(lexer,Binop(OpAssignOp(OpAnd))),
		"\\|=" => mk(lexer,Binop(OpAssignOp(OpOr))),
		"^=" => mk(lexer,Binop(OpAssignOp(OpXor))),
		"\\+=" => mk(lexer,Binop(OpAssignOp(OpAdd))),
		"-=" => mk(lexer,Binop(OpAssignOp(OpSub))),
		"\\*=" => mk(lexer,Binop(OpAssignOp(OpMult))),
		"/=" => mk(lexer,Binop(OpAssignOp(OpDiv))),
		"<<=" => mk(lexer,Binop(OpAssignOp(OpShl))),
		">>=" => mk(lexer,Binop(OpAssignOp(OpShl))),
		
		// Comparison
		"==" => mk(lexer,Binop(OpEq)),
		"!=" => mk(lexer,Binop(OpNotEq)),
		"<=" => mk(lexer,Binop(OpLte)),
		">=" => mk(lexer,Binop(OpGte)),
		"<" => mk(lexer,Binop(OpLt)),
		">" => mk(lexer,Binop(OpGt)),
		"&&" => mk(lexer,Binop(OpBoolAnd)),
		"\\|\\|" => mk(lexer,Binop(OpBoolOr)),
		
		// Binops
		"%" => mk(lexer,Binop(OpMod)),
		"&" => mk(lexer,Binop(OpAnd)),
		"\\|" => mk(lexer,Binop(OpOr)),
		"^" => mk(lexer,Binop(OpXor)),
		"\\+" => mk(lexer,Binop(OpAdd)),
		"-" => mk(lexer,Binop(OpSub)),
		"\\*" => mk(lexer,Binop(OpMult)),
		"/" => mk(lexer,Binop(OpDiv)),
		"=" => mk(lexer, Binop(OpAssign)),
		"<<" => mk(lexer,Binop(OpShl)),
		">>" => mk(lexer,Binop(OpShr)),
		"->" => mk(lexer,Arrow),
		
		// Misc. punctuation
		";" => mk(lexer, Semicolon),
		"," => mk(lexer, Comma),
		"\\." => mk(lexer, Dot),
		"::" => mk(lexer, DoubleColon),
		":" => mk(lexer, Colon),
		"\\?" => mk(lexer, Question),
		
		// Wrappy things
		"[" => mk(lexer, BkOpen),
		"]" => mk(lexer, BkClose),
		"{" => mk(lexer, BrOpen),
		"}" => mk(lexer, BrClose),
		"\\(" => mk(lexer, POpen),
		"\\)" => mk(lexer, PClose),
		
		// Strings
		'"' => {
			buf = new StringBuf();
			var pmin = lexer.curPos();
			var pmax = try lexer.token(stringDouble) catch (e:Eof) throw new LexerError(UnterminatedString, pmin);
			var token = mk(lexer, Const(CString(buf.toString())));
			token.pos.pmin = pmin.pmin;
			token;
		},
		"'" => {
			buf = new StringBuf();
			var pmin = lexer.curPos();
			var pmax = try lexer.token(stringSingle) catch (e:haxe.io.Eof) throw new LexerError(UnterminatedString, pmin);
			var token = mk(lexer, Const(CString(buf.toString())));
			token.pos.pmin = pmin.pmin;
			token;
		},
		
		"#" + ident => {
			var name = lexer.current.substr(1);
			if (name == "define") {
				var pmin = lexer.curPos();
				var id = lexer.token(tok);
				var idname;
				switch (id.tok) {
					case Const(CIdent(s)):
						idname = s;
					default: throw new LexerError(DefineMissingIdentifier, pmin);
				}
				buf = new StringBuf();
				var pmax = lexer.token(define);
				var token = mk(lexer, Define(idname, buf.toString().trim()));
				token.pos.pmin = pmin.pmin;
				token;
			} else {
				mk(lexer, Sharp(name));
			}
		},
		ident => {
			var kwd = keywords.get(lexer.current);
			if (kwd != null) {
				mk(lexer, Kwd(kwd));
			} else {
				mk(lexer, Const(CIdent(lexer.current)));
			}
		},
		idtype => mk(lexer, Const(CIdent(lexer.current))),
	];
	
	public static var define = @:rule [
		backslashSkip => {
			lexer.token(define);
		},
		"//" => {
			// TODO cleaner?
			lexer.pos -= 2;
			lexer.curPos().pmax;
		},
		"\r?\n" => {
			lexer.curPos().pmax;
		},
		"[^\r\n]" => {
			buf.add(lexer.current);
			lexer.token(define);
		}
	];
	
	public static var stringDouble = @:rule [
		"\\\\\\\\" => {
			buf.add("\\");
			lexer.token(stringDouble);
		},
		"\\\\n" => {
			buf.add("\n");
			lexer.token(stringDouble);
		},
		"\\\\r" => {
			buf.add("\r");
			lexer.token(stringDouble);
		},
		"\\\\t" => {
			buf.add("\t");
			lexer.token(stringDouble);
		},
		"\\\\\"" => {
			buf.add('"');
			lexer.token(stringDouble);
		},
		'"' => lexer.curPos().pmax,
		"[^\\\\\"]+" => {
			buf.add(lexer.current);
			lexer.token(stringDouble);
		}
	];
	
	public static var stringSingle = @:rule [
		"\\\\\\\\" => {
			buf.add("\\");
			lexer.token(stringSingle);
		},
		"\\\\n" =>  {
			buf.add("\n");
			lexer.token(stringSingle);
		},
		"\\\\r" => {
			buf.add("\r");
			lexer.token(stringSingle);
		},
		"\\\\t" => {
			buf.add("\t");
			lexer.token(stringSingle);
		},
		'\\\\\'' => {
			buf.add('"');
			lexer.token(stringSingle);
		},
		"'" => lexer.curPos().pmax,
		'[^\\\\\']+' => {
			buf.add(lexer.current);
			lexer.token(stringSingle);
		}
	];
	
	public static var comment = @:rule [
		"\\*/" => lexer.curPos().pmax,
		"\\*" => {
			buf.add("*");
			lexer.token(comment);
		},
		"[^\\*]+" => {
			buf.add(lexer.current);
			lexer.token(comment);
		}
	];
	
}