package cppparser;

import byte.ByteData;
import cppparser.CppParser.Define;
#if cppplog
import com.furusystems.slf4hx.loggers.Logger;
import com.furusystems.slf4hx.Logging;
#end
import hxparse.Parser.Parser;
import hxparse.ParserBuilder;
import hxparse.Position;
import cppparser.Data;
import cppparser.structure.CppClass;
import cppparser.structure.CppFunction;
import cppparser.structure.CppVar;
import cppparser.structure.DataAST;

/*
enum Define {
	DExpr(e:Expr);
	DExprArgs(e:Expr, args:Array<Expr>);
}
*/

typedef Define = {
	e:Expr,
	args:Array<Expr>
}

class CppParser extends Parser<CppLexer, Token> implements ParserBuilder {
	
	#if cppplog
	private static var L:Logger = Logging.getLogger(CppParser);
	#end
	
	var input:ByteData;
	
	// Might be useful in the future
	var skipIdents:Map<String, Int>;
	
	//public var directives:Array<String> = [];
	public var initialDefines:Map<String, String>;
	public var macroRewrite:Map<String, Array<String> -> TokenDef>;
	
	var currentConditionals:Array<Bool>;
	//var skipIf:Int = 0;
	var insharp:Bool;
	var insertedTokens:Array<Token>;
	
	//public var defines:Map<String, String>;
	public var defines:Map<String, Define>;
	public var includes:Array<String>;
	public var typedefs:Array<String>;
	public var enums:Array<String>;
	public var classes:Array<CppClass>;
	public var vars:Array<CppVar>;
	public var functions:Array<CppFunction>;
	
	public function new() {
		//this.skipIdents = skipIdents;
		super(null, null);
	}
	
	public function reset() {
		currentConditionals = [];
		insertedTokens = [];
		//skipIf = 0;
		insharp = false;
		
		//defines = new Map<String, String>();
		//defines = new Map<String, Expr>();
		defines = new Map<String, Define>();
		includes = [];
		typedefs = [];
		enums = [];
		classes = [];
		vars = [];
		functions = [];
	}
	
	public function parse(input:ByteData, sourceName:String) {
		this.input = input;
		
		stream = new CppLexer(input, sourceName);
		ruleset = CppLexer.tok;
		
		reset();
		
		// TODO fix
		//if (initialDefines != null) for (key in initialDefines.keys()) defines.set(key, initialDefines.get(key));
		
		var stop = false;
		while (!stop) {
			//if (preparse()) continue;
			//printToken(peek(0));
			switch stream {
				case [ { tok: Kwd(KwdClass) } ]:
					var c = parseClass();
					if (c != null) classes.push(c);
				case [ { tok: Kwd(KwdTypedef) } ]:
					var skipped = skipToSemicolon();
					typedefs.push(skipped.join(""));
				case [ { tok: Kwd(KwdEnum) } ]:
					var enumName:String = "";
					switch stream {
						case [ { tok: Const(CIdent(ident)) } ]:
							enumName = ident;
						case _:
					}
					var skipped = skipBlock();
					enums.push(enumName + " " + skipped.join(""));
					if (peek(0).tok != Semicolon) throw "Unable to skip enum, semicolon after closing brace missing";
					junk();
				case [ { tok: Kwd(KwdExtern) } ]:
					var skipped = skipToSemicolon();
					trace("Skipped "+skipped.join(" "));
				case [ { tok: Eof } ]:
					finished();
					break;
				case _:
					var defs = parseVarDefinitions(false, false, true);
					switch stream {
						case [ { tok: POpen } ] if (defs.length == 1):
							var def = defs[0];
							functions.push(parseFunction(def.name, def));
						case [ { tok: Semicolon } ] if (defs.length > 0):
							vars = vars.concat(defs);
							continue;
						case _:
							stop = true;
							throw "Unimplemented token: "+peek(0);
					}
			}
		}
	}
	
	inline function finished() {
		// yay!
	}
	
	/*
	function preparse() {
		//if (checkIdentSkip()) return true;
		if (parsePreproc()) return true;
		if (rewriteMacros()) return true;
		return false;
	}
	*/
	
	function checkIdentSkip() {
		var tok = peek(0).tok;
		switch (tok) {
			case Const(CIdent(ident)):
				var skipNum:Int = skipIdents.get(ident);
				if (skipNum != null) {
					junk();
					for (i in 0...skipNum) {
						// TODO no peeking needed when cache isn't in place anymore?
						peek(0);
						junk();
					}
					// skipped over n tokens
				}
			default:
		}
		return false;
	}
	
	function rewriteMacros() {
		if (macroRewrite == null) return false;
		var token = peek(0);
		switch (token.tok) {
			case Const(CIdent(ident)):
				var rewriter = macroRewrite.get(ident);
				if (rewriter != null) {
					junk();
					var params:Array<String> = [];
					switch stream {
						case [ { tok: POpen } ]:
							while (true) {
								switch stream {
									case [ { tok: Const(c) } ]:
										params.push(getConstantString(c));
										switch stream {
											case [ { tok: Comma } ]:
											case [ { tok: PClose } ]: break;
										}
									case _: throw "Invalid value parsing macro rewrite params: "+peek(0);
								}
							}
							if (peek(0).tok == Semicolon) junk();
						case _:
					}
					var result = rewriter(params);
					if (result != null) insertedTokens.push(new Token(result, token.pos));
					return true;
				}
			default:
		}
		return false;
	}
	
	function getConstantString(c:Constant) {
		return switch(c) {
			case CInt(s) | CLong(s) | CFloat(s) | CString(s) | CIdent(s): return s;
		}
	}
	
	// Obsolete
	/*
	function parsePreproc() {
		if (skipIf > 0) {
			switch stream {
				case [ { tok: Sharp("if") }, { tok: Const(CIdent(t)) } ]:
					skipIf++;
				case [ { tok: Sharp("endif") } ]:
					skipIf--;
					if (skipIf == 0) currentConditionals.pop();
				default: junk();
			}
			return true;
		}
		switch stream {
			case [ { tok: Sharp("ifdef" | "ifndef") }, { tok: Const(CIdent(t)) } ]:
				currentConditionals.push(t);
			case [ { tok: Sharp("if") }, { tok: Const(CIdent(t)) } ]:
				if (directives.indexOf(t) == -1) {
					trace("Skipping "+t);
					skipIf++;
				}
				currentConditionals.push(t);
			case [ { tok: Sharp("endif") } ]:
				currentConditionals.pop();
			case [ { tok: Define(k, v) } ]:
				if (defines.exists(k)) throw "Multiple definitions found: "+k;
				defines.set(k, v);
			case [ { tok: Sharp("include") }, { tok: Const(CString(t)) } ]:
				includes.push(t);
			default:
				return false;
		}
		return true;
	}
	*/
	
	inline function printToken(token:Token) {
		#if cppplog
		var line = getLine(token.pos);
		var lp = token.pos.getLinePosition(input);
		L.debug(lp.lineMin + "\t" + line.psource);
		L.debug(token.tok);
		#end
	}
	
	function getLine(pos:Position) {
		var bytes:ByteData = input;
		var ls = findLineStart(bytes, pos.pmin);
		var le = findLineEnd(bytes, pos.pmax);
		if (le < ls) le = ls;
		return new Position(bytes.readString(ls, le-ls), ls, le);
	}
	
	function findLineStart(bytes:ByteData, index:Int) {
		var c;
		while (index > 0 && (c = bytes.readByte(index)) != 10) {
			index--;
		}
		index++;
		return index;
	}
	
	function findLineEnd(bytes:ByteData, index:Int) {
		var c;
		var len = bytes.length;
		while (index < len) {
			c = bytes.readByte(index);
			if (c == 13 || c == 10) break;
			index++;
		}
		return index;
	}
	
	function getVisibility(kwd:Keyword) {
		return switch (kwd) {
			case KwdPublic: Visibility.Public;
			case KwdProtected: Visibility.Protected;
			case KwdPrivate: Visibility.Private;
			default: throw "Unknown visibility keyword: "+kwd;
		}
	}
	
	function parseClass() {
		var token;
		//trace("classpeek "+[ for (i in 0...10) peek(i).tok ]);
		
		var cl = new CppClass();
		cl.idents = parseIdents();
		cl.name = ""+cl.idents.pop();
		
		switch stream {
			// Forward declaration
			case [ { tok: Semicolon } ]: return null;
			case [ { tok: Colon },
				   { tok: Kwd(kvis = KwdPublic | KwdProtected | KwdPrivate) },
				   { tok: Const(CIdent(ident)) }
				]:
				cl.inheritance = {
					visibility: getVisibility(kvis),
					name: ident
				};
			case _:
		}
		
		if (peek(0).tok != BrOpen) throw "Error parsing class, missing opening brace";
		junk();
		
		var visibility = Visibility.Private;
		var operator = null;
		var defs = null;
		var destructor = false;
		var destructorName:String = "";
		
		while (true) {
			//if (preparse()) continue;
			//printToken(peek(0));
			//for (i in 0...10) printToken(peek(i));
			switch stream {
				case [ { tok: BrClose } ]:
					if (peek(0).tok != Semicolon) throw "Error parsing class, missing semicolon after closing brace";
					junk();
					break;
				case [ { tok: Kwd(kvis = KwdPublic | KwdProtected | KwdPrivate) }, { tok: Colon } ]:
					visibility = getVisibility(kvis);
					continue;
				case _:
			}
			if (defs == null) defs = parseVarDefinitions(true, true, false);
			switch stream {
				case [ { tok: Binop(op) } ]:
					if (operator != null) throw "Error parsing function, cannot define a second operator: "+op;
					operator = op;
					continue;
				case [ { tok: Unop(OpTilde) }, { tok: Const(CIdent(ident)) } ]:
					destructor = true;
					destructorName = ident;
					continue;
				case [ { tok: POpen } ]:
					// Function
					if (defs.length > 1) throw "Error parsing function, only one return var allowed";
					var def = defs.length == 1 ? defs[0] : null;
					if (def == null) throw "Error parsing class, missing function name (rogue open parenthesis)";
					if (def.name == null) {
						// Constructor / destructor
						def.name = def.type;
						def.type = null;
						if (destructor) def.name = destructorName;
						if (cl.name != def.name) throw "Error parsing class, function missing specifiers or wrong constructor name: "+def.name;
						var fn = parseFunction(def.name);
						if (destructor) {
							cl.destructors.push(fn);
						} else {
							cl.constructors.push(fn);
						}
						//L.info((destructor ? "Destructor" : "Constructor") + " " + fn);
					} else {
						// Method
						if (operator != null) {
							if (def.name != "operator") throw "Error parsing function, operator overload must have the name 'operator': "+def.name;
						}
						var castOverload = false;
						if (def.type == "operator") {
							var t = def.type;
							def.type = def.name;
							def.name = t;
							castOverload = true;
						}
						var fn = parseFunction(def.name, def);
						fn.operatorOverload = operator;
						fn.castOverload = castOverload;
						fn.visibility = visibility;
						//L.info(fn);
						cl.functions.push(fn);
					}
					defs = null;
				case [ { tok: Semicolon } ]:
					// Properties
					for (def in defs) {
						def.visibility = visibility;
						//L.info(def);
						cl.properties.push(def);
					}
					defs = null;
				default: throw "Error parsing class, unexpected token: "+peek(0).tok;
			}
			if (defs != null) throw "Error parsing class, rogue var definitions found: "+defs;
			operator = null;
			destructor = false;
		}
		
		return cl;
	}
	
	function parseFunction(name:String, returnType:CppVar = null) {
		var fn = new CppFunction();
		fn.name = name;
		if (returnType != null) fn.returnType = returnType;
		parseFunctionParams(fn);
		parseFunctionInitList(fn);
		parseFunctionBody(fn);
		return fn;
	}
	
	function parseFunctionParams(fn:CppFunction) {
		fn.parameters = parseVarDefinitions(false, false, true);
		switch stream {
			case [ { tok: PClose } ]:
			default: throw "Error parsing function, missing closing parenthesis";
		}
	}
	
	function parseVarDefinitions(allowTypeOmission:Bool, allowNameOmission:Bool, allowDefaultValues:Bool) {
		var defs:Array<CppVar> = [];
		var previous:CppVar = null;
		var finished:CppVar = null;
		var end:Bool = false;
		var v:CppVar = null;
		var dirty:Bool = true;
		var idents:Int = 0;
		while (true) {
			//if (preparse()) continue;
			if (v == null) {
				if (finished != null) {
					if (idents < 2) {
						if (previous == null) {
							// Function names
							if (dirty && !(allowNameOmission && end)) {
								throw "Missing var definition name: "+finished;
							}
						} else if (idents == 1 && finished.keywords.length == 0 && !finished.reference) {
							// Omitted type from var, take the previous one
							finished.name = finished.type;
							previous.copyPropertiesInto(finished);
						} else throw "Incomplete specifiers for var in definition list: "+finished;
					}
					defs.push(finished);
				}
				if (end) {
					break;
				}
				dirty = false;
				idents = 0;
				v = new CppVar();
			}
			switch stream {
				case [ { tok: Kwd(kwd) } ]:
					v.keywords.push(kwd);
				case [ { tok: Const(CIdent(s)) } ]:
					switch (idents) {
						case 0: v.type = s;
						case 1: v.name = s;
						default: throw 'Unexpected identifier while parsing var definition (${v.type}, ${v.name}): $s';
					}
					idents++;
				case [ { tok: Binop(OpAnd) } ]:
					v.reference = true;
				case [ { tok: Binop(OpMult) } ]:
					v.pointer = true;
				case [ { tok: Comma } ]:
					previous = finished; finished = v; v = null;
				default:
					previous = finished; finished = v; v = null;
					end = true;
					continue;
			}
			if (allowDefaultValues) {
				switch stream {
					case [ { tok: Binop(OpAssign) } ]:
						var dv = parseConstantExpression();
						if (idents != 2) throw "Can't assign default value to a nameless, typeless var: "+v;
						v.defaultValue = dv;
					case _:
					//default: throw "Invalid value for default value assignment: "+peek(0);
				}
			}
			dirty = true;
		}
		return defs.length == 1 && !dirty ? [] : defs;
	}
	
	function parseConstantExpression() {
		return expr();
	}
	
	public function resolveDefine(def:Define):Int {
		return resolveConstantExpression(def.e);
	}
	
	function resolveConstantExpression(e:Expr, args:Map<String, Expr> = null):Int {
		if (e == null) return 0;
		/*
		trace("RESOLVE");
		trace(e.expr);
		if (args != null) {
			trace("ARGS");
			trace(args);
		}
		//*/
		var r = switch e.expr {
			case EConst(CInt(s)): Std.parseInt(s);
			case EConst(CIdent(ident)):
				var def = defines.get(ident);
				//resolveConstantExpression(def.e, def.args);
				var expr;
				if (def == null) {
					expr = args.get(ident);
					if (expr == null) throw "Identifier not defined or an argument: "+ident;
				} else {
					expr = def.e;
				}
				resolveConstantExpression(expr, args);
			case ECall({ expr: EConst(CIdent("defined")) }, [{ expr: EConst(CIdent(ident)) }]): defines.exists(ident) ? 1 : 0;
			case ECall({ expr: EConst(CIdent(ident)) }, params):
				//trace("CALL "+ident+" "+params);
				var def = defines.get(ident);
				if (args == null) {
					args = new Map<String, Expr>();
				} else {
					var orig = args;
					args = new Map<String, Expr>();
					for (key in orig.keys()) args[key] = orig[key];
				}
				for (i in 0...def.args.length) {
					var arg = def.args[i].expr;
					switch arg {
						case EConst(CIdent(argName)):
							args[argName] = params[i];
						default: throw "Invalid arg type: "+arg;
					}
				}
				//trace("ARGS "+args);
				resolveConstantExpression(def.e, args);
			case EBinop(op, a, b):
				switch op {
					case OpBoolOr: (resolveConstantExpression(a, args) > 0 || resolveConstantExpression(b, args) > 0) ? 1 : 0;
					case OpBoolAnd: (resolveConstantExpression(a, args) > 0 && resolveConstantExpression(b, args) > 0) ? 1 : 0;
					case OpEq: (resolveConstantExpression(a, args) == resolveConstantExpression(b, args)) ? 1 : 0;
					default: throw "Unsupported binary op: "+op;
				}
			case EUnop(op, postFix, e):
				switch op {
					case OpNot: return resolveConstantExpression(e, args) == 0 ? 1 : 0;
					default: throw "Unsupported unary op: "+op;
				}
			default: throw "Unsupported constant expression: "+e;
		}
		trace("RESOLVE "+e.expr+" = "+r);
		return r;
	}
	
	function expr():Expr {
		return switch stream {
			case [ { tok: Unop(op), pos: p }, e = expr() ]: makeUnop(op, e, p);
			case [ { tok: Const(c), pos: p } ]: exprNext({ expr: EConst(c), pos: p });
			case [ { tok: POpen, pos: pa }, e = expr(), { tok: PClose, pos: pb } ]: exprNext({ expr: EParenthesis(e), pos: Position.union(pa, pb) });
		}
		 //| Kwd(KwdTrue | KwdFalse) | Binop(_) } :
	}
	
	function exprNext(ea:Expr) {
		return switch stream {
			case [ { tok: Binop(op), pos: p }, eb = expr() ]:
				makeBinop(op, ea, eb);
			case [ { tok: POpen, pos:_ } ]:
				switch stream {
					case [params = parseCallParams(), { tok: PClose, pos: pb } ]:
						exprNext({ expr: ECall(ea, params), pos: Position.union(ea.pos, pb) });
				}
			case _: ea;
		}
	}
	
	function parseCallParams() {
		var ret = [];
		switch stream {
			case [e = expr()]: ret.push(e);
			case _: return [];
		}
		while (true) {
			switch stream {
				case [ { tok: Comma }, e = expr() ]: ret.push(e);
				case _: break;
			}
		}
		return ret;
	}
	
	static function precedence(op:Binop) {
		var left = true;
		var right = false;
		return switch (op) {
			case OpMod : {p: 0, left: left};
			case OpMult | OpDiv : {p: 0, left: left};
			case OpAdd | OpSub : {p: 0, left: left};
			case OpShl | OpShr | OpUShr : {p: 0, left: left};
			case OpOr | OpAnd | OpXor : {p: 0, left: left};
			case OpEq | OpNotEq | OpGt | OpLt | OpGte | OpLte : {p: 0, left: left};
			case OpBoolAnd : {p: 0, left: left};
			case OpBoolOr : {p: 0, left: left};
			case OpAssign | OpAssignOp(_) : { p:10, left:right };
			case OpPreConcat: {p: 0, left: left};
		}
	}
	
	static function swap(opa:Binop, opb:Binop) {
		var a = precedence(opa);
		var b = precedence(opb);
		return a.left && a.p <= b.p;
	}
	
	static function makeBinop(op:Binop, ea:Expr, eb:Expr) {
		return switch (eb.expr) {
			case EBinop(top, tea, teb) if (swap(op, top)):
				var te = makeBinop(op, ea, tea);
				{ expr: EBinop(top, te, teb), pos: Position.union(tea.pos, teb.pos) };
			case _:
				{ expr: EBinop(op, ea, eb), pos: Position.union(ea.pos, eb.pos) };
		}
	}

	static function makeUnop(op:Unop, e:Expr, p:Position) {
		return switch(e.expr) {
			case EBinop(bop, ea, eb):
				{ expr: EBinop(bop, makeUnop(op, ea, p), eb), pos: Position.union(p, ea.pos) };
			case _:
				{ expr: EUnop(op, false, e), pos: Position.union(p, e.pos) };
		}
	}
	
	function parseFunctionInitList(fn:CppFunction) {
		if (peek(0).tok != Colon) return;
		junk();
		while (true) {
			//printToken(peek(0));
			switch stream {
				case [ { tok: Const(CIdent(name)) }, { tok: POpen } ]:
					// TODO proper parsing for initialization contents (expression?)
					var contents = "";
					while (true) {
						//printToken(peek(0));
						switch stream {
							case [ { tok: PClose } ]:
								fn.initList.set(name, contents);
								break;
							default:
								contents += peek(0);
								junk();
						}
					}
				case [ { tok: Comma } ]:
				default: break;
			}
		}
	}
	
	function parseFunctionBody(fn:CppFunction) {
		//if (peek(0).tok != BrOpen) throw "Error parsing function, missing opening brace";
		//junk();
		switch stream {
			case [ { tok: Kwd(KwdConst) } ]:
				fn.bodyConst = true;
			case _:
		}
		switch stream {
			case [ { tok: Binop(OpAssign) }, { tok: Const(CInt("0")) } ]:
				fn.pureVirtual = true;
			case _:
		}
		switch stream {
			case [ { tok: Semicolon } ]:
				return;
			case _:
		}
		fn.body = skipBlock().join("");
	}
	
	function parseIdents() {
		var idents:Array<Token> = [];
		var token;
		while (true) {
			switch stream {
				case [ { tok: Const(CIdent(s)) } ]:
					idents.push(last);
				default: break;
			}
		}
		return idents;
	}
	
	function skipBlock() {
		var skipped = [];
		var token;
		token = peek(0);
		if (token.tok != BrOpen) throw "Unable to skip block, not an open brace: "+token.tok;
		junk();
		skipped.push(token);
		var level = 1;
		while (token.tok != Eof) {
			token = peek(0);
			skipped.push(token);
			junk();
			if (token.tok == BrOpen) {
				level++;
			}
			if (token.tok == BrClose) {
				level--;
				if (level == 0) return skipped;
			}
		}
		throw "Unable to skip block, end of file encountered before closing brace";
	}
	
	function skipToSemicolon() {
		var skipped = [];
		while (true) {
			var token = peek(0);
			skipped.push(token);
			junk();
			if (token.tok == Semicolon) return skipped;
		}
	}
	
	function printPeek(n) {
		var token = peek(n);
		return token.tok;
	}
	
	override function junk() {
		if (insertedTokens.pop() == null) super.junk();
	}
	
	function skipUntilEndIf() {
		var skipIf = 1;
		while (skipIf > 0) {
		//while (currentConditionals.length >= enterLen) {
			switch super.peek(0) {
				case { tok: Sharp("if") }:
					skipIf++;
				case { tok: Sharp("elif") } :
					if (skipIf == 1) {
						//skipIf = 0;
						//currentConditionals.pop();
						break;
					}
				case { tok: Sharp("else") } :
					if (skipIf == 1) {
						//skipIf = 0;
						//currentConditionals.pop();
						break;
						//currentConditionals.push(false);
					}
				case { tok: Sharp("endif") }:
					skipIf--;
					if (skipIf == 0) break;
					//if (skipIf == 0) {
						//currentConditionals.pop();
					//}
				case { tok: Eof }:
					throw "Eof while looking for endif at level: "+skipIf;
				default:
			}
			junk();
		}
	}
	
	override function peek(n):Token {
		var t;
		
		if (insertedTokens.length > n) {
			t = insertedTokens[insertedTokens.length-1-n];
		} else {
			t = super.peek(n-insertedTokens.length);
		}
		printToken(t);
		t = if (n == 0) {
			
			if (macroRewrite != null) {
				switch t {
					case { tok: Const(CIdent(ident)) }:
						var rewriter = macroRewrite.get(ident);
						if (rewriter != null) {
							junk();
							var params:Array<String> = [];
							switch super.peek(0) {
								case { tok: POpen } :
									junk();
									while (true) {
										switch super.peek(0) {
											case { tok: Const(c) } :
												junk();
												params.push(getConstantString(c));
												switch super.peek(0) {
													case { tok: Comma }: junk();
													case { tok: PClose }: junk(); break;
													default: throw "Invalud value parsing macro rewrite params: "+super.peek(0);
												}
											default: throw "Invalid value parsing macro rewrite params: "+super.peek(0);
										}
									}
									if (super.peek(0).tok == Semicolon) junk();
								default:
							}
							var result = rewriter(params);
							if (result != null) insertedTokens.push(new Token(result, t.pos));
							return peek(0);
						}
					default:
				}
			}
			
			if (insharp) t;
			else switch t {
				case { tok: CommentLine(_) | Comment(_) | Newline }:
					junk();
					peek(0);
				// "Preprocessor" business //
				case { tok: Sharp(s = "ifdef" | "ifndef") }:
					junk();
					switch peek(0) {
						case { tok: Const(c = CIdent(_)) } :
							junk();
							var res = resolveConstantExpression({ expr: ECall({ expr: EConst(CIdent("defined")), pos: null }, [{ expr: EConst(c), pos: null }]), pos: null });
							if (s == "ifndef") res = 1-res;
							if (res == 0) {
								currentConditionals.push(false);
								skipUntilEndIf();
							} else {
								currentConditionals.push(true);
							}
						default: "Missing ifdef identifier";
					}
					peek(0);
				//case { tok: Sharp("else") } :
					//junk();
					//skipIf++;
					//currentConditionals.push(false);
					//skipUntilEndIf();
					//peek(0);
				case { tok: Sharp("endif") }:
					junk();
					currentConditionals.pop();
					peek(0);
				case { tok: Sharp(s = "if" | "elif" | "else") }:
					junk();
					var e = null;
					if (s != "else") {
						insharp = true;
						e = parseConstantExpression();
						insharp = false;
					}
					var res =
						s == "if" || (s == "elif" && !currentConditionals[currentConditionals.length-1]) ? resolveConstantExpression(e) :
						s == "else" && !currentConditionals[currentConditionals.length-1] ? 1 : 0
					;
					if (res == 0) {
						if (s == "if") currentConditionals.push(false);
						skipUntilEndIf();
					} else {
						if (s != "if") currentConditionals.pop();
						currentConditionals.push(true);
					}
					peek(0);
					/*
					switch peek(0) {
						case { tok: Const(CIdent(t)) } :
							junk();
							currentConditionals.push(t);
							if (directives.indexOf(t) == -1) {
								skipIf++;
								while (skipIf > 0) {
									trace(skipIf+" "+super.peek(0));
									switch super.peek(0) {
										case { tok: Sharp("if") }:
											skipIf++;
										case { tok: Sharp("elif") } :
											if (skipIf == 1) {
												skipIf = 0;
												currentConditionals.pop();
												break;
											}
										case { tok: Sharp("else") } :
											if (skipIf == 1) {
												skipIf = 0;
												currentConditionals.pop();
												currentConditionals.push("");
											}
										case { tok: Sharp("endif") }:
											skipIf--;
											if (skipIf == 0) {
												currentConditionals.pop();
											}
										case { tok: Eof }:
											throw "Eof while looking for endif at level: "+skipIf;
										default:
									}
									junk();
								}
							}
						default: "Missing if identifier";
					}
					*/
					/*
				case { tok: Define(k, v) }:
					junk();
					if (defines.exists(k)) throw "Multiple definitions found: "+k;
					defines.set(k, v);
					//trace('DEFINE $k $v');
					peek(0);
					*/
				case { tok: Sharp("define") }:
					junk();
					insharp = true;
					var defName = switch stream {
						case [ { tok: Const(CIdent(ident)) } ]:
							ident;
						default: unexpected();
					}
					var args = null;
					var expression = switch stream {
						case [{ tok: Newline }]:
							{ expr: EConst(CInt("1")), pos: null }
						case [ { tok: POpen }, a = parseCallParams(), { tok: PClose }, e = parseConstantExpression(), { tok: Newline | Eof } ]:
							args = a;
							e;
						case [ e = parseConstantExpression(), { tok: Newline | Eof } ]:
							e;
						case _: unexpected();
					}
					defines.set(defName, { e: expression, args: args });
					insharp = false;
					peek(0);
				case { tok: Sharp("include") }:
					junk();
					switch peek(0) {
						case { tok: Const(CString(t)) }:
							junk();
							includes.push(t);
						default: throw "Missing include string";
					}
					peek(0);
				default: t;
			}
		} else {
			switch t {
				case { tok: CommentLine(_) | Comment(_) | Sharp("endif") }:
					peek(n+1);
				case { tok: Sharp("include") } :
					peek(n+2);
				case { tok: Sharp("ifdef" | "ifndef" | "if") }:
					throw "Unsupported peek into macro";
				default: t;
			}
		}
		//printToken(t);
		return t;
	}
	
	
	
}