package cppparser.structure;

import cppparser.Data;
import cppparser.structure.DataAST;

class CppClass {
	
	public var idents:Array<Token> = [];
	public var name:String;
	
	public var inheritance: { visibility:Visibility, name:String };
	
	public var constructors:Array<CppFunction> = [];
	public var destructors:Array<CppFunction> = [];
	
	public var functions:Array<CppFunction> = [];
	
	public var properties:Array<CppVar> = [];
	
	public function new() {}
	
	public function toString() {
		return {
			'[Class $name ' +
			"[" + idents.join(", ") + "]" +
			(inheritance == null ? "" : ' inherits being ${inheritance.visibility} from ${inheritance.name} ') +
			"\n\t" +
			" constructors " +
			"\n\t" +
			constructors.join("\n\t") +
			" destructors " +
			"\n\t" +
			destructors.join("\n\t") +
			" properties \n\t" +
			"\n\t" +
			properties.join("\n\t") +
			" functions \n\t" +
			"\n\t" +
			functions.join("\n\t") +
			"\n]";
		}
	}
	
}