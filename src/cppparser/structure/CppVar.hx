package cppparser.structure;

import cppparser.Data;
import cppparser.structure.DataAST.Visibility;

class CppVar {
	
	public var visibility:Visibility = NotAvailable;
	
	public var keywords:Array<Keyword> = [];
	
	public var type:String = null;
	public var name:String;
	
	public var reference:Bool = false;
	public var pointer:Bool = false;
	
	public var defaultValue:Null<Expr>;
	
	public function new() {
		
	}
	
	public function copyPropertiesInto(v:CppVar) {
		v.type = type;
		v.reference = reference;
		v.keywords = keywords;
	}
	
	public function toString() {
		return {
			(visibility == NotAvailable ? "[Var" : '[Property ${visibility.getName()}') +
			' $type $name ' +
			(reference ? "by reference " : "") +
			[for (k in keywords) k.getName()] +
			"]";
		};
	}
	
}