package cppparser.structure;

import cppparser.Data;
import cppparser.structure.DataAST.Visibility;

class CppFunction {
	
	public var visibility:Visibility = NotAvailable;
	
	public var name:String;
	
	public var returnType:CppVar;
	public var parameters:Array<CppVar> = [];
	public var initList:Map<String, String> = new Map<String, String>();
	public var operatorOverload:Null<Binop>;
	public var castOverload:Bool = false;
	
	public var pureVirtual:Bool = false;
	
	public var bodyConst:Bool = false;
	public var body:String;
	
	public function new() {
		
	}
	
	public function toString() {
		return {
			"[Function " + 
			(visibility == NotAvailable ? "" : ${visibility.getName()}) +
			' $name ' +
			(operatorOverload == null ? "" : "overloads "+operatorOverload) +
			' $returnType ' + 
			parameters.join(" ") +
			" " + initList +
			(bodyConst ? " const body (can't modify members)" : "") +
			"]";
		}
	}
	
}