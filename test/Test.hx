package ;
import byte.ByteData;
import cppparser.CppParser;
import haxe.unit.TestCase;
import haxe.unit.TestRunner;
import sys.io.File;
import tests.DefineTest;

import cppparser.Data;

class Test {
	
	static function main() {
		//new Test();
		var r = new TestRunner();
		r.add(new DefineTest());
		Sys.exit(r.run() ? 0 : 1);
	}
	
	/*
	public function new() {
		var testFile = "../test/listbase.h";
		
		var nullReturn = function(params:Array<String>) { return null; };
		var macroRewrite = [
			"DECLARE_DYNAMIC_CLASS" => nullReturn,
			"DECLARE_DYNAMIC_CLASS_NO_ASSIGN" => nullReturn,
			"wxDECLARE_EXPORTED_EVENT" => nullReturn,
			"WXUNUSED" => 
				function(params:Array<String>) { return Const(CIdent(params[0])); }
		];
		
		var parser = new CppParser(ByteData.ofString(File.getContent(testFile)), testFile);
		parser.macroRewrite = macroRewrite;
		parser.parse();
		
		trace("DEFINES");
		trace(parser.defines);
		
		trace("INCLUDES");
		trace(parser.includes);
		
		trace("TYPEDEFS");
		trace(parser.typedefs);
		
		trace("ENUMS");
		trace(parser.enums);
		
		trace("CLASSES");
		trace(parser.classes.join("\n\n\n\n\n"));
	}
	*/
	
}