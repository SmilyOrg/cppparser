package tests;
import byte.ByteData;
import cppparser.CppParser;
import haxe.unit.TestCase;

class ParserTest extends TestCase {

	function getParsed(test:String) {
		var parser = new CppParser();
		parser.parse(ByteData.ofString(test), "<test>");
		return parser;
	}
	
}