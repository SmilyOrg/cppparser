package cppparser.structure;

class CppType {
	
	public var name:Null<String>;
	public var params:Array<CppType>;
	
	public function new(name, params) {
		this.name = name;
		this.params = params;
	}
	
}