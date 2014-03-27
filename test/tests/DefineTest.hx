package tests;
import haxe.unit.TestCase;

class DefineTest extends ParserTest {
	
	///*
	
	public function testDefine() {
		var p = getParsed("
			#define SomeCustomIdentifier 1337
		");
		assertEquals(1337, p.resolveDefine(p.defines.get("SomeCustomIdentifier")));
	}
	
	public function testIfDef() {
		var p = getParsed("
			#define Existent
			
			#ifdef Existent
			#define A 1
			#else
			#define A 0
			#endif
			
			#ifdef Nonexistent
			#define B 0
			#else
			#define B 1
			#endif
		");
		assertTrue(p.defines.exists("Existent"));
		assertTrue(p.defines.exists("A"));
		assertTrue(p.defines.exists("B"));
		assertFalse(p.defines.exists("Nonexistent"));
		assertEquals(1, p.resolveDefine(p.defines.get("A")));
		assertEquals(1, p.resolveDefine(p.defines.get("B")));
	}
	
	public function testIfNotDef() {
		var p = getParsed("
			#define Existent
			
			#ifndef Existent
			#define A 0
			#else
			#define A 1
			#endif
			
			#ifndef Nonexistent
			#define B 1
			#else
			#define B 0
			#endif
		");
		assertTrue(p.defines.exists("Existent"));
		assertTrue(p.defines.exists("A"));
		assertTrue(p.defines.exists("B"));
		assertFalse(p.defines.exists("Nonexistent"));
		assertEquals(1, p.resolveDefine(p.defines.get("A")));
		assertEquals(1, p.resolveDefine(p.defines.get("B")));
	}
	
	public function testConditionalIf() {
		var p = getParsed("
			#define A 1
			#define B 1
			#if A == B
			#define C 1
			#elif B == 2
			#define C 2
			#else
			#define C 3
			#endif
		");
		assertEquals(1, p.resolveDefine(p.defines.get("C")));
	}
	
	public function testeConditionalElif() {
		var p = getParsed("
			#define A 1
			#define B 2
			#if A == B
			#define C 1
			#elif B == 2
			#define C 2
			#else
			#define C 3
			#endif
		");
		assertEquals(2, p.resolveDefine(p.defines.get("C")));
	}
	
	public function testConditionalElse() {
		var p = getParsed("
			#define A 1
			#define B 3
			#if A == B
			#define C 1
			#elif B == 2
			#define C 2
			#else
			#define C 3
			#endif
		");
		assertEquals(3, p.resolveDefine(p.defines.get("C")));
	}
	
	public function testOperations() {
		var p = getParsed("
			#define TRUE 1
			#define FALSE 0
			
			#define OFF FALSE || FALSE
			#define OFT FALSE || TRUE
			#define OTF TRUE || FALSE
			#define OTT TRUE || TRUE
			
			#define AFF FALSE && FALSE
			#define AFT FALSE && TRUE
			#define ATF TRUE && FALSE
			#define ATT TRUE && TRUE
			
			#define EFF FALSE == FALSE
			#define EFT FALSE == TRUE
			#define ETF TRUE == FALSE
			#define ETT TRUE == TRUE
			
			#define NT !TRUE
			#define NF !FALSE
			
			#define C TRUE || FALSE && !TRUE
		");
		
		assertEquals(0, p.resolveDefine(p.defines.get("OFF")));
		assertEquals(1, p.resolveDefine(p.defines.get("OFT")));
		assertEquals(1, p.resolveDefine(p.defines.get("OTF")));
		assertEquals(1, p.resolveDefine(p.defines.get("OTT")));
		
		assertEquals(0, p.resolveDefine(p.defines.get("AFF")));
		assertEquals(0, p.resolveDefine(p.defines.get("AFT")));
		assertEquals(0, p.resolveDefine(p.defines.get("ATF")));
		assertEquals(1, p.resolveDefine(p.defines.get("ATT")));
		
		assertEquals(1, p.resolveDefine(p.defines.get("EFF")));
		assertEquals(0, p.resolveDefine(p.defines.get("EFT")));
		assertEquals(0, p.resolveDefine(p.defines.get("ETF")));
		assertEquals(1, p.resolveDefine(p.defines.get("ETT")));
		
		assertEquals(0, p.resolveDefine(p.defines.get("NT")));
		assertEquals(1, p.resolveDefine(p.defines.get("NF")));
		
		assertEquals(0, p.resolveDefine(p.defines.get("C")));
		
	}
	
	//*/
	
	public function testArguments() {
		var p = getParsed("
			#define A 1
			#define TEST(x) x || A
			#define B TEST(0)
			#define C TEST(1)
			#define OR(a, b) a || b
			#define DFF OR(0, 0)
			#define DFT OR(0, 1)
			#define DTF OR(1, 0)
			#define DTT OR(1, 1)
			#define DN OR(0, OR(OR(0, 1), OR(0, 1)))
			#define AND(a, b) a && b
			#define E AND(OR(0, 1), OR(0, 0))
		");
		assertEquals(1, p.resolveDefine(p.defines.get("B")));
		assertEquals(1, p.resolveDefine(p.defines.get("C")));
		assertEquals(0, p.resolveDefine(p.defines.get("DFF")));
		assertEquals(1, p.resolveDefine(p.defines.get("DFT")));
		assertEquals(1, p.resolveDefine(p.defines.get("DTF")));
		assertEquals(1, p.resolveDefine(p.defines.get("DTT")));
		assertEquals(1, p.resolveDefine(p.defines.get("DN")));
		assertEquals(0, p.resolveDefine(p.defines.get("E")));
	}
	
}