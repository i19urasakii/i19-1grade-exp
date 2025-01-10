/*
 * 作成日: 2004/03/31
 *
 * Copyright (c) 2004 重村哲至
 * All rights reserved.
 */
package jp.ac.tokuyama.pico.tec6;

/**
 * @author sigemura
 *
 */
class RsvWords {
	private String      spell;
	
	final static RsvWords NO    = new RsvWords("NO"   );
	final static RsvWords LD    = new RsvWords("LD"   );
	final static RsvWords ST    = new RsvWords("ST"   );
	final static RsvWords ADD   = new RsvWords("ADD"  );
	final static RsvWords SUB   = new RsvWords("SUB"  );
	final static RsvWords CMP   = new RsvWords("CMP"  );
	final static RsvWords AND   = new RsvWords("AND"  );
	final static RsvWords OR    = new RsvWords("OR"   );
	final static RsvWords XOR   = new RsvWords("XOR"  );
	final static RsvWords SHLA  = new RsvWords("SHLA" );
	final static RsvWords SHLL  = new RsvWords("SHLL" );
	final static RsvWords SHRA  = new RsvWords("SHRA" );
	final static RsvWords SHRL  = new RsvWords("SHRL" );
	final static RsvWords JMP   = new RsvWords("JMP"  );
	final static RsvWords JZ    = new RsvWords("JZ"   );
	final static RsvWords JC    = new RsvWords("JC"   );
	final static RsvWords JM    = new RsvWords("JM"   );
	final static RsvWords CALL  = new RsvWords("CALL" );
	final static RsvWords IN    = new RsvWords("IN"   );
	final static RsvWords OUT   = new RsvWords("OUT"  );
	final static RsvWords PUSH  = new RsvWords("PUSH" );
	final static RsvWords PUSHF = new RsvWords("PUSHF");
	final static RsvWords POP   = new RsvWords("POP"  );
	final static RsvWords POPF  = new RsvWords("POPF" );
	final static RsvWords EI    = new RsvWords("EI"   );
	final static RsvWords DI    = new RsvWords("DI"   );
	final static RsvWords RET   = new RsvWords("RET"  );
	final static RsvWords RETI  = new RsvWords("RETI" );
	final static RsvWords HALT  = new RsvWords("HALT" );
	final static RsvWords ORG   = new RsvWords("ORG"  );
	final static RsvWords EQU   = new RsvWords("EQU"  );
	final static RsvWords DC    = new RsvWords("DC"   );
	final static RsvWords DS    = new RsvWords("DS"   );
	final static RsvWords G0    = new RsvWords("G0"   );
    final static RsvWords G1    = new RsvWords("G1"   );
    final static RsvWords G2    = new RsvWords("G2"   );
	final static RsvWords SP    = new RsvWords("SP"   );
	
	private final static RsvWords[] words = {
		NO, LD, ST, ADD, SUB, CMP, AND, OR, XOR, SHLA, SHLL, SHRA, SHRL,
		JMP, JZ, JC, JM, CALL, IN, OUT, PUSH, PUSHF, POP, POPF, EI, DI,
		RET, RETI, HALT, EQU, DC, DS, G0, G1, G2, SP , ORG
	};
	
	private RsvWords(String s) {
		spell = s;
	}
	
	static RsvWords search(String word) {
		for (int i=0; i<words.length; i++) {
			if (words[i].spell.compareToIgnoreCase(word)==0) {
				return words[i];
			}
		}
		return null;
	}

	
	public String toString() {
		return spell;
	}
}
