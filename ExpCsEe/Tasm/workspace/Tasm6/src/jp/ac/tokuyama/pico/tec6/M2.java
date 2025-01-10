/*
 * 作成日: 2004/04/01
 *
 * Copyright (c) 2004 重村哲至
 * All rights reserved.
 */
package jp.ac.tokuyama.pico.tec6;

/**
 * @author sigemura
 * SHLA,SHLL,SHRA,SHRL,PUSH,POP
 */
class M2 extends SyntaxAnalyzer {

  /* (非 Javadoc)
   * @see jp.ac.tokuyama.pico.tec.SyntaxAnalyzer#analyzer(jp.ac.tokuyama.pico.tec.Lexer, boolean)
   */
  byte[] analyzer(byte op, Lexer lexer, SymTbl symTbl) throws AsmException {
    Object w = lexer.getNextTok().getValue();

    if (!isReg(w)) {
      throw new AsmException(Err.BAD_Reg);
    }
    lexer.setNextTok(false);
    
    byte[] obj = new byte[1];
    obj[0] = (byte)(op | (regNo(w)<<2));
    return obj;
  }

}
