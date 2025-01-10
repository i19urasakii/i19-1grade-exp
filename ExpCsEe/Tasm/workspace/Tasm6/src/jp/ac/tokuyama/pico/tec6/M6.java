/*
 * 作成日: 2004/04/01
 *
 * Copyright (c) 2004 重村哲至
 * All rights reserved.
 */
package jp.ac.tokuyama.pico.tec6;

/**
 * @author sigemura
 * JMP,JZ,JC,JM,CALL
 */
class M6 extends SyntaxAnalyzer {

  /* (非 Javadoc)
   * @see jp.ac.tokuyama.pico.tec.SyntaxAnalyzer#analyzer(jp.ac.tokuyama.pico.tec.Lexer, boolean)
   */
  byte[] analyzer(byte op, Lexer lexer, SymTbl symTbl) throws AsmException {
    Token  token = lexer.getNextTok();
    byte[] obj = new byte[2];

    obj[0] = op;
    obj[1] = (byte) expr(lexer, symTbl, false);

    token = lexer.getNextTok();
    if (token.getType()==TokenType.CMM) {
      // インデクスドモード
      lexer.setNextTok(true);
      token = lexer.getNextTok();
      if (!isIndexReg(token.getValue())) {
        throw new AsmException(Err.BAD_IReg);
      }
      lexer.setNextTok(false);
      obj[0] = (byte)(obj[0] | regNo(token.getValue()));
    }
    return obj;
  }

}
