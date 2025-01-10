/*
 * 作成日: 2004/04/01
 *
 * Copyright (c) 2004 重村哲至
 * All rights reserved.
 */
package jp.ac.tokuyama.pico.tec6;

/**
 * @author sigemura
 *
 */
class P1 extends SyntaxAnalyzer {

  /* (非 Javadoc)
   * @see jp.ac.tokuyama.pico.tec.SyntaxAnalyzer#analyzer(jp.ac.tokuyama.pico.tec.Lexer, boolean)
   */
  byte[] analyzer(byte op, Lexer lexer, SymTbl symTbl) throws AsmException {
    System.err.println("class P1 が使用された！！  バグバグバグ！！");
    System.exit(1);
    return null;
  }
}
