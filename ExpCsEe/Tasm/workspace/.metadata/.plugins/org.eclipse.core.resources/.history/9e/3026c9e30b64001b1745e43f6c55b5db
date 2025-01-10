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
class InstTblEntry {
  private RsvWords mnemonic; // ニーモニック
  private int length; // 長さ
  private byte code; // オペコード
  private SyntaxAnalyzer analyzer; // 構文解析クラス

  InstTblEntry(RsvWords o, int l, byte c, SyntaxAnalyzer a) {
    mnemonic = o;
    length = l;
    code = c;
    analyzer = a;
  }

  RsvWords getMnemonic() {
    return mnemonic;
  }

  int getLength() {
    return length;
  }

  byte getCode() {
    return code;
  }

  SyntaxAnalyzer getAnalyzer() {
    return analyzer;
  }
}
