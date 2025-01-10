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

class InstTbl {
  final static InstTblEntry[] tbl =
    {
      new InstTblEntry(RsvWords.NO, 1, (byte) 0x00, SyntaxAnalyzer.M1),
      new InstTblEntry(RsvWords.LD, 2, (byte) 0x10, SyntaxAnalyzer.M4),
      new InstTblEntry(RsvWords.ST, 2, (byte) 0x20, SyntaxAnalyzer.M5),
      new InstTblEntry(RsvWords.ADD, 2, (byte) 0x30, SyntaxAnalyzer.M4),
      new InstTblEntry(RsvWords.SUB, 2, (byte) 0x40, SyntaxAnalyzer.M4),
      new InstTblEntry(RsvWords.CMP, 2, (byte) 0x50, SyntaxAnalyzer.M4),
      new InstTblEntry(RsvWords.AND, 2, (byte) 0x60, SyntaxAnalyzer.M4),
      new InstTblEntry(RsvWords.OR, 2, (byte) 0x70, SyntaxAnalyzer.M4),
      new InstTblEntry(RsvWords.XOR, 2, (byte) 0x80, SyntaxAnalyzer.M4),
      new InstTblEntry(RsvWords.SHLA, 1, (byte) 0x90, SyntaxAnalyzer.M2),
      new InstTblEntry(RsvWords.SHLL, 1, (byte) 0x91, SyntaxAnalyzer.M2),
      new InstTblEntry(RsvWords.SHRA, 1, (byte) 0x92, SyntaxAnalyzer.M2),
      new InstTblEntry(RsvWords.SHRL, 1, (byte) 0x93, SyntaxAnalyzer.M2),
      new InstTblEntry(RsvWords.JMP, 2, (byte) 0xA0, SyntaxAnalyzer.M6),
      new InstTblEntry(RsvWords.JZ, 2, (byte) 0xA4, SyntaxAnalyzer.M6),
      new InstTblEntry(RsvWords.JC, 2, (byte) 0xA8, SyntaxAnalyzer.M6),
      new InstTblEntry(RsvWords.JM, 2, (byte) 0xAC, SyntaxAnalyzer.M6),
      new InstTblEntry(RsvWords.CALL, 2, (byte) 0xBC, SyntaxAnalyzer.M6),
      new InstTblEntry(RsvWords.IN, 2, (byte) 0xC0, SyntaxAnalyzer.M3),
      new InstTblEntry(RsvWords.OUT, 2, (byte) 0xC3, SyntaxAnalyzer.M3),
      new InstTblEntry(RsvWords.PUSH, 1, (byte) 0xD0, SyntaxAnalyzer.M2),
      new InstTblEntry(RsvWords.PUSHF, 1, (byte) 0xDD, SyntaxAnalyzer.M1),
      new InstTblEntry(RsvWords.POP, 1, (byte) 0xD2, SyntaxAnalyzer.M2),
      new InstTblEntry(RsvWords.POPF, 1, (byte) 0xDF, SyntaxAnalyzer.M1),
      new InstTblEntry(RsvWords.EI, 1, (byte) 0xE0, SyntaxAnalyzer.M1),
      new InstTblEntry(RsvWords.DI, 1, (byte) 0xE3, SyntaxAnalyzer.M1),
      new InstTblEntry(RsvWords.RET, 1, (byte) 0xEC, SyntaxAnalyzer.M1),
      new InstTblEntry(RsvWords.RETI, 1, (byte) 0xEF, SyntaxAnalyzer.M1),
      new InstTblEntry(RsvWords.HALT, 1, (byte) 0xFF, SyntaxAnalyzer.M1),
      new InstTblEntry(RsvWords.EQU, 0, (byte) 0xFF, SyntaxAnalyzer.P1),
      new InstTblEntry(RsvWords.DS, 0, (byte) 0xFF, SyntaxAnalyzer.P1),
      new InstTblEntry(RsvWords.ORG, 0, (byte) 0xFF, SyntaxAnalyzer.P1),
      new InstTblEntry(RsvWords.DC, 0, (byte) 0xFF, SyntaxAnalyzer.P1)};

  // 命令表のエントリーを返す。
  static InstTblEntry getEntry(RsvWords w) {
    for (int i = 0; i < tbl.length; i++) {
      if (tbl[i].getMnemonic() == w)
        return tbl[i];
    }
    return null;
  }
}
