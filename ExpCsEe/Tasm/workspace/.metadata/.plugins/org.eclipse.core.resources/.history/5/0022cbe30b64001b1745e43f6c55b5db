/*
 * 作成日: 2004/04/02
 *
 * Copyright (c) 2004 重村哲至
 * All rights reserved.
 */
package jp.ac.tokuyama.pico.tec6;

import java.io.*;

/**
 * @author sigemura
 *
 */
class ListFormatter {
  BufferedWriter out;
  int lc;
  int w, l;
  int page = 0;

  ListFormatter(BufferedWriter bw, int w, int l) {
    out = bw;
    this.w = w;
    this.l = lc = l;
  }

  private void out(String line) {
    try {
      // 改ページ処理
      if (lc == l) {
        out.write(
          "ADR  CODE          Label   Instruction             Comment");
        out.write("              Page(" + (++page) + ")\n");
        out.newLine();
        lc = 2;
      }

      out.write(line);
      out.newLine();
      lc++;
    } catch (IOException e) {
      System.err.println("リストファイルの書き込みエラー");
      System.exit(1);
    }
  }

  final static private String hexStr = "0123456789ABCDEF";
  static String byteToHex(byte b) {
    return Character.toString(hexStr.charAt((b >> 4) & 0x0f))
      + Character.toString(hexStr.charAt(b & 0x0f));
  }

  void output(byte adr, byte[] bin, String src, int len) {
    StringBuffer line = objCode(adr, bin, len, 0);

    int t = line.length();
    for (int i = 0; i < 19 - t; i++) {
      line.append(' ');
    }

    line.append(src);

    // １行の長さ制限
    int length = 0;
    int last = 0;

    // １行に収まる最後の文字を探す。
    for (int i = 0; i < line.length(); i++) {
      last = i;
      if (line.charAt(i) > 0xff)
        length += 2;
      else
        length++;
      if (length >= w) {
        line.delete(last + 1, line.length());
        break;
      }
    }

    out(line.toString());

    len -= 4;
    adr += 4;
    int off = 4;
    while (len > 0) {
      line = objCode(adr, bin, len, off);
      out(line.toString());
      len -= 4;
      adr += 4;
      off += 4;
    }
  }

  private StringBuffer objCode(byte adr, byte[] bin, int len, int off) {
    StringBuffer line = new StringBuffer(byteToHex(adr));
    line.append(' ');
    line.append(' ');

    for (int i = 0; i < (len < 4 ? len : 4); i++) {
      if (bin != null) {
        line.append(byteToHex(bin[i + off]));
        line.append(' ');
      } else {
        line.append("XX ");
      }
    }
    return line;
  }
}
