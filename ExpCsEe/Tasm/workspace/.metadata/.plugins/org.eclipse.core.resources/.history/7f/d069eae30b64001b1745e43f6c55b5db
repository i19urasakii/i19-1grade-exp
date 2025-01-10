/*
 * 作成日: 2004/03/31
 *
 * Copyright (c) 2004 重村哲至
 * All rights reserved.
 */
package jp.ac.tokuyama.pico.tec6;

import java.util.*;

/**
 * @author sigemura
 *
 */
class SymTbl {
  private class Entry {
    String symbol;
    Integer value;

    Entry(String s, Integer v) {
      symbol = s;
      value = v;
    }
  }

  private Vector tbl = new Vector();
  private int length = 0;

  // 名前から値を求める。
  Integer getValue(String s) {
    for (int i = 0; i < length; i++) {
      Entry e = (Entry) tbl.get(i);
      if (e.symbol.compareTo(s) == 0) {
        return e.value;
      }
    }
    return null;
  }

  // エントリーを追加
  void insert(String s, Integer v) throws AsmException {
    if (getValue(s) == null) { // ２重定義防止
      tbl.add(new Entry(s, v));
      length++;
    } else {
      throw new AsmException(Err.DBL_Idt);
    }
  }

  // 中身のダンプ
  public String toString() {
    StringBuffer s = new StringBuffer();

    for (int i = 0; i < length; i++) {
      Entry e = (Entry) tbl.get(i);
      s.append(e.symbol + " : " + e.value + "\n");
    }
    return s.toString();
  }
}
