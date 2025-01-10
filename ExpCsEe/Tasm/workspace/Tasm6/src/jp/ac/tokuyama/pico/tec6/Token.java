/*
 * 作成日: 2004/03/30
 *
 * Copyright (c) 2004 重村哲至
 * All rights reserved.
 */
package jp.ac.tokuyama.pico.tec6;

/**
 * @author sigemura
 *
 */
class Token {
  private TokenType type;
  private String src;
  private Object value;

  Token(TokenType t, String s, Object v) {
    type = t;
    src = s;
    value = v;
  }

  Token(TokenType t, String s) {
    type = t;
    src = s;
    value = null;
  }

  Token(TokenType t) {
    type = t;
    src = null;
    value = null;
  }

  TokenType getType() {
    return type;
  }

  Object getValue() {
    return value;
  }

  String getSrc() {
    return src;
  }

  public String toString() {
    return "[Type : "
      + type
      + "]"
      + " [Value : "
      + value
      + "]"
      + " [Src : "
      + src
      + "]";
  }
}
