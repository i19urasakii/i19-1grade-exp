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
final class TokenType {
  private String name;
  final static TokenType SPC = new TokenType("SPC"); // Space
  final static TokenType EOF = new TokenType("EOF"); // End Of File
  final static TokenType EOL = new TokenType("EOL"); // End Of Line
  final static TokenType COM = new TokenType("Comment"); // Comment
  final static TokenType NUM = new TokenType("Number"); // Number
  final static TokenType STR = new TokenType("String"); // String
  final static TokenType NAM = new TokenType("Name"); // Name
  final static TokenType LBR = new TokenType("("); // Left Bracket
  final static TokenType RBR = new TokenType(")"); // Right Bracket
  final static TokenType CMM = new TokenType("Comma"); // Comma
  final static TokenType PLS = new TokenType("Pluse"); // Pluse
  final static TokenType MNS = new TokenType("Minus"); // Minus
  final static TokenType MUL = new TokenType("Multiple"); // Multiple
  final static TokenType DIV = new TokenType("Divide"); // Divide
  final static TokenType IMM = new TokenType("Immediate"); // Immediate
  final static TokenType RSV = new TokenType("Word"); // Reserved Word
  final static TokenType ERR = new TokenType("Error"); // Error

  private TokenType(String n) {
    name = n;
  }

  public String toString() {
    return name;
  }
}
