/*
  * 作成日: 2004/03/30
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
class Lexer {
  private BufferedReader in; // ソースファイル
  private StringBuffer line = new StringBuffer(); // 現在行のリスト出力
  private int nextch; // 次の文字
  private int nextCH; // nextCH を大文字に変換したもの
  private int cur = 0; // カーソル
  private int tab = 0; // タブストップ
  private String curLine = null; // 現在の入力行
  private int    curPos  = 0;    // 入力行の中で何文字目

  // 入力からの１文字入力
  private int getc() throws IOException {
    if (curLine == null) {
      curLine = in.readLine();
    }
    if (curLine == null) {
      return -1;
    }
    int len = curLine.length();
    int ret;
    if (len > curPos) {
      ret = curLine.charAt(curPos); 
      curPos++;    
    } else {
      ret = '\n';
      curPos = 0;
      curLine = null;
    }
    return ret;
  }
  
  // 大文字小文字の nextch を準備する
  private int setNext(int c) {
    nextch = c;
    if ('a' <= nextch && nextch <= 'z')
      nextCH = Character.toUpperCase((char) nextch);
    else
      nextCH = nextch;
    return nextCH;
  }
  
  // 次の１文字を読み込む。
  private void getnext() throws IOException {
    if (cur < tab) {
      setNext(' ');
      cur++;
    } else {
      if (nextCH == '\n' || nextCH == -1) {
        line = new StringBuffer();
      }
      setNext(getc());
      if (nextCH == '\n') {
        cur = 0;
        tab = 0;
      } else if (nextCH == '\t') {
        tab = ((cur + 8) / 8) * 8;
        setNext(' ');
        cur++;
      } else if (nextCH != -1) {
        cur++;
      }
    }
    if (nextCH != '\n' && nextCH != -1)
      line.append((char) nextCH);
  }

  // 現在の行を返す(エラー時に使用)
  String getLine() {
    try {
      while (nextCH != -1 && nextCH != '\n') {
        setNext(getc());
        if (nextCH != -1 && nextCH != '\n')
          line.append((char) nextCH);
      }
    } catch (IOException e) {
      // エラー処理中なので無視する。
    }
    return line.toString();
  }

  // 空白まで読み飛ばす。
  private String skipToSPC() throws IOException {
    StringBuffer src = new StringBuffer();
    while (nextCH != ' ' && nextCH != '\t' && nextCH != '\n' && nextCH != -1) {
      src.append((char) nextCH);
      getnext();
    }
    return src.toString();
  }

  // 行末まで読み飛ばす。
  private String skipLine() throws IOException {
    StringBuffer src = new StringBuffer();
    while (nextCH != -1 && nextCH != '\n') {
      src.append((char) nextCH);
      getnext();
    }
    return src.toString();
  }

  // 空白を読み飛ばす。
  private void skipSpc() throws IOException {
    while ((char) nextCH == ' ' || (char) nextCH == '\t')
      getnext();
  }

  // １文字の記号を読み込む。
  private Token addAndSkip(TokenType t, String s) throws IOException {
    Token token = new Token(t, s);
    getnext();
    return token;
  }

  // エラーを発見したとき空白までをエラートークンに読み込む。
  private Token addErr(StringBuffer s, Err e) throws IOException {
    s.append(skipToSPC());
    skipLine();
    return new Token(TokenType.ERR, s.toString(), e);
  }

  // １６進数を数値として読み込む。
  private Token addHex(StringBuffer s) throws IOException {
    int v = 0;

    for (int i = 0; i < s.length() - 1; i++) { // 最後の H を無視する。
      char c = s.charAt(i);
      v = v * 16 + Character.digit(c, 16);
    }
    return new Token(TokenType.NUM, s.toString(), new Integer(v));
  }

  // １０進数を数値として登録する。
  private Token addDec(StringBuffer s) throws IOException {
    int v = 0;

    for (int i = 0; i < s.length(); i++) {
      char c = s.charAt(i);
      v = v * 10 + Character.digit(c, 10);
    }
    return new Token(TokenType.NUM, s.toString(), new Integer(v));
  }

  // 数値を読み込み１０進数か１６進数か判定して登録する。
  private Token getNum() throws IOException {
    boolean hex = false;
    StringBuffer src = new StringBuffer();
    src.append((char) nextCH);
    getnext();

    while (true) {
      if ('A' <= (char) nextCH && (char) nextCH <= 'F') {
        hex = true;
      } else if ('0' <= (char) nextCH && (char) nextCH <= '9') {
        ;
      } else {
        break;
      }
      src.append((char) nextCH);
      getnext();
    }

    if ((char) nextCH == 'H') {
      hex = true;
      src.append((char) nextCH);
      getnext();
    } else if (hex) {
      return addErr(src, Err.BAD_Hex);
    }

    if (hex) {
      return addHex(src);
    }
    return addDec(src);
  }

  // 文字定数 'C' を読み込み登録する。
  private Token getChar() throws IOException {
    StringBuffer src = new StringBuffer();
    src.append((char) nextCH);
    getnext();

    if (0x20 <= nextCH && nextCH <= 0x7f && (char) nextCH != '\'') {
      int c = nextch;
      src.append((char) nextch);
      getnext();

      if ((char) nextCH == '\'') {
        src.append((char) nextCH);
        getnext();
        return new Token(TokenType.NUM, src.toString(), new Integer(c));
      }
    }
    return addErr(src, Err.BAD_Chr);
  }

  // 文字列 "aaa" を読み込み登録する。
  private Token getStr() throws IOException {
    StringBuffer src = new StringBuffer();
    src.append((char) nextCH);
    getnext();

    while (0x20 <= nextCH && nextCH <= 0x7f && (char) nextCH!= '"') {
      src.append((char) nextch);
      getnext();
    }

    if ((char) nextCH == '"') {
      src.append((char) nextCH);
      getnext();
      String s = src.toString();
      return new Token(TokenType.STR, s, s);
    }

    return addErr(src, Err.BAD_Str);
  }

  // 予約語または名前を読み込み登録する。
  private Token getName(boolean rsvWrd) throws IOException {
    StringBuffer src = new StringBuffer();
    src.append((char) nextCH);
    getnext();

    while ((char) nextCH == '_'
      || 'A' <= (char) nextCH && (char) nextCH <= 'Z'
      || '0' <= (char) nextCH && (char) nextCH <= '9') {
      src.append((char) nextCH);
      getnext();
    }

    String s = src.toString().toUpperCase();

    if (rsvWrd) {
      RsvWords w = RsvWords.search(s);
      if (w != null) {
        return new Token(TokenType.RSV, s, w);
      }
    }

    return new Token(TokenType.NAM, s, s);
  }

  // コンストラクタ
  Lexer(BufferedReader br) throws IOException {
    in = br;
    getnext();
  }

  // EOF を表すトークン
  private final static Token EOF = new Token(TokenType.EOF);

  // 次のトークンを入力から取り出す。
  private boolean eolFlg = true;
  private Token readToken(boolean rsvWrd) throws IOException {
    // 行の先頭以外ならトークンの前の空白をスキップする
    if (!eolFlg)
      skipSpc();
    eolFlg = false;

    // EOF になった
    if (nextCH==-1)
      return EOF;
      
    // 数値、名前、予約語以外
    switch ((char) nextCH) {
      case ' ' :
        skipSpc();
        return new Token(TokenType.SPC);

      case '\n' :
        getnext();
        eolFlg = true;
        return new Token(TokenType.EOL);

      case ';' :
        return new Token(TokenType.COM, skipLine());

      case '+' :
        return addAndSkip(TokenType.PLS, "+");

      case '-' :
        return addAndSkip(TokenType.MNS, "-");

      case '*' :
        return addAndSkip(TokenType.MUL, "*");

      case '/' :
        return addAndSkip(TokenType.DIV, "/");

      case '(' :
        return addAndSkip(TokenType.LBR, "(");

      case ')' :
        return addAndSkip(TokenType.RBR, ")");

      case ',' :
        return addAndSkip(TokenType.CMM, ",");

      case '#' :
        return addAndSkip(TokenType.IMM, "#");

      case '\'' :
        return getChar();

      case '"' :
        return getStr();

      default :
        break;
    }

    // 数値、名前、予約語      
    if ('0' <= (char) nextCH && (char) nextCH <= '9') {
      return getNum();
    } else if (
      'A' <= (char) nextCH && (char) nextCH <= 'Z' || (char) nextCH == '_') {
      return getName(rsvWrd);
    }
    return addErr(new StringBuffer(), Err.BAD_Inp);
  }

  // 次のトークン  
  private Token nextTok = null;

  // リスト整形は有効／無効
  private boolean listOutput = false;

  // 次のトークンをセットする。
  void setNextTok(boolean rsvWrd) throws AsmException {
    if (nextCH != -1) {
      if (listOutput && nextTok != null)
        formatList();
      try {
        nextTok = readToken(rsvWrd);
      } catch (IOException e) {
        nextTok = EOF;
      }
    } else if (nextTok != EOF) {
      if (listOutput && nextTok != null)
        formatList();
      nextTok = EOF;
    }

    if (nextTok.getType() == TokenType.ERR)
      throw new AsmException((Err) nextTok.getValue());

    //System.out.println(nextTok.toString());
  }

  // 次のトークンを参照する。
  Token getNextTok() {
    return nextTok;
  }

  // リスト整形処理関連
  private StringBuffer listLine;

  // リスト整形処理を有効化／無効化する。
  void setListOutput(boolean f) {
    listOutput = f;
    listLine = new StringBuffer();
  }

  // トークンを取り出す時に呼ばれる。
  private void formatList() {
    if (nextTok.getType() == TokenType.EOL) {
      listLine = new StringBuffer();
    } else {
      String src = nextTok.getSrc();
      if (src != null)
        listLine.append(src);
    }
  }

  // リスト行中のカーソルを p に移動する。
  void setListCur(int p) {
    int l = listLine.length();
    if (l >= p)
      listLine.append(' ');
    else {
      for (int i = 0; i < p - l; i++)
        listLine.append(' ');
    }
  }

  // 整形済みリストを返す。
  String getListLine() {
    return listLine.toString();
  }
}
