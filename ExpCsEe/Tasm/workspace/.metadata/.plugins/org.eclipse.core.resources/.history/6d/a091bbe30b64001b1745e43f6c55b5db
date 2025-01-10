/*
 * 作成日: 2004/03/30
 *
 * Copyright (c) 2004 重村哲至
 * All rights reserved.
 */
package jp.ac.tokuyama.pico.tec6;

/**
 * @author sigemura
 * エラーを表現するクラス
 */
class Err {
  private String msg;

  final static Err BAD_Hex = new Err("不正な１６進数");
  final static Err BAD_Chr = new Err("不正な文字定数");
  final static Err BAD_Str = new Err("不正な文字列");
  final static Err BAD_Inp = new Err("不正な文字");
  final static Err BAD_Reg = new Err("不正なレジスタ指定");
  final static Err BAD_IReg = new Err("不正なインデクスレジスタ指定");
  final static Err BAD_Lab = new Err("不正なラベル");
  final static Err BAD_Exp = new Err("不正な数式");
  final static Err BAD_Opr = new Err("不正なオペランド");
  final static Err BAD_Org = new Err("不正なORG命令");
  final static Err UND_Idt = new Err("未定義ラベル");
  final static Err UND_Op = new Err("未知のニーモニック");
  final static Err UNB_Br = new Err("')'が見つからない");
  final static Err Exp_Cmm = new Err("カンマが見つからない");
  final static Err DBL_Idt = new Err("ラベルの２重定義");
  final static Err BUG_BUG = new Err("プログラムにバグ");

  // コンストラクタ
  private Err(String m) {
    msg = m;
  }

  // エラーメッセージの取り出し
  String getMsg() {
    return msg;
  }

  public String toString() {
    return msg;
  }
}
