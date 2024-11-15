# 学生実験用の TeC7 (TeC7CE)

学生実験でTeCのCPUを実装するためのVHDLソースです．
CPU内部が実装してありません．
Cpu.vhdの内容を自分で作成して完成してください．

コンソールの操作方法は本物の TeC と概ね同じですが，
CPU のデバッグに便利なようにフラグの書き換えができるようになっています．
ロータリースイッチをMMから更に右に移動しようとすると，
ロータリースイッチの位置を表す黄色のランプが全て点灯します．
この状態でWRITEボタンを押すことで，
フラグにデータスイッチに値を書き込むことができます．
CフラグがD2，SフラグがD1，ZフラグがD0に対応します．

49.1520MHz(RAMが逆相なので実質98.3040MHz)で動作させます．
組み合わせ回路に許される遅延は基本的に約20ns以内です．
RAMとのインタフェース部分は約10ns以内です．
真面目に設計しないと制約を満たすことができません．

## 注意

本設計データを Xilinx ISE でコンパイルすると下の警告が表示されます．
これは，RAM の初期化データを暗号化する場合に問題が発生するので表示されるものです．
本設計データでは暗号化していないので，この警告は無視しても問題ありません．

```
WARNING:PhysDesignRules:2410 - This design is using one or more 9K Block RAMs
   (RAMB8BWER).  9K Block RAM initialization data, both user defined and
   default, may be incorrect and should not be used.  For more information,
   please reference Xilinx Answer Record 39999
```

## レポジトリの内容

```
+ README.md                   このファイル自身
|
+ Vhdl -------+               演習用に配布する設計データ
|             |
|             + Cpu.vhd       このファイルに CPU の機能を＊＊自分で＊＊実装する
|             |
|             + TeC.vhd       TeC のトップレベルを記述した VHDL
|             |
|             + Console.vhd   コンソールパネルの機能を実装した VHDL
|             |
|             + Ram.vhd       TeC主記憶用の256バイトRAMを記述したVHDL
|             |
|             + Dcm.vhd       外部 9.8304MHz クロックから 49.1520MHz を発生
|             |
|             + TeC7a.ucf     TeC7aボード用の制約ファイル
|             |
|             + TeC7.bit      配布設計データのコンパイル結果
|
+ VhdlExample +               Cpu.vhd の実装例
|             |
|             + Cpu.vhd       CPU の実装例
|             |
|             + Sequencer.vhd CPU の制御回路実装例
|             |
|             + TeC7.bit      実装例のコンパイル結果
|
+ Doc --------+               資料
              |
              + BlockDiagram  トップレベル(TeC.vhd)をブロック図で表現したもの
```

## TeC7CE のブロック図

![TeC7CEのブロック図](https://github.com/tctsigemura/TeC7CE/blob/master/Doc/BlockDiagram.png?raw=true "ブロック図")
