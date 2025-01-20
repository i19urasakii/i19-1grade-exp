-- Cpu.vhd
-- 情報電子工学総合実験(CE1)用 TeC の CPU 部分
--
-- (c)2014 - 2024 by Dept. of Computer Science and Electronic Engineering,
--            Tokuyama College of Technology, JAPAN

--　以下を自分たちのデータパスに合わせて修正

-- ライブラリのインポート
library ieee;
use ieee.std_logic_1164.all; -- 標準論理信号の定義
use ieee.std_logic_arith.all;-- 算術操作用の信号型
use ieee.std_logic_unsigned.all;-- 符号なし数の演算

-- CPUのエンティティ定義
entity Cpu is
  Port ( Clk     : in  std_logic;  -- クロック信号
         -- 制御
         Reset   : in  std_logic;  -- リセット信号
         Stop    : in  std_logic;  -- 停止信号
         Halt    : out std_logic;  -- 停止状態出力
         Li      : out std_logic;                       -- 命令フェッチ
         Flags   : out std_logic_vector (2 downto 0);   -- CSZ
         -- RAM
         Addr    : out std_logic_vector (7 downto 0);  -- アドレス出力
         Din     : in  std_logic_vector (7 downto 0);  -- データ入力
         Dout    : out std_logic_vector (7 downto 0);  -- データ出力
         We      : out std_logic;  -- 書き込み信号
         -- Console
         DbgAin  : in  std_logic_vector (2 downto 0);  -- デバッグ用アドレス入力
         DbgDin  : in  std_logic_vector (7 downto 0);  -- デバッグ用データ入力
         DbgDout : out std_logic_vector (7 downto 0);  -- デバッグ用データ出力
         DbgWe   : in  std_logic  -- デバッグ用書き込み信号
         );
end Cpu;


-- CPUの内部動作を定義するアーキテクチャ部分
architecture Behavioral of Cpu is
  -- サブコンポーネント（シーケンサ）の定義
  component Sequencer is
    Port ( Clk   : in  STD_LOGIC;
           -- 入力
           Reset : in  STD_LOGIC;
           OP    : in  STD_LOGIC_vector (3 downto 0);
           Rd    : in  STD_LOGIC_vector (1 downto 0);
           Rx    : in  STD_LOGIC_vector (1 downto 0);
           Flag  : in  STD_LOGIC_vector (2 downto 0);   -- CSZ
           Stop  : in  STD_LOGIC;
           -- CPU内部の制御用に出力
           IRLd  : out  STD_LOGIC; -- 命令レジスタのロード信号
           DRLd  : out  STD_LOGIC; -- データレジスタのロード信号
           FLLd : out  STD_LOGIC; -- フラグレジスタのロード信号
           PCLd  : out  STD_LOGIC; -- プログラムカウンタのロード信号

           --DataALU_OP : out  STD_LOGIC_vector (3 downto 0); -- DataALUの入力
           SPop : out  STD_LOGIC_vector (1 downto 0);  -- スタックポインタの操作信号

           GRsel : out  STD_LOGIC_vector (2 downto 0); -- レジスタの選択信号
           PCSel : out  STD_LOGIC_vector (1 downto 0); -- プログラムカウンタの選択信号
           DoutSel: out  STD_LOGIC_vector (1 downto 0);  -- データバス入力の選択信号
           AddrSel: out  STD_LOGIC_vector (2 downto 0); -- アドレスバスの選択信号
           SPsel  : out  STD_LOGIC_vector (1 downto 0);  -- スタックポインタの選択信号

           -- CPU外部へ出力
           We    : out  STD_LOGIC;  -- 書き込み信号
           Halt  : out  STD_LOGIC   -- 停止信号
           );
  end component;


-- CPU Register (レジスタ定義)
  signal G0  : std_logic_vector(7 downto 0);
  signal G1  : std_logic_vector(7 downto 0);
  signal G2  : std_logic_vector(7 downto 0);
  signal SP  : std_logic_vector(7 downto 0);
-- PSW
  signal PC  : std_logic_vector(7 downto 0);
  signal FLG : std_logic_vector(2 downto 0);            -- CSZ

-- IR (命令レジスタ (IR) とオペコード)
  signal OP  : std_logic_vector(3 downto 0);
  signal Rd  : std_logic_vector(1 downto 0);
  signal Rx  : std_logic_vector(1 downto 0);

-- オペコード (定数定義：各操作コードに対応するビットパターン)
  constant OP_NO  : std_logic_vector(3 downto 0) := "0000"; -- 0
  constant OP_LD  : std_logic_vector(3 downto 0) := "0001"; -- 1
  constant OP_ST  : std_logic_vector(3 downto 0) := "0010"; -- 2
  constant OP_ADD : std_logic_vector(3 downto 0) := "0011"; -- 3
  constant OP_SUB : std_logic_vector(3 downto 0) := "0100"; -- 4
  constant OP_CMP : std_logic_vector(3 downto 0) := "0101"; -- 5
  constant OP_AND : std_logic_vector(3 downto 0) := "0110"; -- 6
  constant OP_OR  : std_logic_vector(3 downto 0) := "0111"; -- 7
  constant OP_XOR : std_logic_vector(3 downto 0) := "1000"; -- 8
  constant OP_SFT : std_logic_vector(3 downto 0) := "1001"; -- 9
  constant OP_JMP : std_logic_vector(3 downto 0) := "1010"; -- A
  constant OP_CALL: std_logic_vector(3 downto 0) := "1011"; -- B
  constant OP_STCK: std_logic_vector(3 downto 0) := "1101"; -- D
  constant OP_RET : std_logic_vector(3 downto 0) := "1110"; -- E
  constant OP_HALT: std_logic_vector(3 downto 0) := "1111"; -- F

-- DR(データレジスタ（DR）の定義)
  signal DR  : std_logic_vector(7 downto 0);

-- 内部バス
  signal EA    : std_logic_vector(7 downto 0); -- Effective Address
  signal RegRd : std_logic_vector(7 downto 0); -- Reg[Rd]
  signal RegRx : std_logic_vector(7 downto 0); -- Reg[Rx]
  signal Alu   : std_logic_vector(8 downto 0); -- ALU出力（キャリー付)
  signal Zero  : std_logic;                    -- ALUが0か？
  signal SftRd : std_logic_vector(8 downto 0); -- RegRdをシフトしたもの

-- 内部制御線（ステートマシンの出力)
  signal IRLd  : std_logic;                    -- IR:Ld
  signal DRLd  : std_logic;                    -- DR:Ld
  signal FLLd : std_logic;                    -- Flag:Ld
  signal PCLd  : std_logic;                    -- GR:Ld
  --signal DataALU_OP : std_logic_vector(3 downto 0); -- DataALUの命令
  signal SPop : std_logic_vector(1 downto 0);  -- SP:op
  signal GRSel : std_logic_vector(2 downto 0); -- GR:sel
  signal PCSel : std_logic_vector(1 downto 0); -- PC:sel
  signal DoutSel: std_logic_vector(1 downto 0);  -- Dout:sel
  signal AddrSel: std_logic_vector(2 downto 0); -- Addr:sel
  signal SPsel  : std_logic_vector(1 downto 0);  -- SP:sel

  -- Muxの出力信号(データが入る？)
  signal Mux0_out : std_logic_vector(7 downto 0);
  signal Mux1_out : std_logic_vector(7 downto 0);
  signal Mux2_out : std_logic_vector(7 downto 0);
  signal Mux3_out : std_logic_vector(7 downto 0);
  signal Mux4_out : std_logic_vector(7 downto 0);
  --signal Mux5_out : std_logic_vector(7 downto 0);
  -- DataALUの出力信号
  --signal DataALU_out : std_logic_vector(7 downto 0); -- 8-0
  signal AddrADD_out : std_logic_vector(7 downto 0); -- 8-0

  signal Ma    : std_logic_vector(1 downto 0); -- MA(PC=00,EA=01,SP=10)
  signal Md    : std_logic;                    -- MD(PC=0,GR=1)


-- ここからアーキテクチャ部分の記述が始まる
begin

-- 仮の信号を出力しておく(コメント行にしたらエラー消えた)
  -- Halt <= '0';
  -- Addr <= "00000000";
  -- Dout <= "00000000";
  -- We   <= '0';
  -- Li   <= '0';

-- コンソールへの接続
  Flags <= FLG;
  Li    <= IRLd;
-- フラグ状態を外部出力。
-- Li は命令フェッチを示す信号で、IR(命令レジスタ)のロード時に立ち上がる


-- 制御部
-- 制御部を構成するモジュールで、クロックやリセット信号を基に各制御信号を生成。
  seq1: Sequencer Port map (Clk, Reset, OP, Rd, Rx, FLG, Stop,
                            IRLd, DRLd, FLLd, PCLd, SPop,
                            GRSel, PCSel, DoutSel, AddrSel, SPSel,
                            We, Halt);

-- BUS
  Addr <= Mux2_out; -- メモリアドレス選択
  Dout <= Mux1_out; -- データ

  -- ???
  --EA <= DR + RegRx; -- 有効アドレス計算 (データレジスタ + 選択されたレジスタ)
  


-- MUXの動作定義
-- MUX0(PCSel)
  -- PCに格納するデータを選択
  Mux0_out <= AddrADD_out when PCSel="01" else -- 0
              Din when PCSel="10" else         -- 1
              PC+'1' when PCSel="11";          -- 2                 
  -- Doutに格納するデータを選択
  Mux1_out <= Mux4_out when DoutSel="01" else --0
              PC+'1' when DoutSel="10";       -- 1          
  -- Addrに格納するデータを選択
  Mux2_out <= PC when AddrSel="001" else           -- 0
              PC+'1' when AddrSel="010" else       -- 1
              AddrADD_out when AddrSel="011" else  -- 2
              SP when AddrSel="100" else           -- 3
              SP+'1';                              -- 4
  -- インデクスドモード：レジスタ選択
  Mux3_out <= "00000000" when Rx="00" else -- 0
              G1 when Rx="01" else -- 1
              G2 when Rx="10" else -- 2
              "00000000";          -- 3
  -- レジスタ選択
  Mux4_out <= G0 when Rd="00" else -- 0
              G1 when Rd="01" else -- 1
              G2 when Rd="10" else -- 2
              SP;                  -- 3
  -- SPに格納するデータを選択
  -- Mux5_out <= (SP+'1') when (SPop = "01" and SPSel = '0') else
  --             (SP-'1') when (SPop = "10" and SPSel = '0') else
  --             DataALU_out when SPSel = '1';


  -- レジスタ選択????
  --GRSel <= Rd

-- AddrADD BUS
  AddrADD_out <= DR + Mux3_out;
  --DataALU_out <= Alu(8 downto 1);


-- DataALU BUS
  -- ALU (演算論理ユニット) の動作定義
  SftRd <= (Mux4_out & '0') when Rx(1)='0' else                      -- 左シフト(論理も算術も同じ)　SHLA/SHLL
    (Mux4_out(0) & Mux4_out(7) & Mux4_out(7 downto 1)) when Rx(0)='0' else -- 算術右シフト　SHRA
    (Mux4_out(0) & '0' & Mux4_out(7 downto 1));                         -- 論理右シフト　SHRL
  
  Alu <= ('0' & Mux4_out) + ('0' & DR) when OP=OP_ADD else -- 加算
         ('0' & Mux4_out) - ('0' & DR) when OP=OP_SUB or OP=OP_CMP else -- 減算または比較
         ('0' & Mux4_out)and('0' & DR) when OP=OP_AND else -- 論理積
         ('0' & Mux4_out)or ('0' & DR) when OP=OP_OR  else -- 論理和
         ('0' & Mux4_out)xor('0' & DR) when OP=OP_XOR else -- 排他的論理和
         SftRd when OP=OP_SFT else ('0' & DR); -- シフト演算 else データレジスタ

  Zero <= '1' when ALU(7 downto 0)="00000000" else '0';   -- ゼロフラグ設定


  -- PC の制御
  process(Clk, Reset)
  begin
    if (Reset='1') then
      PC <= "00000000";
    elsif (Clk'event and Clk='1') then
      if (PCLd='1') then
        PC <= Mux0_out;
      end if;
    end if;
  end process;

  -- IR,DR の制御(ロード処理)
  process(Clk)
  begin
    if (Clk'event and Clk='1') then
      if (IRLd='1') then
        OP <= Din(7 downto 4); -- 命令の操作コード
        Rd <= Din(3 downto 2); -- デスティネーションレジスタ
        Rx <= Din(1 downto 0); -- ソースレジスタ
      end if;
      if (DRLd='1') then
        DR <= Din;  -- データレジスタのロード
      end if;
    end if;
  end process;

  -- CPU レジスタの制御
  process(Clk, Reset)
  begin
    if (Reset='1') then
      G0  <= "00000000";         -- 全レジスタを0にリセット
      G1  <= "00000000";
      G2  <= "00000000"; 
      SP  <= "00000000";
    elsif (Clk'event and Clk='1') then
      if (GRSel="001" or GRSel="010" or GRSel="011" or GRSel="100") then --レジスタが選択されてたら
        case GRSel is
          when "001" => G0 <= Alu(7 downto 0); -- G0にALU結果を格納
          when "010" => G1 <= Alu(7 downto 0); -- G1にALU結果を格納
          when "011" => G2 <= Alu(7 downto 0); -- G2にALU結果を格納
          when "100" => SP <= Alu(7 downto 0); -- SPにALU結果を格納
          when others => null;        -- その他の場合は無処理
        end case;
      -- 先にSPのif文にした方がいい？SPで計算があった時はSPに格納するようにする
      else
        --SP <= Mux5_out;
        case SPop is
          when "00" => SP <= SP; -- SPをそのまま
          when "01" => SP <= SP + 1; -- SPをインクリメント
          when "10" => SP <= SP - 1; -- SPをデクリメント
          when "11" => SP <= SP; -- SPをそのまま
          when others => null;        -- その他の場合は無処理
        end case;
      end if;

      if (DbgWe='1') then
        case DbgAin is
          when "000" => G0 <= DbgDin; -- デバッグ入力でG0を設定
          when "001" => G1 <= DbgDin; -- デバッグ入力でG1を設定
          when "010" => G2 <= DbgDin; -- デバッグ入力でG2を設定
          when "011" => SP <= DbgDin; -- デバッグ入力でSPを設定
          when others => null;        -- その他の場合は無処理
        end case;
      end if;
    end if;
  end process;


  -- フラグの制御(追記：FLLd=1の時にフラグを設定)
  process(Clk, Reset)
  begin
    if (Reset='1') then
      FLG <= "000"; -- 全フラグをリセット
    elsif (Clk'event and Clk='1') then
      if (FLLd='1') then
        FLG(2) <= Alu(8);                -- Carry
        FLG(1) <= Alu(7);                -- Sign
        FLG(0) <= Zero;                  -- Zero
      elsif (DbgWe='1' and DbgAin="110" and FLLd='1') then
        FLG <= DbgDin(2 downto 0); -- デバッグ入力でフラグを設定
      end if;
    end if;
  end process;


  
-- CPU レジスタの制御(そのまま)
  -- RegRd <= G0 when Rd="00" else  -- G0レジスタを選択
  --          G1 when Rd="01" else  -- G1レジスタを選択
  --          G2 when Rd="10" else  -- G2レジスタを選択
  --          SP;                   -- その他の場合スタックポインタを選択
  -- RegRx <= G1 when Rx="01" else  -- G1レジスタを選択
  --          G2 when Rx="10" else  -- G2レジスタを選択
  --          "00000000";           -- その他の場合0を選択
  
-- デバッグ用のコンソール接続
  DbgDout <= G0 when DbgAin="000" else  -- G0出力
             G1 when DbgAin="001" else  -- G1出力
             G2 when DbgAin="010" else  -- G2出力
             SP when DbgAin="011" else  -- SP出力
             PC when DbgAin="100" else  -- PC出力
             "00000" & FLG;

end Behavioral;
