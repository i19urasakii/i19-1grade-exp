-- Console.vhd
-- 情報電子工学総合実験(CE1)用 TeC のコンソールパネル部分
--
-- (c)2014 - 2024 by Dept. of Computer Science and Electronic Engineering,
--            Tokuyama College of Technology, JAPAN

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity Console is
  Port ( Locked  : in  std_logic;
         Clk     : in  std_logic;                 -- 49.1520MHz
         Reset   : out std_logic;
         Stop    : out std_logic;
         Halt    : in  std_logic;
         Li      : in  std_logic;
         Flags   : in  std_logic_vector (2 downto 0);  -- CSZ
         -- CPU と Memory の表示・書換え用配線
         Aout    : out std_logic_vector (7 downto 0);
         Dout    : out std_logic_vector (7 downto 0);
         DinCpu  : in  std_logic_vector (7 downto 0);
         DinMem  : in  std_logic_vector (7 downto 0);
         WeCpu   : out std_logic;
         WeMem   : out std_logic;
         Ain     : in  std_logic_vector (7 downto 0);
         -- Console からの入力
         DSw     : in std_logic_vector (7 downto 0);
         RstSw   : in std_logic;
         SetSw   : in std_logic;
         IncSw   : in std_logic;
         DecSw   : in std_logic;
         WrtSw   : in std_logic;
         StopSw  : in std_logic;
         BrkSw   : in std_logic;
         StepSw  : in std_logic;
         RunSw   : in std_logic;
         CwSw    : in std_logic;                  -- 右回り「→」
         CcwSw   : in std_logic;                  -- 左回り「←」
         -- Console への出力
         AddrLed : out std_logic_vector (7 downto 0);
         DataLed : out std_logic_vector (7 downto 0);
         RunLed  : out std_logic;
         CrryLed : out std_logic;
         SignLed : out std_logic;
         ZeroLed : out std_logic;
         G0Led   : out std_logic;
         G1Led   : out std_logic;
         G2Led   : out std_logic;
         SpLed   : out std_logic;
         PcLed   : out std_logic;
         MmLed   : out std_logic;
         Spk     : out std_logic
         );
end Console;

architecture Behavioral of Console is
-- 低速クロック・パルス
  signal Cnt1   : std_logic_vector(13 downto 0);  -- 4.8kHz生成用のカウンタ
  signal Cnt2   : std_logic_vector( 7 downto 0);  -- 18.75Hz生成用のカウンタ
  signal c2_4kHz: std_logic;                      -- 2.4kHz（「ピ」の音）
  signal p18_75Hz: std_logic;                     -- 18.75Hz（パルス）
  signal LckDly : std_logic;                      -- Locked をクロックに同期

-- Debounce
  signal BtnDly1: std_logic_vector(8 downto 0);   -- ９つの押しボタンで
  signal BtnDly2: std_logic_vector(8 downto 0);   --  まとめて Debounce を行う
  signal BtnDbnc: std_logic_vector(8 downto 0);

-- ボタンのリピート
  signal RptBtn : std_logic;                      -- ボタンが操作押された
  signal RptGo  : std_logic;                      -- リピート中
  signal RptCnt1: std_logic_vector(3 downto 0);   -- リピート開始タイマ
  signal RptCnt2: std_logic_vector(1 downto 0);   -- リピート間隔タイマ
  
-- 操作音
  signal Pi     : std_logic;                      -- 「ピ」を鳴らしている

-- コンソールの機能
  signal Addr   : std_logic_vector(7 downto 0);  -- メモリのアドレス
  signal Pos    : std_logic_vector(2 downto 0);  -- ロータリースイッチの位置
  signal PosDec : std_logic_vector(5 downto 0);  --   位置をデコードしたもの
  signal G0     : std_logic;                     -- G0 選択中
  signal Mm     : std_logic;                     -- MM 選択中
  signal Flg    : std_logic;                     -- FLAG 選択中
  signal Run    : std_logic;                     -- CPU 実行/停止
  signal Rst    : std_logic;                     -- 内部配線用
  signal WeM    : std_logic;                     -- 内部配線用

begin
-- 低速のクロックが必要な部分で共通のクロック生成用カウンタ
-- オリジナルTeCと同じ操作感になるように周波数を再現する
  process(Clk)
  begin
    if (Clk'event and Clk='1') then
      if (Cnt1="10011111111111") then            -- 毎秒 4.8k 回成立
        Cnt1 <= "00000000000000";
        Cnt2 <= Cnt2 + 1;
        if (Cnt2="11111111") then                -- 4.8kHz / 256 = 18.75Hz
          p18_75Hz <= '1';                       --   18.75Hz のパルスを作る
        end if;
        c2_4kHz <= not c2_4kHz;                  -- 2.4kHzの矩形波を作る
      else
        Cnt1 <= Cnt1 + 1;
        p18_75Hz <= '0';
      end if;
      LckDly  <= Locked;                         -- Locked も同期しておく
    end if;
  end process;

-- 押しボタン９個分の Debounce 回路
  process(Clk)
  begin
    if (Clk'event and Clk='1') then
      if (p18_75Hz='1') then                     -- 1/18.75秒(53ms)に1回
        if (RptGo='0' or RptCnt2/="00") then     --  リピートの瞬間以外
          BtnDly2 <= BtnDly1;                    --    押しボタンの状態を読取る
        else                                     --  リピートの瞬間
          BtnDly2 <= "000000000";                --    一旦，ボタンを戻す
        end if;
        BtnDbnc <= (not BtnDly2) and BtnDly1 and
                   ('1' & not G0 & not Flg       -- (Rst)  (<-)   (->)
                    & not Run & Run & Mm         -- (Run)  (Stop) (SetA)
                    & Mm  & Mm  & not Run);      -- (IncA) (DecA) (Write)
      else
        BtnDbnc <= "000000000";
      end if;
      BtnDly1 <= RstSw & CcwSw & CwSw            -- 押しボタンの入力を
                 & RunSw & StopSw & SetSw        --  クロックに同期しておく
                 & IncSw & DecSw & WrtSw;        -- 
    end if;
  end process;

-- リピートする押しボタンが押されているか
  RptBtn <= '1' when (BtnDly1 and
            ('0'  & '1'  & '1'  & '0'  & '0'     -- (Rst)(<-)(->)(Run)(Stop)
             & '0'  & '1'  & '1'  & Mm))         -- (SetA)(IncA)(DecA)(Write)
            /="000000000" else '0';

-- リピート継続中
  RptGo <= '1' when RptCnt1="1001" else '0';     -- 477ms 経過したら開始

-- 押しボタンのリピート
  process(Clk)
  begin
    if (Clk'event and Clk='1') then
      if (p18_75Hz='1') then                     -- 1/18.75秒(53ms)に1回
        if (RptBtn='1') then                     -- 操作されている
          if (RptGo='0') then                    --   リピート開始になってない
            RptCnt1 <= RptCnt1 + 1;              --     継続時間を測定
          elsif (RptCnt2/="10") then             --   リピート間隔測定
            RptCnt2 <= RptCnt2 + 1;
          else
            RptCnt2 <= "00";
          end if;
        else                                     -- 操作されていない
          RptCnt1 <= "0000";                     --   タイマリセット
          RptCnt2 <= "00";
        end if;
      end if;
    end if;
  end process;
  
-- 操作音
  Spk <= c2_4kHz when Pi='1' else '1';           -- 音が継続中は 2.4kHz

  process(Clk)
  begin
    if (Clk'event and Clk='1') then
      if (BtnDbnc/="000000000" or Rst='1') then  -- ボタン操作またはリセット
        Pi <= '1';                               --   「ピ」開始
      elsif (p18_75Hz='1') then                  -- 53ms 後に
        Pi <= '0';                               --   「ピ」終了
      end if;
    end if;
  end process;

-- フラグ表示（単にコンソールを通過）
  CrryLed <= not Flags(2);
  SignLed <= not Flags(1);
  ZeroLed <= not Flags(0);
  
-- RESET
  Rst <= BtnDbnc(8) or (not LckDly);             -- RESETボタンまたはDCM
  Reset <= Rst;                                  -- 外部端子に接続
  
-- アドレスとデータ出力
  Aout <= Addr when Mm='1' else "00000" & Pos;
  AddrLed <= (not Addr) when Mm='1' else "11111111";
  Dout <= DSw;

-- 表示データ選択用のマルチプレクサ
  DataLed <= (not DinMem) when Mm='1' else (not DinCpu);

-- ロータリースイッチの位置デコーダ
  G0    <= PosDec(5) and not PosDec(1);          -- G0 選択中
  Mm    <= PosDec(0) and not PosDec(1);          -- MM 選択中
  Flg   <= PosDec(0) and PosDec(1);              -- FLAG 選択中
  G0Led <= not PosDec(5);
  G1Led <= not PosDec(4);
  G2Led <= not PosDec(3);
  SpLed <= not PosDec(2);
  PcLed <= not PosDec(1);
  MmLed <= not PosDec(0);
  with Pos select PosDec <=
    "100000" when "000" ,                        -- G0
    "010000" when "001" ,                        -- G1
    "001000" when "010" ,                        -- G2
    "000100" when "011" ,                        -- SP
    "000010" when "100" ,                        -- PC
    "000001" when "101" ,                        -- MM
    "111111" when others;                        -- FLAG

-- ロータリースイッチ位置を切換える
  process(Clk, LckDly)
  begin
    if (LckDly='0') then                         -- パワーオンリセット
      Pos <= "000";
    elsif (Clk'event and Clk='1') then
      if (BtnDbnc(7)='1') then                   -- Btn7(CcwSw)
        Pos <= Pos - 1;
      elsif (BtnDbnc(6)='1') then                -- Btn6(CwSw)
        Pos <= Pos + 1;
      end if;
    end if;
  end process;

-- CPU 関連
  WeCpu <= BtnDbnc(0) when Mm='0' else '0';      -- CPU へのデータ書込み指示
  Stop <= not Run;                               -- 外部端子に接続
  RunLed <= not Run;                             -- 外部端子に接続

  process(Clk, Rst)                              -- Start/Stop の制御
  begin
    if (Rst='1') then
      Run  <= '0';
    elsif (Clk'event and Clk='1') then
      if (BtnDbnc(5)='1') then                   -- Btn5(RUN)
        Run  <= '1';
      elsif (Halt='1' or BtnDbnc(4)='1' or       -- Halt 命令または Btn4(STOP)
          (BrkSw='1' and Li='1' and DSw=Ain) or  -- BREAK 実行時
          (StepSw='1' and Li='1')) then          -- STEP 実行時
        Run  <= '0';
      end if;
    end if;
  end process;
  
-- メモリ関連
  WeM <= BtnDbnc(0) when Mm='1' else '0';        -- Btn0(WRITE)
  WeMem <= WeM;                                  -- 外部端子へ接続

  process(Clk, Rst)
  begin
    if (Rst='1') then
      Addr <= "00000000";
    elsif (Clk'event and Clk='1') then
      if (BtnDbnc(1)='1') then                   -- Btn1(DECA)
        Addr <= Addr - 1;
      elsif (BtnDbnc(2)='1' or WeM='1') then     -- Btn2(INCA)
        Addr <= Addr + 1;
      elsif (BtnDbnc(3)='1') then                -- Btn3(SETA)
        Addr <= DSw;
      end if;
    end if;
  end process;

end Behavioral;
