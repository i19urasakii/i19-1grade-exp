-- TeC.vhd
-- 情報電子工学総合実験(CE1)用 TeC のトップレベル
--
-- (c)2014 - 2019 by Dept. of Computer Science and Electronic Engineering,
--            Tokuyama College of Technology, JAPAN

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity TeC is
  Port ( CLK_IN    : in    std_logic;             -- 9.8304MHz
         -- CONSOLE(INPUT)
         DATA_SW   : in   std_logic_vector (7 downto 0);
         RESET_SW  : in   std_logic;
         SETA_SW   : in   std_logic;
         INCA_SW   : in   std_logic;
         DECA_SW   : in   std_logic;
         WRITE_SW  : in   std_logic;
         STEP_SW   : in   std_logic;
         BREAK_SW  : in   std_logic;
         STOP_SW   : in   std_logic;
         RUN_SW    : in   std_logic;
         RIGHT_SW  : in   std_logic;
         LEFT_SW   : in   std_logic;
         -- CONSOLE(OUTPUT)
         ADDR_LED  : out  std_logic_vector (7 downto 0);
         DATA_LED  : out  std_logic_vector (7 downto 0);
         RUN_LED   : out  std_logic;
         C_LED     : out  std_logic;
         S_LED     : out  std_logic;
         Z_LED     : out  std_logic;
         G0_LED    : out  std_logic;
         G1_LED    : out  std_logic;
         G2_LED    : out  std_logic;
         SP_LED    : out  std_logic;
         PC_LED    : out  std_logic;
         MM_LED    : out  std_logic;
         SPK_OUT   : out  std_logic
         );
end TeC;

architecture Behavioral of TeC is
-- クロック関係の配線
  signal i9_8304    : std_logic;
  signal Locked     : std_logic;
  signal Clk        : std_logic;
-- CPU と RAM の配線
  signal Addr       : std_logic_vector(7 downto 0);
  signal DataIn     : std_logic_vector(7 downto 0);
  signal DataOut    : std_logic_vector(7 downto 0);
  signal WeMem      : std_logic;
-- CPU と Console の配線
  signal Reset      : std_logic;
  signal Stop       : std_logic;
  signal Halt       : std_logic;
  signal Li         : std_logic;
  signal Flags      : std_logic_vector (2 downto 0);     -- CSZ
-- Console のデバッグ表示・書込み用
  signal DbgAddr    : std_logic_vector(7 downto 0);
  signal DbgDataCns : std_logic_vector(7 downto 0);
  signal DbgDataCpu : std_logic_vector(7 downto 0);
  signal DbgDataMem : std_logic_vector(7 downto 0);
  signal DbgWeCpu   : std_logic;
  signal DbgWeMem   : std_logic;

-- クロックを5倍速にする(Digital Clock Manager)
  component DCM
    port ( CLK_IN1  : in  std_logic;                     --  9.8304MHz
           CLK_OUT1 : out std_logic;                     -- 49.1520MHz
           LOCKED   : out std_logic                      -- 出力が安定した
           );
  end component;

-- コンソール
  component Console
    Port ( Locked  : in  std_logic;
           Clk     : in  std_logic;                      -- 49.1520MHz
           Reset   : out std_logic;
           Stop    : out std_logic;
           Halt    : in  std_logic;
           Li      : in  std_logic;
           Flags   : in  std_logic_vector (2 downto 0);  -- CSZ
           -- CPU と Memory 内容の表示・書換え用配線
           Aout    : out std_logic_vector (7 downto 0);
           Dout    : out std_logic_vector (7 downto 0);
           DinCpu  : in  std_logic_vector (7 downto 0);
           DinMem  : in  std_logic_vector (7 downto 0);
           WeCpu   : out std_logic;
           WeMem   : out std_logic;
           Ain     : in  std_logic_vector (7 downto 0);
           -- Console(INPUT)
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
           CwSw    : in std_logic;                       -- 右回り「→」
           CcwSw   : in std_logic;                       -- 左回り「←」
           -- Console(OUTPUT)
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
  end component;

  component Cpu
    Port ( Clk     : in  std_logic;
           -- 制御
           Reset   : in  std_logic;
           Stop    : in  std_logic;
           Halt    : out std_logic;
           Li      : out std_logic;                      -- 命令フェッチ
           Flags   : out std_logic_vector (2 downto 0);  -- CSZ
           -- RAM
           Addr    : out std_logic_vector (7 downto 0);
           Din     : in  std_logic_vector (7 downto 0);
           Dout    : out std_logic_vector (7 downto 0);
           We      : out std_logic;
           -- Console
           DbgAin  : in  std_logic_vector (2 downto 0);
           DbgDin  : in  std_logic_vector (7 downto 0);
           DbgDout : out std_logic_vector (7 downto 0);
           DbgWe   : in  std_logic
           );
  end component;

  component Ram
    Port ( Clk     : in  std_logic;
           -- CPU 用のポート
           Addr    : in  std_logic_vector (7 downto 0);
           Din     : in  std_logic_vector (7 downto 0);
           Dout    : out std_logic_vector (7 downto 0);
           We      : in  std_logic;
           -- Console 用のポート
           DbgAddr : in  std_logic_vector (7 downto 0);
           DbgDin  : in  std_logic_vector (7 downto 0);
           DbgDout : out std_logic_vector (7 downto 0);
           DbgWe   : in  std_logic
           );
  end component;

begin
  DCM1 : DCM
    port map ( CLK_IN1 => CLK_IN, CLK_OUT1 => Clk, LOCKED => Locked );

  Console1 : Console
    port map (
      Locked  => Locked,                                 -- CLK が有効
      Clk     => Clk,                                    -- 49.1520MHz
      Reset   => Reset,
      Stop    => Stop,
      Halt    => Halt,
      Li      => Li,
      Flags   => Flags,
      -- CPU と Memory 内容の表示・書換え用配線
      Aout    => DbgAddr,
      Dout    => DbgDataCns,
      DinCpu  => DbgDataCpu,
      DinMem  => DbgDataMem,
      WeCpu   => DbgWeCpu,
      WeMem   => DbgWeMem,
      Ain     => Addr,
      -- Console(INPUT)
      DSw     => DATA_SW,
      RstSw   => RESET_SW,
      SetSw   => SETA_SW,
      IncSw   => INCA_SW,
      DecSw   => DECA_SW,
      WrtSw   => WRITE_SW,
      StepSw  => STEP_SW,
      BrkSw   => BREAK_SW,
      StopSw  => STOP_SW,
      RunSw   => RUN_SW,
      CwSw    => RIGHT_SW,
      CcwSw   => LEFT_SW,
      -- Console(OUTPUT)
      AddrLed => ADDR_LED,
      DataLed => DATA_LED,
      RunLed  => RUN_LED, 
      CrryLed => C_LED,
      SignLed => S_LED,
      ZeroLed => Z_LED,
      G0Led   => G0_LED,
      G1Led   => G1_LED,
      G2Led   => G2_LED,
      SpLed   => SP_LED,
      PcLed   => PC_LED,
      MmLed   => MM_LED,
      Spk     => SPK_OUT
      );
  
  Cpu1 : Cpu
    port map (
      Clk     => Clk,
      -- 制御
      Reset   => Reset,
      Stop    => Stop,
      Halt    => Halt,
      Li      => Li,
      Flags   => Flags,
      -- RAM
      Addr    => Addr,
      Din     => DataIn,
      Dout    => DataOut,
      We      => WeMem,
      -- Console
      DbgAin  => DbgAddr(2 downto 0),
      DbgDin  => DbgDataCns,
      DbgDout => DbgDataCpu,
      DbgWe   => DbgWeCpu
      );

  Ram1 : Ram
    port map (
      Clk     => Clk,
      -- CPU 用のポート
      Addr    => Addr,
      Din     => DataOut,                                -- CPU の出力を入力
      Dout    => DataIn,                                 -- CPU の入力へ出力
      We      => WeMem,
      -- Console 用のポート
      DbgAddr => DbgAddr,
      DbgDin  => DbgDataCns,
      DbgDout => DbgDataMem,
      DbgWe   => DbgWeMem
      );
end Behavioral;
