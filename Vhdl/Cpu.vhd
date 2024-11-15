-- Cpu.vhd
-- 情報電子工学総合実験(CE1)用 TeC の CPU 部分 !!! ひな形 !!!
--
-- (c)2014 - 2024 by Dept. of Computer Science and Electronic Engineering,
--            Tokuyama College of Technology, JAPAN

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity Cpu is
  Port ( Clk     : in  std_logic;
         -- 制御
         Reset   : in  std_logic;
         Stop    : in  std_logic;
         Halt    : out std_logic;
         Li      : out std_logic;                       -- 命令フェッチ
         Flags   : out std_logic_vector (2 downto 0);   -- CSZ
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
end Cpu;

architecture Behavioral of Cpu is

-- CPU Register
  signal G0  : std_logic_vector(7 downto 0);
  signal G1  : std_logic_vector(7 downto 0);
  signal G2  : std_logic_vector(7 downto 0);
  signal SP  : std_logic_vector(7 downto 0);

-- PSW
  signal PC  : std_logic_vector(7 downto 0);
  signal FLG : std_logic_vector(2 downto 0);            -- CSZ

-- IR
  signal OP  : std_logic_vector(3 downto 0);
  signal Rd  : std_logic_vector(1 downto 0);
  signal Rx  : std_logic_vector(1 downto 0);

begin
-- 仮の信号を出力しておく
  Halt <= '0';
  Addr <= "00000000";
  Dout <= "00000000";
  We   <= '0';
  Li   <= '0';

-- フラグ
  Flags <= FLG;
  
-- PC の制御
  process(Clk, Reset)
  begin
    if (Reset='1') then
      PC <= "00000000";
    elsif (Clk'event and Clk='1') then
      if (DbgWe='1' and DbgAin="100") then
        PC <= DbgDin;
      end if;
    end if;
  end process;
  
-- CPU レジスタの制御
  process(Clk, Reset)
  begin
    if (Reset='1') then
      G0  <= "00000000";
      G1  <= "00000000";
      G2  <= "00000000";
      SP  <= "00000000";
    elsif (Clk'event and Clk='1') then
      if (DbgWe='1') then
        case DbgAin is
          when "000" => G0 <= DbgDin;
          when "001" => G1 <= DbgDin;
          when "010" => G2 <= DbgDin;
          when "011" => SP <= DbgDin;
          when others => null;
        end case;
      end if;
    end if;
  end process;

-- フラグの制御
  process(Clk, Reset)
  begin
    if (Reset='1') then
      FLG <= "000";
    elsif (Clk'event and Clk='1') then
      if (DbgWe='1' and DbgAin="110") then
        FLG <= DbgDin(2 downto 0);
      end if;
    end if;
  end process;
  
-- デバッグ用のコンソール接続
  DbgDout <= G0 when DbgAin="000" else
             G1 when DbgAin="001" else
             G2 when DbgAin="010" else
             SP when DbgAin="011" else
             PC when DbgAin="100" else
             "00000" & FLG;

end Behavioral;
