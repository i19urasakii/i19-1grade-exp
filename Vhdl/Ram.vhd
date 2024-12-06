-- Ram.VHD
-- 情報電子工学総合実験(CE1)用 TeC の主記憶部分
-- (ブロック RAM を用いた、デュアルポート RAM)
--
-- (c)2014 - 2019 by Dept. of Computer Science and Electronic Engineering,
--            Tokuyama College of Technology, JAPAN

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity RAM is
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
end RAM;

architecture Behavioral of RAM is
  subtype word is std_logic_vector(7 downto 0);
  type memory is array(255 downto 0) of word;
  shared variable mem : memory;
  
begin
  process(Clk)
  begin
    if (Clk'event and Clk='0') then -- 逆相で動作させる
      if (We='1') then
        mem( conv_integer(Addr) ) := Din;
      end if;
      Dout <= mem( conv_integer(Addr) );
    end if;
  end process;

  process(Clk)
  begin
    if (Clk'event and Clk='0') then -- 逆相で動作させる
      if (DbgWe='1') then
        mem( conv_integer(DbgAddr) ) := DbgDin;
      end if;
      DbgDout <= mem( conv_integer(DbgAddr) );
    end if;
  end process;
  
end Behavioral;