-- Sequencer.vhd
-- 情報電子工学総合実験(CE1)用 TeC CPU の制御部
--
-- (c)2014 - 2019 by Dept. of Computer Science and Electronic Engineering,
--            Tokuyama College of Technology, JAPAN

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity Sequencer is
  Port ( Clk   : in  STD_LOGIC;
         -- 入力
         Reset : in  STD_LOGIC;
         OP    : in  STD_LOGIC_VECTOR (3 downto 0);
         Rd    : in  STD_LOGIC_VECTOR (1 downto 0);
         Rx    : in  STD_LOGIC_VECTOR (1 downto 0);
         Flag  : in  STD_LOGIC_VECTOR (2 downto 0); -- CSZ
         Stop  : in  STD_LOGIC;
         -- CPU内部の制御用に出力
         IRLd  : out  STD_LOGIC;
         DRLd  : out  STD_LOGIC;
         FLLd : out  STD_LOGIC;
         GRLd  : out  STD_LOGIC;
         AddrADD    : out  STD_LOGIC;
         DataALU    : out  STD_LOGIC_VECTOR (2 downto 0);
         SPop  : out  STD_LOGIC;
         MUX0Ld: out  STD_LOGIC_VECTOR (1 downto 0);
         MUX1Ld: out  STD_LOGIC;
         MUX2Ld: out  STD_LOGIC_VECTOR (2 downto 0);
         MUX5Ld: out  STD_LOGIC_VECTOR (1 downto 0);

         -- CPU外部へ出力
         We    : out  STD_LOGIC;
         Halt  : out  STD_LOGIC
         );
end Sequencer;

architecture Behavioral of Sequencer is
  signal Stat  : STD_LOGIC_VECTOR(3  downto 0); -- State
  signal NxtSt : STD_LOGIC_VECTOR(3  downto 0); -- Next State
  signal DecSt : STD_LOGIC_VECTOR(13 downto 0); -- Decoded State
  signal Type1 : STD_LOGIC;                     -- LD/ADD/SUB/CMP/AND/OR/XOR
  signal Jmp   : STD_LOGIC;                     -- JMP
  signal Jz    : STD_LOGIC;                     -- JZ
  signal Jc    : STD_LOGIC;                     -- JC
  signal Jm    : STD_LOGIC;                     -- JM
  signal JmpCnd: STD_LOGIC;                     -- Jmp Condition
  signal Immd  : STD_LOGIC;                     -- Immediate mode

begin
-- State machine
  with Stat select
    DecSt <= "00000000000001" when "0000",
             "00000000000010" when "0001",
             "00000000000100" when "0010",
             "00000000001000" when "0011",
             "00000000010000" when "0100",
             "00000000100000" when "0101",
             "00000001000000" when "0110",
             "00000010000000" when "0111",
             "00000100000000" when "1000",
             "00001000000000" when "1001",
             "00010000000000" when "1010",
             "00100000000000" when "1011",
             "01000000000000" when "1100",
             "10000000000000" when others;

  Type1 <= '1' when OP="0001" or OP="0011" or OP="0100" or -- LD/ADD/SUB
           OP="0101" or OP="0110" or OP="0111" or -- CMP/AND/OR
           OP="1000" else '0';                    -- XOR
  
  NxtSt <= "0000" when (DecSt(0)='1' and Stop='1') or    -- Stop
                       (DecSt(1)='1' and OP="0000") or   -- No
                       DecSt(3)='1' or DecSt(4)='1' or   -- LD/.../XOR/SHxx
                       DecSt(5)='1' or DecSt(6)='1' or   -- ST,JMP,JZ,JC
                       DecSt(8)='1' or DecSt(9)='1' or  -- CALL,PUSH
                       DecSt(11)='1' or DecSt(12)='1' or -- POP,RET
                       DecSt(13)='1' else -- HALT
           "0001" when DecSt(0)='1' and Stop='0'  else   -- Fetch
           "0010" when DecSt(1)='1' and Type1='1' else   -- LD/ADD/.../XOR
           "0011" when DecSt(2)='1' or Rx="01" or Rx="10" else                 -- LD/ADD/.../XOR
           "0100" when DecSt(1)='1' and OP="0010" else   -- ST
           "0101" when DecSt(1)='1' and OP="1010" else   -- JMP/JZ/JC/JM
           "0110" when DecSt(1)='1' and OP="1011" else   -- CALL
           "0111" when DecSt(6)='1'               else
           "1000" when DecSt(1)='1' and OP="1101" and Rx="00" else -- PUSH
           "1001" when DecSt(8)='1'               else
           "1010" when DecSt(1)='1' and OP="1101" and Rx="10" else -- POP
           "1011" when DecSt(10)='1'                          else
           "1100" when DecSt(1)='1' and OP="1110" else   -- RET
           "1101";                                       -- HALT/ERROR

  process(Clk, Reset)
  begin
    if (Reset='1') then
      Stat <= "0000";
    elsif (Clk'event and Clk='1') then
      Stat <= NxtSt;
    end if;
  end process;
  
  -- Control Signals
  Jmp  <= '1' when Rd="00" else '0';  -- JMP
  Jz   <= '1' when Rd="01" else '0';  -- JZ
  Jc   <= '1' when Rd="10" else '0';  -- JC
  Jm   <= '1' when Rd="11" else '0';  -- JM
  Immd <= '1' when Rx="11" else '0';  -- Immediate mode
  
  --        JMP     JZ and Z Flag       JC and C Flag       JM and S Flag
  JmpCnd <= Jmp or (Jz and Flag(0)) or (Jc and Flag(2)) or (Jm and Flag(1));
  
  IrLd  <= DecSt(0);
  DrLd  <= DecSt(1) or (DecSt(2) and not Immd) or DecSt(10);
  FlgLd <= '1' when DecSt(3)='1' and OP/="0001" else '0';    -- OP /=LD
  GrLd  <= '1' when (DecSt(3)='1' and OP/="0101") or         -- OP /=CMP
           DecSt(11)='1' else '0';
  SpP1  <= DecSt(10) or DecSt(12);
  SpM1  <= DecSt(6)  or DecSt(8);
  PcP1  <= (DecSt(0) and not Stop) or
           DecSt(2) or DecSt(4) or DecSt(5) or DecSt(6);
  PcJmp <= (DecSt(5) and JmpCnd) or DecSt(7);
  PcRet <= DecSt(12);
  Ma    <= "00" when DecSt(0)='1' or DecSt(1)='1' else       -- "00"=PC
           "01" when DecSt(2)='1' or DecSt(4)='1' else       -- "01"=EA
           "10";                                             -- "10"=SP
  Md    <= not DecSt(7);
  We    <= DecSt(4)  or DecSt(7) or DecSt(9);
  Halt  <= DecSt(13);

end Behavioral;