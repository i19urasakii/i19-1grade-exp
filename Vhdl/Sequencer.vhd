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
         PCLD  : out  STD_LOGIC;

         SPop  : out  STD_LOGIC_vector (1 downto 0);

         GRsel : out  STD_LOGIC_vector (2 downto 0);  -- レジスタの選択信号
         PCSel : out  STD_LOGIC_vector (1 downto 0);  -- MUX0 : プログラムカウンタの選択信号
         DoutSel: out  STD_LOGIC_vector (1 downto 0); -- MUX1 : データバス入力の選択信号
         AddrSel: out  STD_LOGIC_vector (2 downto 0); -- MUX2 : アドレスバスの選択信号
         SPsel  : out  STD_LOGIC_vector (1 downto 0);                     -- MUX5 : スタックポインタの選択信号


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
           "0010" when DecSt(1)='1' and Type1='1' and Immd='0' else   -- LD/ADD/.../XOR (mode:ダイレクト,インデクスド)
           "0011" when (DecSt(1)='1' and Type1='1' and Immd='1') or DecSt(2)='1' else  -- LD/ADD/.../XOR (mode:全部)  判断式Rx="01" or Rx="10"でも？
           "0100" when DecSt(1)='1' and OP="1001" else   -- SHIFT
           "0101" when DecSt(1)='1' and OP="0010" else   -- ST
           "0110" when DecSt(1)='1' and OP="1010" else   -- JMP,JZ,JC
           "0111" when DecSt(1)='1' and OP="1011" else   -- CALL
           "1000" when DecSt(7)='1' else -- CALL
           "1001" when DecSt(1)='1' and OP="1101" and Rx="00" else -- PUSH
           "1010" when DecSt(1)='1' and OP="1101" and Rx="10" else -- POP
           "1011" when DecSt(10)='1' else
           "1100" when DecSt(1)='1' and OP="1110" else   -- RET
           "1101";                                       -- HALT/ERROR

  process(Clk, Reset)
  begin
    if (Reset='1') then
      Stat <= "0000";
    elsif (Clk'event and Clk='1') then --　クロックが立ち上がりエッジに変化した時
      Stat <= NxtSt;
    end if;
  end process;
  
  -- Control Signals
  Jmp  <= '1' when Rd="00" else '0';  -- JMP
  Jz   <= '1' when Rd="01" else '0';  -- JZ
  Jc   <= '1' when Rd="10" else '0';  -- JC
  Jm   <= '1' when Rd="11" else '0';  -- JM
  Immd <= '1' when Rx="11" else '0';  -- Immediate mode
  
  -- jmpしてたら1
  --        JMP     JZ and Z Flag       JC and C Flag       JM and S Flag
  JmpCnd <= Jmp or (Jz and Flag(0)) or (Jc and Flag(2)) or (Jm and Flag(1));
   
  IRLd  <= '1' when DecSt(0)='1' else '0';  --
  DRLd  <= '1' when DecSt(1)='1' or DecSt(2)='1' or DecSt(10)='1' else '0';  -- LD/ST/POP
  FLLd <= '1' when (DecSt(3)='1' and OP/="0001") or DecSt(4)='1' else '0';    -- OP LD以外
  PCLd <= '1' when (DecSt(0)='1' and Stop='0') or DecSt(3)='1' or DecSt(5)='1' or DecSt(6)='1'  
                or DecSt(8)='1' or DecSt(12)='1' else '0';  -- JMP/ST/CALL/PUSH/POP/RET
  
  GRsel <= "001" when ((DecSt(3)='1' and OP/="0101")or DecSt(4)='1' or DecSt(11)='1') and Rd="00" else      -- G0, cmp以外
           "010" when ((DecSt(3)='1' and OP/="0101") or DecSt(4)='1' or DecSt(11)='1') and Rd="01" else -- G1
           "011" when ((DecSt(3)='1' and OP/="0101") or DecSt(4)='1' or DecSt(11)='1') and Rd="10" else -- G2
           "100" when (((DecSt(3)='1' and OP/="0101") or DecSt(4)='1' or DecSt(11)='1') and Rd="11") or -- SP 汎用レジスタ
                     (DecSt(7)='1' or DecSt(9)='1' or DecSt(10)='1' or DecSt(12)='1') else
            "000";    -- SP スタック用
          --  "11" when (DecSt(7)='1' or DecSt(9)='1' or DecSt(10)='1' or DecSt(12)='1') and SPsel='1';    -- SP スタック用

  -- Mux0: プログラムカウンタの選択信号
           -- DecSt(7)='1' or DecSt(9)='1' or DecSt(10)='1' or DecSt(12)='1'
           -- (Jz='1' or Jc='1' or Jm='1')
  PCSel <= "01" when ((DecSt(6)='1' and JmpCnd='1' and  (Rx="01" or Rx="10") ) or (DecSt(8)='1' and (Rx="01" or Rx="10"))) else  -- AddrADDの出力.  :jmp(成立時)とCALL のインデクスドモード, 
           "10" when ((DecSt(6)='1' and JmpCnd='1' and  Rx="00" ) or (DecSt(8)='1' and Rx="00") or DecSt(12)='1') else   --Din            :jmp(成立時)とCALL のダイレクトモード, RET
           "11" when ((DecSt(0)='1' and Stop='0') or DecSt(3)='1' or DecSt(5)='1' or (DecSt(6)='1' and JmpCnd='0')) else -- PC+1　　　　　:jmp(不成立)
           "00";
  -- Mux1: データバス入力の選択信号
  DoutSel <= "01" when DecSt(5)='1' or DecSt(9)='1' else -- 1:Mux4out:GR 
             "10" when DecSt(7)='1' else -- 2:PC+1
             "00";
  -- Mux2: アドレスバスの選択信号
  AddrSel <= "001" when DecSt(0)='1' or DecSt(1)='1' else -- 1:PC
             "010" when (DecSt(6)='1' and JmpCnd='0')  else                           -- 2:PC+1, jmp(不成立)
             "011" when DecSt(2)='1' or DecSt(5)='1' or (DecSt(6)='1' and JmpCnd='1') else -- 3:AddrADD, jmp(成立時)
             "100" when DecSt(10)='1' or DecSt(12)='1' else -- 4:SP
             "101" when DecSt(7)='1' or DecSt(9)='1' else -- 5:SP +/-
             "000";       

  -- Mux5: スタックポインタの選択信号
  SPSel <= "01" when DecSt(10)='1' or DecSt(12)='1'else -- 
           "10" when DecSt(7)='1' or DecSt(9)='1'  else --
           "00";
  -- SPの演算
  SPop <= "01" when DecSt(10)='1' or DecSt(12)='1' else  -- SPをインクリメント
          "10" when DecSt(7)='1' or DecSt(9)='1' else  -- SPをデクリメント
          "00";

  We    <= DecSt(5) or DecSt(7) or DecSt(9); -- 書き込み  /ST/CALL/PUSHの時 4,6,8
  Halt  <= DecSt(13);

end Behavioral;