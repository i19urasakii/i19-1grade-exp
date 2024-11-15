-- Cpu.vhd
-- 情報電子工学総合実験(CE1)用 TeC の CPU 部分 !!! 模範解答 !!!
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
           IrLd  : out  STD_LOGIC;
           DrLd  : out  STD_LOGIC;
           FlgLd : out  STD_LOGIC;
           GrLd  : out  STD_LOGIC;
           SpM1  : out  STD_LOGIC;
           SpP1  : out  STD_LOGIC;
           PcP1  : out  STD_LOGIC;
           PcJmp : out  STD_LOGIC;
           PcRet : out  STD_LOGIC;
           Ma    : out  STD_LOGIC_vector (1 downto 0);
           Md    : out  STD_LOGIC;
           -- CPU外部へ出力
           We    : out  STD_LOGIC;
           Halt  : out  STD_LOGIC
           );
  end component;

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

-- オペコード
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

-- DR
  signal DR  : std_logic_vector(7 downto 0);

-- 内部バス
  signal EA    : std_logic_vector(7 downto 0); -- Effective Address
  signal RegRd : std_logic_vector(7 downto 0); -- Reg[Rd]
  signal RegRx : std_logic_vector(7 downto 0); -- Reg[Rx]
  signal Alu   : std_logic_vector(8 downto 0); -- ALU出力（キャリー付)
  signal Zero  : std_logic;                    -- ALUが0か？
  signal SftRd : std_logic_vector(8 downto 0); -- RegRdをシフトしたもの

-- 内部制御線（ステートマシンの出力)
  signal IrLd  : std_logic;                    -- IR:Ld
  signal DrLd  : std_logic;                    -- DR:Ld
  signal FlgLd : std_logic;                    -- Flag:Ld
  signal GrLd  : std_logic;                    -- GR:Ld
  signal SpM1  : std_logic;                    -- SP:M1
  signal SpP1  : std_logic;                    -- SP:P1
  signal PcP1  : std_logic;                    -- PC:P1
  signal PcJmp : std_logic;                    -- PC:JMP
  signal PcRet : std_logic;                    -- PC:RET

  signal Ma    : std_logic_vector(1 downto 0); -- MA(PC=00,EA=01,SP=10)
  signal Md    : std_logic;                    -- MD(PC=0,GR=1)

begin
-- コンソールへの接続
  Flags <= FLG;
  Li    <= IrLd;

-- 制御部
  seq1: Sequencer Port map (Clk, Reset, OP, Rd, Rx, FLG, Stop,
                            IrLd, DrLd, FlgLd, GrLd, SpM1, SpP1, PcP1,
                            PcJmp, PcRet, Ma, Md, We, Halt);

-- BUS
  Addr <= PC when Ma="00" else
          EA when Ma="01" else SP;
  
  EA <= DR + RegRx;

  Dout <= PC when Md='0' else RegRd;
  
-- ALU
  SftRd <= (RegRd & '0') when Rx(1)='0' else                      -- SHLA/SHLL
    (RegRd(0) & RegRd(7) & RegRd(7 downto 1)) when Rx(0)='0' else -- SHRA
    (RegRd(0) & '0' & RegRd(7 downto 1));                         -- SHRL
  
  Alu <= ('0' & RegRd) + ('0' & DR) when OP=OP_ADD else
         ('0' & RegRd) - ('0' & DR) when OP=OP_SUB or OP=OP_CMP else
         ('0' & RegRd)and('0' & DR) when OP=OP_AND else
         ('0' & RegRd)or ('0' & DR) when OP=OP_OR  else
         ('0' & RegRd)xor('0' & DR) when OP=OP_XOR else
         SftRd when OP=OP_SFT else ('0' & DR);

  Zero <= '1' when ALU(7 downto 0)="00000000" else '0';

-- IR,DR の制御
  process(Clk)
  begin
    if (Clk'event and Clk='1') then
      if (IrLd='1') then
        OP <= Din(7 downto 4);
        Rd <= Din(3 downto 2);
        Rx <= Din(1 downto 0);
      end if;
      if (DrLd='1') then
        DR <= Din;
      end if;
    end if;
  end process;
  
-- PC の制御
  process(Clk, Reset)
  begin
    if (Reset='1') then
      PC <= "00000000";
    elsif (Clk'event and Clk='1') then
      if (PcJmp='1') then
        PC <= Ea;
      elsif (PcRet='1') then
        PC <= Din;
      elsif (PcP1='1') then
        PC <= PC + 1;
      elsif (DbgWe='1' and DbgAin="100") then
        PC <= DbgDin;
      end if;
    end if;
  end process;
  
-- CPU レジスタの制御
  RegRd <= G0 when Rd="00" else G1 when Rd="01" else
           G2 when Rd="10" else SP;

  RegRx <= G1 when Rx="01" else G2 when Rx="10" else "00000000";
  
  process(Clk, Reset)
  begin
    if (Reset='1') then
      G0  <= "00000000";
      G1  <= "00000000";
      G2  <= "00000000";
      SP  <= "00000000";
    elsif (Clk'event and Clk='1') then
      if (GrLd='1') then
        case Rd is
          when "00" => G0 <= Alu(7 downto 0);
          when "01" => G1 <= Alu(7 downto 0);
          when "10" => G2 <= Alu(7 downto 0);
          when others => SP <= Alu(7 downto 0);
        end case;
      elsif (SpP1='1') then
        SP <= SP + 1;
      elsif (SpM1='1') then
        SP <= Sp - 1;
      elsif (DbgWe='1') then
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
      if (FlgLd='1') then
        FLG(2) <= Alu(8);                -- Carry
        FLG(1) <= Alu(7);                -- Sign
        FLG(0) <= Zero;                  -- Zero
      elsif (DbgWe='1' and DbgAin="110") then
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
