
--Signal types are listed in parenthesis:
--
--(r) this line goes to the ROM only.
--(s) this line is Shared between the ROM, MMC/chip, and Nintendo
--(n) this line connects to the NES cart edge only, and not the ROM
--(w) this line connects to the WRAM only and nowhere else
--
--
--MMC1 Chip:    (24 pin shrink-DIP)
------------
--Comes in several varieties: 'MMC1', 'MMC1A', and 'MMC1B2'                                  
--
--                                  .---\/---.
--                    PRG A14 (r) - |01    24| - +5V
--                    PRG A15 (r) - |02    23| - M2        
--                    PRG A16 (r) - |03    22| - PRG A13 (s)
--                    PRG A17 (r) - |04    21| - PRG A14 (n)
--                    PRG /CE (r) - |05    20| - PRG /CE (n)
--                    WRAM CE (w) - |06    19| - PRG D7 (s)
--                    CHR A12 (r) - |07    18| - PRG D0 (s)
--                    CHR A13 (r) - |08    17| - PRG R/W 
--                    CHR A14 (r) - |09    16| - CIRAM A10 (n)
--                    CHR A15 (r) - |10    15| - CHR A12 (n)
--    CHR A16 (r) or WRAM /CE (w) - |11    14| - CHR A11 (s)
--                            GND - |12    13| - CHR A10 (s)
--                                  `--------'
--
--                                     MMC1											 
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

library altera; 
use altera.altera_primitives_components.all;

entity MMC1 is 

	port (
			--reset generator (if required)
			nRST_p			:	in 	std_logic;
	
			--input from NES
			CPUDATA_p		:	in 	std_logic_vector(7 downto 0);
			CPURnW_p			:  in    std_logic;
			nROMSEL_p		:  in		std_logic;
			CPUA14_p			:	in 	std_logic;
			CPUA13_p			:	in 	std_logic;
			nPPUA13_p		:	in		std_logic;
			PPUA13_p			:  in    std_logic;
			PPUA12_p			:  in    std_logic;
			PPUA11_p			:  in    std_logic;
			PPUA10_p			:  in    std_logic;
			nPPURD_p			:  in		std_logic;
			nPPUWR_p			:  in		std_logic;
			M2_p				:  in		std_logic;
			CLK_p				:  in		std_logic;

			--output to Program ROM / WRAM
			PRGA17_p			:  out	std_logic;
			PRGA16_p			:  out	std_logic;
			PRGA15_p			:  out	std_logic;
			PRGA14_p			:  out	std_logic;
			PRGA13_p			:  out	std_logic;
			nPRGCE_p			:  out   std_logic;
			nWRAMCE_p		:  out   std_logic;
			
			--output to Character ROM
			CHRA16_p			:  out	std_logic;
			CHRA15_p			:  out	std_logic;
			CHRA14_p			:  out	std_logic;
			CHRA13_p			:  out	std_logic;
			CHRA12_p			:  out	std_logic;
			CHRA11_p			:  out	std_logic;
			CHRA10_p			:  out	std_logic;
			
			--output to NES
			nIRQ_p			:	out	std_logic;
			nCIRAMCE_p		:  out   std_logic;
			CIRAMA10_p		:  out	std_logic
	);
end entity; 

architecture MMC1_a of MMC1 is

	signal RomAddr17to14_s		:	std_logic_vector(3 downto 0);
	signal ChrAddr16to12_s		:	std_logic_vector(4 downto 0);
	signal cpuA15_s				:  std_logic;
	
	signal MMCReg0_s				:  std_logic_vector(4 downto 0);
	signal MMCReg1_s				:  std_logic_vector(4 downto 0);
	signal MMCReg2_s				:  std_logic_vector(4 downto 0);
	signal MMCReg3_s				:  std_logic_vector(4 downto 0);
	signal TempReg_s				:	std_logic_vector(4 downto 0);
	signal CHRMirr_s				:  std_logic_vector(1 downto 0);
	
	--state machine for serial writes to registers
	signal resetState				:  std_logic;
	type 	 state_type 			is (s0,s1,s2,s3,s4);
	signal current_s,next_s		: 	state_type;
	signal cpuAddr15to13_s		:  std_logic_vector(2 downto 0);
	
begin

	--no IRQ in MMC1
	nIRQ_p <= 'Z';
	
	--CIRAM always enabled
	nCIRAMCE_p <= nPPUA13_p;
	
	--determine A15
	cpuA15_s <= '0' when (M2_p = '1' and nROMSEL_p = '0') else '1';
	
	--group higher addresses for easier reading
	cpuAddr15to13_s <= cpuA15_s & CPUA14_p & CPUA13_p;
	
	--**************************************************************
	--WRAM
	--CPU $6000-$7FFF: 8 KB PRG RAM bank, fixed on all boards but SOROM and SXROM
	--M2 is high when address is valid
	--0b0110 -> 0b0111
	nWRAMCE_p <= '0' when (M2_p = '1' and cpuAddr15to13_s = "011" and MMCReg3_s(4) = '0') else '1';
	
	--**************************************************************	
	--Mirroring
	--To configure a cartridge board for horizontal mirroring, connect PPU A11 to CIRAM A10
	--To configure a cartridge board for vertical mirroring, connect PPU A10 to CIRAM A10
		--00b - 1-screen mirroring (nametable 0)
		--01b - 1-screen mirroring (nametable 1)
		--10b - Vert. mirroring
		--11b - Horiz. mirroring
	CHRMirr_s <= MMCReg0_s(1 downto 0);
	CIRAMA10_p <= 	'0' when CHRMirr_s = "00" else 
						'1' when CHRMirr_s = "01" else
						PPUA10_p when CHRMirr_s = "10" else
						PPUA11_p when CHRMirr_s = "11" else
						'0';
	
	--**************************************************************
	--CHR ROM banking
	CHRA10_p <= PPUA10_p;
	CHRA11_p <= PPUA11_p;
	CHRA12_p <= ChrAddr16to12_s(0);
	CHRA13_p <= ChrAddr16to12_s(1);
	CHRA14_p <= ChrAddr16to12_s(2);
	CHRA15_p <= ChrAddr16to12_s(3);
	CHRA16_p <= ChrAddr16to12_s(4);
	CHRBanking : process (PPUA13_p, PPUA12_p)
	begin
		--check bank size
		if (MMCReg0_s(4) = '0') then
			--0 - Single 8K bank in CHR space.
			--8K bank mode, this selects a full 8K bank at 0000h on the PPU space.
			ChrAddr16to12_s <= MMCReg1_s(4 downto 1) & PPUA12_p;
		else
			--1 - Two 4K banks in CHR space.
			--4K bank mode, this selects a 4K bank at 0000h on the PPU space.
			if (PPUA12_p = '0') then
				ChrAddr16to12_s <= MMCReg1_s(4 downto 0);
			else
				ChrAddr16to12_s <= MMCReg2_s(4 downto 0);
			end if;
		end if;
	end process;
	
	--**************************************************************
	--PRG ROM banking
	nPRGCE_p <= nROMSEL_p;
	PRGA13_p <= CPUA13_p;
	PRGA14_p <= RomAddr17to14_s(0);
	PRGA15_p <= RomAddr17to14_s(1);
	PRGA16_p <= RomAddr17to14_s(2);
	PRGA17_p <= RomAddr17to14_s(3);
	PRGBanking : process (nROMSEL_p, CPUA14_p)
	begin
		--check bank size
		if (MMCReg0_s(3) = '1') then
			--16K mode, this selects a 16K bank in either 8000-BFFFh
			--or C000-FFFFh depending on the state of the "H" bit in register 0.
			--check which bank is swappable
			if (MMCReg0_s(2) = '1') then
				--1 - Bank C000-FFFFh is fixed, while 8000-FFFFh is swappable. (power-on default)
				--fix last bank at $C000 and switch 16 KB bank at $8000
				if (CPUA14_p = '0') then --first bank
					RomAddr17to14_s <= MMCReg3_s(3 downto 0);
				else							 --last bank
					RomAddr17to14_s <= "1111";
				end if;
			else
				--0 - Bank 8000-BFFFh is fixed, while C000-FFFFh is swappable
				--fix first bank at $8000 and switch 16 KB bank at $C000;
				if (CPUA14_p = '1') then --last bank
					RomAddr17to14_s <= MMCReg3_s(3 downto 0);
				else							 --first bank
					RomAddr17to14_s <= "0000";
				end if;
			end if;
		else
			--32K mode, this selects a full 32K bank in the PRG space.
			--Only the upper 3 bits are used then.
			RomAddr17to14_s(3 downto 0) <= MMCReg3_s(3 downto 1) & CPUA14_p;
		end if;
	end process;
	
	--write to mapper registers state machine
	--use A14 and A13 to determine the register being written to
	--The first bit in is the LSB, while the last bit in is the MSB.
	process (nROMSEL_p, CPURnW_p)
	begin
		if (falling_edge(CPURnW_p)) then
			if (nROMSEL_p = '0') then
				current_s <= next_s;   --state change.
			end if;
		end if;
	end process;
	
	process (current_s, CPUDATA_p, nROMSEL_p)
	begin
		if (rising_edge(nROMSEL_p)) then
			if (CPURnW_p = '0') then
				case current_s is
					when s0 =>
						if (CPUDATA_p(7) = '1') then
							next_s <= s0;
							case cpuAddr15to13_s(1 downto 0) is
								when "00" =>
									MMCReg0_s <= "01100";
								when "01" =>
									MMCReg1_s <= "00000";
								when "10" =>
									MMCReg2_s <= "00000";
								when "11" =>
									MMCReg3_s <= "00000";
							end case;
						else
							tempReg_s(3 downto 0) <= tempReg_s(4 downto 1);
							tempReg_s(4) <= CPUDATA_p(0);
							next_s <= s1;
						end if;
					when s1 =>
						if (CPUDATA_p(7) = '1') then
							next_s <= s0;
							case cpuAddr15to13_s(1 downto 0) is
								when "00" =>
									MMCReg0_s <= "01100";
								when "01" =>
									MMCReg1_s <= "00000";
								when "10" =>
									MMCReg2_s <= "00000";
								when "11" =>
									MMCReg3_s <= "00000";
							end case;
						else
							tempReg_s(3 downto 0) <= tempReg_s(4 downto 1);
							tempReg_s(4) <= CPUDATA_p(0);
							next_s <= s2;
						end if;
					when s2 =>
						if (CPUDATA_p(7) = '1') then
							next_s <= s0;
							case cpuAddr15to13_s(1 downto 0) is
								when "00" =>
									MMCReg0_s <= "01100";
								when "01" =>
									MMCReg1_s <= "00000";
								when "10" =>
									MMCReg2_s <= "00000";
								when "11" =>
									MMCReg3_s <= "00000";
							end case;
						else
							tempReg_s(3 downto 0) <= tempReg_s(4 downto 1);
							tempReg_s(4) <= CPUDATA_p(0);
							next_s <= s3;
						end if;
					when s3 =>
						if (CPUDATA_p(7) = '1') then
							next_s <= s0;
							case cpuAddr15to13_s(1 downto 0) is
								when "00" =>
									MMCReg0_s <= "01100";
								when "01" =>
									MMCReg1_s <= "00000";
								when "10" =>
									MMCReg2_s <= "00000";
								when "11" =>
									MMCReg3_s <= "00000";
							end case;
						else
							tempReg_s(3 downto 0) <= tempReg_s(4 downto 1);
							tempReg_s(4) <= CPUDATA_p(0);
							next_s <= s4;
						end if;
					when s4 =>
						if (CPUDATA_p(7) = '1') then
							next_s <= s0;
							case cpuAddr15to13_s(1 downto 0) is
								when "00" =>
									MMCReg0_s <= "01100";
								when "01" =>
									MMCReg1_s <= "00000";
								when "10" =>
									MMCReg2_s <= "00000";
								when "11" =>
									MMCReg3_s <= "00000";
							end case;
						else
							case cpuAddr15to13_s(1 downto 0) is
								when "00" =>
									MMCReg0_s(3 downto 0) <= tempReg_s(4 downto 1);
									MMCReg0_s(4) <= CPUDATA_p(0);
								when "01" =>
									MMCReg1_s(3 downto 0) <= tempReg_s(4 downto 1);
									MMCReg1_s(4) <= CPUDATA_p(0);
								when "10" =>
									MMCReg2_s(3 downto 0) <= tempReg_s(4 downto 1);
									MMCReg2_s(4) <= CPUDATA_p(0);
								when "11" =>
									MMCReg3_s(3 downto 0) <= tempReg_s(4 downto 1);
									MMCReg3_s(4) <= CPUDATA_p(0);
							end case;
							next_s <= s0;
						end if;
					when others =>
						next_s <= s0;
				end case;
			end if;
		end if;
	end process;
	
	
end MMC1_a;