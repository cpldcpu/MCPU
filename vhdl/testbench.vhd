library ieee;
use ieee.std_logic_1164.all;


entity cpu2system is
	port (clk:	in	std_logic;
	      reset:	in	std_logic
	);
end;

architecture main of cpu2system is

component CPU8BIT2
	port (	--dout:	out	std_logic_vector(7 downto 0);
		--data:	in	std_logic_vector(7 downto 0);
		data:	inout	std_logic_vector(7 downto 0);
		adress:	out	std_logic_vector(5 downto 0);
		oe:	out	std_logic;
		we:	out	std_logic;
		rst:	in	std_logic;
		clk:	in	std_logic
	);
end component;

component sram64kx8 
  port (ncs1, cs2: in std_logic;        -- not chip select 1, cs2
        addr     : in std_logic_vector( 15 downto 0 );
        data     : inout std_logic_vector( 7 downto 0 );
        nwe      : in std_logic;        -- not write enable
        noe      : in std_logic        -- not output enable 
       );
end component;

	signal	ncs,cs:	std_logic;
	signal	oe,we:	std_logic;
	signal	data:	std_logic_vector(7 downto 0);
	signal  adrram: std_logic_vector(15 downto 0);
	signal  adrcpu:	std_logic_vector(5 downto 0);
begin

 CPU:	CPU8BIT2	port map(rst => reset, clk => clk, oe => oe, we => we, data => data, adress => adrcpu); 
 RAM:	sram64kx8	port map(ncs1 => ncs, cs2 => cs, data => data, addr => adrram, nwe => we, noe => oe);	

	ncs <= '0';
	cs  <= '1';
	adrram <= "0000000000" & adrcpu;
end;

library ieee;
use ieee.std_logic_1164.all;

entity testbench is
end;

architecture testmain of testbench is

component cpu2system
	port (clk:	in	std_logic;
	      reset:	in	std_logic
	);
end component;

 signal clk,reset:	std_logic;
begin

 SYS: cpu2system	port map(clk => clk, reset => reset);

  process
  begin
	clk <= '0';
	reset <= '1';
	WAIT FOR 50 ns;
	clk <= '0';
	reset <= '0';
	WAIT FOR 50 ns;
	clk <= '1';
	WAIT FOR 25 ns;
	reset <= '1';
	WAIT FOR 25 ns;

	loop
	  clk <= '0';
  	  WAIT FOR 50 ns;
	  clk <= '1';
	  WAIT FOR 50 ns;		-- clock.
	end loop;

	
  end process;
end;

configuration tb_cfg of testbench is
	for testmain
	end for;
end tb_cfg;






