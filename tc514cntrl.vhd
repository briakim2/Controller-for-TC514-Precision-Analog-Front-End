-------------------------------------------------------------------------------
--
-- Title       : tc514cntrl
-- Design      : lab11_5
-- Author      : ESDL User
-- Company     : Stony Brook
--
-------------------------------------------------------------------------------
--
-- File        : f:\ESE382\lab11_4\lab11_5\src\tc514cntrl.vhd
-- Generated   : Tue May  6 12:34:32 2025
-- From        : interface description file
-- By          : Itf2Vhdl ver. 1.22
--
-------------------------------------------------------------------------------
--
-- Description : 
--
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
--
-- FREQ DIVIDER 
--
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity freq_div is
    port (
    	clk      : in std_logic;                      -- system clock
    	rst_bar  : in std_logic;                      -- synchronous reset
    	divisor  : in std_logic_vector(3 downto 0);   -- frequency divider value
    	clk_dvd  : out std_logic                      -- output pulse
    );
end freq_div;

architecture behavioral of freq_div is
    signal count : unsigned(3 downto 0) := (others => '0');
    signal div_val : unsigned(3 downto 0);
begin

    process(clk)
    begin
    	if rising_edge(clk) then
      		if rst_bar = '0' then
        		count <= (others => '0');
        		clk_dvd <= '0';
      		else
        		div_val <= unsigned(divisor);
        		if count = div_val - 1 then
          			count <= (others => '0');
          			clk_dvd <= '1';       -- generate 1-cycle pulse
        		else
          			count <= count + 1;
          			clk_dvd <= '0';
        		end if;
        	end if;
    	end if;
    end process;

end behavioral;

-------------------------------------------------------------------------------
--
-- BINARY COUNTER
--
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.all;

entity binary_cntr is
	generic (n : integer := 16); 	-- generic for counter width
	port (
		clk : in std_logic; 		-- system clock
		cnten1 : in std_logic; 		-- active high enable counter count	
		cnten2 : in std_logic; 		-- active high enable counter count
		up : in std_logic;			-- count direction
		clr_bar : in std_logic;		-- synchronous counter clear
		rst_bar : in std_logic; 	-- synchronous reset (active low)
		q : out std_logic_vector (n-1 downto 0);	-- output  
		max_cnt : out std_logic 	-- maximum count indication
	); 
end binary_cntr ;

architecture unsigned_variable_arch of binary_cntr is
begin
    process(clk)
    	variable count_uv : unsigned(n-1 downto 0) := (others => '0');	   -- unsigned variable; initialize to 0s
    begin
		
    	if rising_edge(clk) then
      		if (clr_bar = '0') or (rst_bar = '0') then					 -- reset enabled (active low)
        		count_uv := (others => '0');  		 -- reset count
				max_cnt <= '0';  		 				 -- reset count
      		elsif (cnten1 = '1') and (cnten2 = '1') then -- both counts enabled
				if up = '1' then
        			count_uv := count_uv + 1;		 -- increment count variable  
				else
					count_uv := count_uv - 1;
				end if;	
				
				if count_uv = to_unsigned(2**n - 1, n) then
					max_cnt <= '1';				 -- if count is all 1's, max_cnt = '1'
				else 
					max_cnt <= '0';				 -- otherwise, max_cnt = '0'
				end if;	 
				
      		end if;
			  
      		q <= std_logic_vector(count_uv);		 -- assign count variable to output		   
			  
    	end if;
		
  	end process;
end unsigned_variable_arch;


-------------------------------------------------------------------------------
--
-- OUT REG
--
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity out_reg is
	generic (n : integer := 16);
	port (
		clk : in std_logic;			--system clock
		enable : in std_logic;		-- parallel load enable
		rst_bar : in std_logic;		-- synchronous reset
		d : in std_logic_vector (n-1 downto 0);	-- data in
		q : out std_logic_vector (n-1 downto 0)	-- data out
	);
end out_reg;

--}} End of automatically maintained section

architecture out_reg of out_reg is
begin
	process (clk)
	begin
		if rising_edge(clk) then 
			if rst_bar = '0' then
				q <= (others => '0');
			elsif enable = '1' then
				q <= d;
			end if;
		end if;
	end process;
end out_reg;


-------------------------------------------------------------------------------
--
-- TC514 FSM
--
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity TC514fsm is
    port (
		-- INPUTS --
        soc           : in  std_logic;  -- start conversion control input
        cmptr         : in  std_logic;  -- TC 514 comparator status input
        max_cnt       : in  std_logic;  -- maximum count status input
        clk           : in  std_logic;  -- system clock
        clk_dvd       : in  std_logic;  -- clock divided down
        rst_bar       : in  std_logic;  -- synchronous reset
		-- OUTPUTS --
        a             : out std_logic;  -- conversion phase control
        b             : out std_logic;  -- conversion phase control
        busy_bar      : out std_logic;  -- active low busy status
        cnt_en        : out std_logic;  -- counter enable control to counter
        clr_cntr_bar  : out std_logic;  -- signal to clear counter
        load_result   : out std_logic   -- load enable
    );
end;

architecture behavioral of TC514fsm is

    -- State encoding: (load_result, clr_cntr_bar, cnt_en, busy_bar, a, b)
    type state_type is (  
		AUTO_ZERO,         -- 0 1 1 0 01
		IDLE,			   -- 0 1 1 1 10
        SIGNAL_INTEGRATE,  -- 1 1 1 0 10
        DEINTEGRATE,       -- 1 1 1 0 11
        ZERO_OUTPUT        -- 0 0 0 0 00
    );

    signal present_state, next_state : state_type;

begin

	 -- State Register
    state_register: process(clk, clk_dvd)
    begin
        if rising_edge(clk) and clk_dvd = '1' then
            if rst_bar = '0' then
                present_state <= AUTO_ZERO;
            else
                present_state <= next_state;
            end if;
        end if;
    end process;
	
    -- Next-State Logic
    next_state_logic: process(present_state, soc, cmptr, max_cnt)
    begin
        case present_state is
            when AUTO_ZERO =>
                if max_cnt = '1' then
                    next_state <= IDLE;
                else
                    next_state <= AUTO_ZERO;
                end if;
			
			when IDLE =>
				if soc = '1' then
					next_state <= SIGNAL_INTEGRATE;
				else 
					next_state <= IDLE;
				end if;

            when SIGNAL_INTEGRATE =>
                if max_cnt = '1' then
                    next_state <= DEINTEGRATE;
                else
                    next_state <= SIGNAL_INTEGRATE;
                end if;

            when DEINTEGRATE =>
                if falling_edge(cmptr) then
                    next_state <= ZERO_OUTPUT;
                else
                    next_state <= DEINTEGRATE;
                end if;

            when ZERO_OUTPUT =>
                if rising_edge(cmptr) then
                    next_state <= AUTO_ZERO;
                else
                    next_state <= ZERO_OUTPUT;
                end if;

            when others =>
                next_state <= AUTO_ZERO;
        end case;
    end process;
	
	-- Output Logic
	output_logic: process(present_state)
	begin
	    case present_state is
	        when AUTO_ZERO =>
	            load_result   <= '0';
	            clr_cntr_bar  <= '1';
	            cnt_en        <= '1';
	            busy_bar      <= '0';
	            a             <= '0';
	            b             <= '1';	 
				
			when IDLE =>
	            load_result   <= '0';
	            clr_cntr_bar  <= '1';
	            cnt_en        <= '1';
	            busy_bar      <= '1';
	            a             <= '1';
	            b             <= '0';
				
	        when SIGNAL_INTEGRATE =>
	            load_result   <= '1';
	            clr_cntr_bar  <= '1';
	            cnt_en        <= '1';
	            busy_bar      <= '0';
	            a             <= '1';
	            b             <= '0';
	
	        when DEINTEGRATE =>
	            load_result   <= '1';
	            clr_cntr_bar  <= '1';
	            cnt_en        <= '1';
	            busy_bar      <= '0';
	            a             <= '1';
	            b             <= '1';
	
	        when ZERO_OUTPUT =>
	            load_result   <= '0';
	            clr_cntr_bar  <= '0';
	            cnt_en        <= '0';
	            busy_bar      <= '0';
	            a             <= '0';
	            b             <= '0';
				
	    end case;
	end process;

end behavioral;


-------------------------------------------------------------------------------
--
-- TC514 CTRL
--
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity tc514cntrl is
	generic (n : integer := 16);
	port (
		soc : in std_logic;
		rst_bar : in std_logic;
		clk : in std_logic;
		cmptr : in std_logic;
		a : out std_logic;
		b : out std_logic;
		dout : out std_logic_vector (n-1 downto 0);
		busy_bar : out std_logic
	);	
end tc514cntrl;

architecture tc514cntrl of tc514cntrl is
	signal clk_dvd, max_cnt, cnt_en, clr_cntr_bar, load_result : std_logic;
	signal q : std_logic_vector (15 downto 0);
begin
	
	u0 : entity work.freq_div port map (clk => clk, rst_bar => rst_bar, divisor => "0100", clk_dvd => clk_dvd);
	u1 : entity work.binary_cntr port map (clk => clk, cnten1 => cnt_en, cnten2 => clk_dvd, up => '1', clr_bar => clr_cntr_bar, rst_bar => rst_bar, max_cnt => max_cnt, q => q); 
	u2 : entity work.TC514fsm port map (clk => clk, rst_bar => rst_bar, soc => soc, cmptr => cmptr, max_cnt => max_cnt, clk_dvd => clk_dvd, a => a, b => b, busy_bar => busy_bar, cnt_en => cnt_en, clr_cntr_bar => clr_cntr_bar, load_result => load_result);
	u3 : entity work.out_reg port map (clk => clk, rst_bar => rst_bar, enable => load_result, d => q, q => dout);

end tc514cntrl;
