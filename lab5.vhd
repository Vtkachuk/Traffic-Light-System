LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

--
-- 7-segment display driver. It displays a 4-bit number on 7-segments 
-- This is created as an entity so that it can be reused many times easily
--

ENTITY SevenSegment IS PORT (
   
   dataIn      :  IN  std_logic_vector(3 DOWNTO 0);   -- The 4 bit data to be displayed
   blanking    :  IN  std_logic;                      -- This bit turns off all segments
   
   segmentsOut :  OUT std_logic_vector(6 DOWNTO 0)    -- 7-bit outputs to a 7-segment
); 
END SevenSegment;

ARCHITECTURE Behavioral OF SevenSegment IS

-- 
-- The following statements convert a 4-bit input, called dataIn to a pattern of 7 bits
-- The segment turns on when it is '0' otherwise '1'
-- The blanking input is added to turns off the all segments
--

BEGIN

   with blanking & dataIn SELECT --  gfedcba        b3210      -- D7S
      segmentsOut(6 DOWNTO 0) <=    "1000000" WHEN "00000",    -- [0]
                                    "1111001" WHEN "00001",    -- [1]
                                    "0100100" WHEN "00010",    -- [2]      +---- a ----+
                                    "0110000" WHEN "00011",    -- [3]      |           |
                                    "0011001" WHEN "00100",    -- [4]      |           |
                                    "0010010" WHEN "00101",    -- [5]      f           b
                                    "0000010" WHEN "00110",    -- [6]      |           |
                                    "1111000" WHEN "00111",    -- [7]      |           |
                                    "0000000" WHEN "01000",    -- [8]      +---- g ----+
                                    "0010000" WHEN "01001",    -- [9]      |           |
                                    "0001000" WHEN "01010",    -- [A]      |           |
                                    "0000011" WHEN "01011",    -- [b]      e           c
                                    "0100111" WHEN "01100",    -- [c]      |           |
                                    "0100001" WHEN "01101",    -- [d]      |           |
                                    "0000110" WHEN "01110",    -- [E]      +---- d ----+
                                    "0001110" WHEN "01111",    -- [F]
                                    "1111111" WHEN OTHERS;     -- [ ]

END Behavioral;

--------------------------------------------------------------------------------
-- Main entity
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY Lab5 IS
   PORT(
      
      clock_50   : IN  STD_LOGIC;
      sw         : IN  STD_LOGIC_VECTOR(17 DOWNTO 0); -- 18 dip switches on the board

      ledr       : OUT STD_LOGIC_VECTOR(17 DOWNTO 0); -- LEDs, many Red ones are available
      ledg       : OUT STD_LOGIC_VECTOR( 8 DOWNTO 0); -- LEDs, many Green ones are available
      hex0, hex2, hex1, hex3, hex4, hex5, hex6, hex7 : OUT STD_LOGIC_VECTOR( 6 DOWNTO 0)  -- seven segments to display numbers
);
END Lab5;

ARCHITECTURE SimpleCircuit OF Lab5 IS

--
-- In order to use the "SevenSegment" entity, we should declare it with first
-- 

   COMPONENT SevenSegment PORT(        -- Declare the 7 segment component to be used
      dataIn      : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      blanking    : IN  STD_LOGIC;
      segmentsOut : OUT STD_LOGIC_VECTOR(6 DOWNTO 0)
   );
   END COMPONENT;
----------------------------------------------------------------------------------------------------
   CONSTANT CLK_DIV_SIZE: INTEGER := 25;     -- size of vectors for the counters

   SIGNAL OneHzBinCLK:  STD_LOGIC; -- binary 1 Hz clock
   SIGNAL OneHzModCLK:  STD_LOGIC; -- main 1 Hz clock to drive FSM
   SIGNAL TenHzModCLK:  STD_LOGIC; -- modulus 10 Hz clock

   SIGNAL bin_counter:  UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset binary counter to zero
   SIGNAL mod_counter1:  UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset modulus counter to zero
   SIGNAL mod_counter2:  UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset modulus counter to zero
   SIGNAL mod_terminal1: UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset terminal count of modulus counter to zero
   SIGNAL mod_terminal2: UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset terminal count of modulus counter to zero
   
   TYPE STATES IS (STATE0, STATE1, STATE2, STATE3, STATE4, STATE5);   -- list all the STATES
   SIGNAL state, next_state:  STATES;                 -- current and next state signals od type STATES
   
   SIGNAL state_number: STD_LOGIC_VECTOR(3 DOWNTO 0); -- binary state number to display on seven-segment

   SIGNAL second_counter, state_counter, ns_wait_counter, ew_wait_counter: UNSIGNED(3 DOWNTO 0);
----------------------------------------------------------------------------------------------------

BEGIN

   BinCLK: PROCESS(clock_50)
   BEGIN
      IF (rising_edge(clock_50)) THEN -- binary counter increments on rising clock edge
         bin_counter <= bin_counter + 1;
      END IF;
   END PROCESS;
   OneHzBinCLK <= std_logic(bin_counter(CLK_DIV_SIZE-1)); -- binary counter MSB
   LEDG(2) <= OneHzBinCLK;
----------------------------------------------------------------------------------------------------
-- terminal count for modulus counter for F0= 50 MHz clock input (T0 = 20 ns) 
   mod_terminal1 <= "1011111010111100000111111";
   mod_terminal2 <= "0001001100010010110011111";
   
   ModCLK: PROCESS(clock_50) 
   BEGIN
      IF (rising_edge(clock_50)) THEN -- modulus counter increments on rising clock edge
         IF (mod_counter2 = mod_terminal2) THEN       -- half period
            TenHzModCLK <= NOT TenHzModCLK;                 -- toggle
            mod_counter2 <= to_unsigned(0,CLK_DIV_SIZE); -- reset counter
         ELSE
            mod_counter2 <= mod_counter2 + 1;
         END IF;
      END IF;
   END PROCESS;
   
   FirstHZModCLK: PROCESS(TenHzModCLK) 
   BEGIN
      IF (rising_edge(TenHzModCLK)) THEN -- modulus counter increments on rising clock edge
         IF (mod_counter1 = "0000000000000000000000100") THEN       -- half period
            OneHzModCLK <= NOT OneHzModCLK;                 -- toggle
            mod_counter1 <= to_unsigned(0,CLK_DIV_SIZE); -- reset counter
         ELSE
            mod_counter1 <= mod_counter1 + 1;
         END IF;
      END IF;
   END PROCESS;
   LEDG(1) <= OneHzModCLK;
   LEDG(0) <= TenHzModCLK;
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
   FSM: PROCESS(second_counter, state, TenHzModCLK, next_state, sw) -- main FSM
   BEGIN
       	-- The only purpose of this line is to give initial value to the signal 'next_state' in order to avoid latch creation. 
      ledr(15 DOWNTO 0) <= "0000000000000000";
      
      CASE state IS
         WHEN STATE0 =>
            state_number <= "0000";
            LEDG(8) <= TenHzModCLK;
            LEDR(11) <= '0';
            LEDG(7) <= '0';
            LEDR(0) <= '1';
            IF (second_counter = "0001") THEN
			 next_state <= STATE1;
			ELSE
			 next_state <= STATE0;
			END IF;
         WHEN STATE1 =>
            state_number <= "0001";
			LEDG(8) <= '1';
			LEDR(11) <= '0';
			LEDG(7) <= '0';
            LEDR(0) <= '1';
			IF (second_counter = "0101") THEN
			 next_state <= STATE2;
			ELSE
			 next_state <= STATE1;
			END IF;
         WHEN STATE2 =>
            state_number <= "0010";
			LEDG(8) <= '0';
			LEDR(11) <= TenHzModCLK;
			LEDG(7) <= '0';
            LEDR(0) <= '1';
			IF ((sw(17) = '0' AND second_counter = "0111") OR (sw(17) = '1' AND second_counter = "0111" AND ((sw(16) = '0' AND sw(14) = '1') OR sw(16) = '1'))) THEN
			 next_state <= STATE3;
			ELSIF (second_counter = "0111") THEN
			 next_state <= STATE1;
			ELSE
			 next_state <= STATE2;
			END IF;
         WHEN STATE3 => 
            state_number <= "0011";
			LEDG(8) <= '0';
			LEDR(11) <= '1';
			LEDG(7) <= TenHzModCLK;
            LEDR(0) <= '0';
			IF (second_counter = "1001") THEN
			 next_state <= STATE4;
			ELSE
			 next_state <= STATE3;
			END IF;
		 WHEN STATE4 =>
            state_number <= "0100";
            LEDG(8) <= '0';
			LEDR(11) <= '1';
			LEDG(7) <= '1';
            LEDR(0) <= '0';
			IF (second_counter = "1101") THEN
			 next_state <= STATE5;
			ELSE
			 next_state <= STATE4;
			END IF;
         WHEN OTHERS => -- STATE5
            state_number <= "0101";
            LEDG(8) <= '0';
			LEDR(11) <= '1';
			LEDG(7) <= '0';
            LEDR(0) <= TenHzModCLK;
			IF ((sw(17) = '0' AND second_counter = "1111") OR (sw(17) = '1' AND second_counter = "1111" AND ((sw(16) = '1' AND sw(15) = '1') OR sw(16) = '0'))) THEN
			 next_state <= STATE0;
			ELSIF (second_counter = "1111") THEN
			 next_state <= STATE4;
			ELSE
			 next_state <= STATE5;
			END IF;
      END CASE;
   END PROCESS;
----------------------------------------------------------------------------------------------------
   Counters: PROCESS(state_counter, ns_wait_counter, ew_wait_counter, next_state, sw, state, second_counter, OneHzModCLK)
   BEGIN
	IF (rising_edge(OneHzModCLK)) THEN
		  IF (next_state = state) THEN
			 state_counter <= state_counter + 1;
		  ELSE
			 state_counter <= "0000";
		  END IF;
		  
		  IF (sw(15) = '1' AND second_counter > "1000") THEN
			 ns_wait_counter <= ns_wait_counter + 1;
		  ELSE
			 ns_wait_counter <= "0000";
		  END IF;
		  
		  IF (sw(14) = '1' AND second_counter < "0111") THEN
			 ew_wait_counter <= ew_wait_counter + 1;
		  ELSE
			 ew_wait_counter <= "0000";
		  END IF;
	END IF;
   END PROCESS;
----------------------------------------------------------------------------------------------------
   SeqLogic: PROCESS(OneHzModCLK, second_counter) -- creats sequential logic to latch the state
   BEGIN
      IF (rising_edge(OneHzModCLK)) THEN
	      second_counter <= second_counter + 1;   
	      state <= next_state;
	      IF (sw(17) = '1' AND sw(16) = '0' AND sw(14) = '0' AND second_counter = "0111") THEN
	         second_counter <= "0000";
	      ELSIF (sw(17) = '1' AND sw(16) = '1' AND sw(15) = '0' AND second_counter = "1111") THEN
	         second_counter <= "1000";
	      END IF;      -- on the rising edge of clock the current state is updated with next state
	  END IF;
   END PROCESS;
----------------------------------------------------------------------------------------------------
   D7S0: SevenSegment PORT MAP( state_number, '0', hex0 );
   D7S1: SevenSegment PORT MAP( "0000", '1', hex1 );
   D7S2: SevenSegment PORT MAP( std_logic_vector(state_counter), '0', hex2 );
   D7S3: SevenSegment PORT MAP( "0000", '1', hex3 );
   D7S4: SevenSegment PORT MAP( std_logic_vector(ns_wait_counter), '0', hex4 );
   D7S5: SevenSegment PORT MAP( "0000", '1', hex5 );
   D7S6: SevenSegment PORT MAP( std_logic_vector(ew_wait_counter), '0', hex6 );
   D7S7: SevenSegment PORT MAP( "0000", '1', hex7 );

END SimpleCircuit;
