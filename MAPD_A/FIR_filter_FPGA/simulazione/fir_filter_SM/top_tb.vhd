library ieee;
use ieee.std_logic_1164.all;   --solite librerie
use ieee.numeric_std.all;

use STD.textio.all; --per caricare i file

entity simulazione is --creo la entity simulazione

end entity simulazione;


architecture test of simulazione is -- creo la architecture

    signal CLK100MHZ    : std_logic := '0'; --il master clock dell' fpga
    signal uart_txd_in  : std_logic:= '0'; --l'input al receiver
    signal uart_rxd_out : std_logic:='0'; -- l'output del transmitter
    file file_VECTORS   : text; --file di input
    file file_RESULTS   : text;  -- file di output

begin

  DUT : entity work.top --connetto i fili
    port map (
      CLK100MHZ      => CLK100MHZ,       
      uart_txd_in    => uart_txd_in,      
      uart_rxd_out   => uart_rxd_out);    

--genero il clock
  CLK100MHZ <= not CLK100MHZ after 5 ns; --100Mhz -> 10 ns period

 -- waveform generation
  WaveGen_Proc : process
    variable v_ILINE        : line; --
    variable v_OLINE        : line; --
    variable i_data_integer : integer   := 0;
    variable o_data_integer : integer   := 0;
    variable i_data_slv     : std_logic_vector(7 downto 0) := (others => '0');
    variable o_data_slv     : std_logic_vector(7 downto 0) := (others => '0');
    variable count          : integer := 0; 
    constant c_WIDTH        : natural   := 8;
    constant divisor        : natural   := 867;

  begin
    -- insert signal assignments here
    file_open(file_VECTORS, "input_signal.txt", read_mode); --apro l'input
    file_open(file_RESULTS, "output_results.txt", write_mode); --apro l' output
    wait until rising_edge(CLK100MHZ); --ricorda che questi sono i clock veloci 
    data : while not endfile(file_VECTORS) loop --  ciclo fin quando non finiscono i dati
      readline(file_VECTORS, v_ILINE); --leggo la linea
      read(v_ILINE, i_data_integer);  
      i_data_slv         := std_logic_vector(to_signed(i_data_integer, i_data_slv'length)); -- slave perchè è lo stsso convertito
	wait until rising_edge(CLK100MHZ);
	uart_txd_in <= '0';
	while (count < divisor) loop 
    			wait until rising_edge(CLK100MHZ); 
			count := count + 1; 	--conto per il baudrate
		end loop;
	count := 0;  	
	i_o : for i in 0 to 7 loop
		while (count < divisor) loop
    			wait until rising_edge(CLK100MHZ);
			uart_txd_in     <= std_logic(i_data_slv(i));--mando all' input bit 
			count := count + 1; 	
		end loop;
	count := 0;
	end loop i_o;

	uart_txd_in <= '1'; --send idle bit
	wait until rising_edge(CLK100MHZ); 

	wait until uart_rxd_out = '0';

	--aspetta un baudrate:
	while (count < divisor) loop --new
    			wait until rising_edge(CLK100MHZ); --new 
			count := count + 1; 	--conto per il baudrate
		end loop;
	count := 0;
	o_i : for k in 0 to 7 loop
		while (count < divisor) loop
    			wait until rising_edge(CLK100MHZ);
			o_data_slv(k)     := std_logic(uart_rxd_out);--mando l'output
			count := count + 1; 	
		end loop;
	count := 0;
	end loop o_i;
      o_data_integer := to_integer(signed(o_data_slv));
      --o_data_integer := to_integer(signed(o_data_slv));
      write(v_OLINE, o_data_integer, left, c_WIDTH); -- scrivo l'output 
      writeline(file_RESULTS, v_OLINE);
    end loop data; --chiudo loop data
    file_close(file_VECTORS);--chiudo file
    file_close(file_RESULTS);
    wait;
  end process WaveGen_Proc;



end architecture test;





