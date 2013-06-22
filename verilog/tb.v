
`timescale 1ns / 1ns

module test_bench;
	
wire [7:0] data;
wire [5:0] adress;
wire oe, we;
reg rst,clk;

mcpu dut (data,adress,oe,we,rst,clk);	
RamChip  ram (adress, data, 1'b0, we, oe);

initial begin
	clk <= 0;
	rst <= 1;
	#50;
	clk <= 0;
	rst <= 0;
	#50
	clk <= 1;
	#25;
	rst <= 1;
	#25;
	end
		
initial // Clock generator
	begin
   forever #50 clk = !clk;
  end
  

endmodule


// RAM Model
//
// +-----------------------------+
// |    Copyright 1996 DOULOS    |
// |       Library: Memory       |
// |   designer : John Aynsley   |
// +-----------------------------+

module RamChip (Address, Data, CS, WE, OE);

parameter AddressSize = 6;
parameter WordSize = 8;

input [AddressSize-1:0] Address;
inout [WordSize-1:0] Data;
input CS, WE, OE;

reg [WordSize-1:0] Mem [0:1<<AddressSize];

assign #5 Data = (!CS && !OE) ? Mem[Address] : {WordSize{1'bz}};  // !!!! aw = had to add the #5 dealy to get teh MCPU to work -- the state machine needs the data to hold past the posedge clk.

initial begin  // pre-program the ram
	Mem[61]= 8'b00000000;
	Mem[62]= 8'b11111111;
	Mem[63]= 8'b00000001;
	Mem[00]= 8'b00111110;
	Mem[01]= 8'b01000110;
	Mem[02]= 8'b01111111;
	Mem[03]= 8'b11000010;
	Mem[04]= 8'b11000100;
	Mem[05]= 8'b11000100;
	Mem[06]= 8'b11111100;
	Mem[07]= 8'b00000000;
	
	$monitor($time, "mem[0]=",Mem[0]);
	$monitor($time, "mem[6]=",Mem[6]);
end

always @(CS or WE)
  if (!CS && !WE)
    Mem[Address] = Data;

always @(WE or OE)
  if (!WE && !OE)
    $display("Operational error in RamChip: OE and WE both active");

endmodule


/*
 .=61
zero:
	B 0
allone:
	B 255
one:
	B 1
.=0

MACRO LDA src
	NOR allone
	ADD src	
                             3  
                             4  USE "cpu3.inc" // pasted above
 00003D: 00  FF  01          5  
                             6  label0:
                             7          LDA     count
 000000: 3E  46              8  cloop:  
                             9          ADD     one     
 000002: 7F                 10          JCC     cloop
 000003: C2                 11  loop:
                            12          JMP loop
 000004: C4  C4             13  
                            14  count:  
                            15          DCB     (256-10)
 000006: F6                 16  END
                    no errors
						  
;------------------------------
;some constants
;

					  
						  
61 00000000
62 11111111
63 00000001
00 00111110
01 01000110
02 01111111
03 11000010
04 11000100
05 11000100
06 11111100
07 00000000
						  
*/


