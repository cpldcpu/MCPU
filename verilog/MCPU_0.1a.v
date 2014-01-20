//
// Minimal 8 Bit CPU
//
// 01-02/2001 Tim B"oscke
// 10   /2001 changed to synch. reset
// 10   /2004 Verilog version, unverified!
//
// t.boescke@tuhh.d


module mcpu(data,adress,oe,we,rst,clk);

inout [7:0] data;
output [5:0] adress;
output oe;
output we;
input rst;
input clk;

reg [8:0] accumulator; // accumulator(8) is carry !
reg [5:0] adreg;
reg [5:0] pc;
reg [2:0] states;

initial begin
	$display("time, state, adreg, pc, rst, clk, data, adress, oe");
	$monitor($stime,",states=",states, ", adreg=", adreg, ",pc=", pc, ",rst=", rst,",clk=",clk,",data=",data,",address=",adress,",oe =",oe); 
end

	always @(posedge clk)
		if (~rst) begin
			adreg 	  <= 0;
			states	  <= 0;
			accumulator <= 0;	
			pc <= 0;
		end
		else begin
			// PC / Address path
			if (~|states) begin
				pc	 <= adreg + 1'b1;
				adreg <= data[5:0];  // was adreg <=pc, aw fix.
			end
			else adreg <= pc;
		
			// ALU / Data Path
			case(states)
				3'b010 : accumulator 	 <= {1'b0, accumulator[7:0]} + {1'b0, data}; // add
				3'b011 : accumulator[7:0] <= ~(accumulator[7:0]|data); // nor
				3'b101 : accumulator[8]   <= 1'b0; // branch not taken, clear carry					   
			endcase							// default:  instruction fetch, jcc taken

			// State machine
			if (|states) states <= 0;
			else begin 
				if ( &data[7:6] && accumulator[8] ) states <= 3'b101;
				else states <= {1'b0, ~data[7:6]};
			end
		end
// output
assign adress = adreg;
assign data   = states!=3'b001 ?  8'bZZZZZZZZ : accumulator[7:0]; 
assign oe     = clk | ~rst | states==3'b001 | states==3'b101 ; 
assign we     = clk | ~rst | (states!=3'b001) ; 

endmodule
