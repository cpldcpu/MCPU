# MCPU - Minimal CPU for a 32 Macrocell CPLD #

MCPU is a minimal cpu aimed to fit into a 32 Macrocell CPLD - one of the smallest available programmable logic devices.
While this CPU is not powerful enough for real world applications it has proven itself as a valuable educational 
tool. The source code is just a single page and easily understood. Both VHDL and Verilog versions are supplied.
The package comes with assembler, emulator and extensive documentation. 

This is an old project from 2001. Since it still seems to be of interest to many, I migrated it to Github 
for easier maintenance. Please refer to the [original project description](https://github.com/cpldcpu/MCPU/blob/master/mcpu.pdf?raw=true) (pdf) for further information.

### Version History ###

* v1.0x - 2001 
	- Released on university homepage
* v1.06b - 2008 
 	- last release on opencores.org
* v1.1 - 2013/06/23
	- Initial release on github
	- Converted readme to markdown
	- Included bugfixed verilog version submitted by A. Wood.

### Archive content ###
```
ASM/
	- example programs.
	- simulator.
	- include file for smal.
	- smal.exe - see license for smal !
VHDL/
	- VHDL source of CPU.
	- VHDL testbench.
	- Memory files.
	- The testbench requires a simple sram implementation.
          I used this one: http://tams-www.informatik.uni-hamburg.de/vhdl/models/sram-simple/sram64kx8.vhd          
 	  From the Hamburg VHDL archive (http://tams-www.informatik.uni-hamburg.de/vhdl/)

verilog/
	- Verilog conversion of CPU source 
	- A small Verilog testbench

SMAL-source/
	- SMAL sourcode.

SIM-source/
   - WIN32 Simulator sourcecode. Porting to other platforms should be easy.
```

### License ###
```
SMAL license:

/* smal32.c   language: C
   copyright 1996 by Douglas W. Jones
                     University of Iowa
                     Iowa City, Iowa  52242
                     USA

   Permission is granted to make copies of this program for any purpose,
   provided that the above copyright notice is preserved in the copy, and
   provided that the copy is not made for direct commercial advantage.

   Note: This software was developed with no outside funding.  If you find
   it useful, and especially if you find it useful in a profit making
   environment, please consider making a contribution to the University
   of Iowa Department of Computer Science in care of:

                     The University of Iowa Foundation
                     Alumni Center
                     University of Iowa
                     Iowa City, Iowa  52242
                     USA
*/
```

The other sources in this package licensed nuder GPL2.

I am always interested in possible uses and modifications to it. Let me know what you are doing
with it.

