/*

   4 Bit CPU Emulation, hacker version

   07.07.2000 Tim B"oscke

    CPU model static variables

  */

#ifndef __CPU8Bit2_h
#define __CPU8Bit2_h

#include "hexfileio.h"

typedef struct
{
	/* CPU State */
/*
	u32 PC;
	u8 Regs[32];
	u8 Akku;
	u8 CFlag;
	u8 ZFlag;
	u8 *Rom;
*/
	u8 Akku;
	u32 PC;		// Lets keep it flexible..
	u8  CFlag;
	u8 *Ram;
	/* Statistics */

	u32	NumInstructions;
	u32 NumCycles;

} Cpu8Bit2State;

/* new CPU */

Cpu8Bit2State* CPU_New(void);	

/* load program data into program memory rom/ram */

void CPU_LoadProgram(Cpu8Bit2State *state,HexFile *file);

/* Reset CPU */

void CPU_Reset(Cpu8Bit2State *state);

/* Step */

void CPU_Step(Cpu8Bit2State *state);

/* free CPU struct */

void CPU_Free(Cpu8Bit2State *state);	

/* Disassemble.. returns number of words in instruction */

u32	CPU_Disassemble(Cpu8Bit2State *state,u32 adress,char *string);

/* Return next instruction word adress*/

u32	CPU_NextAdress(Cpu8Bit2State *state,u32 adress);
u32	CPU_PreviousAdress(Cpu8Bit2State *state,u32 adress);

/* returns branch target, if instruction is branch. Otherwhise 0xffffffff */

u32	CPU_BranchTarget(Cpu8Bit2State *state,u32 adress);

/* Returns value depending on branch execution */

u32 CPU_EvaluateBranch(Cpu8Bit2State *state,u32 adress);

#define CPUC_NOBRANCH 0
#define CPUC_BRANCHTAKEN 1
#define CPUC_BRANCHNOTTAKEN 2
#define CPUC_BRANCHALWAYS 3

#define RamSize 64
#endif
