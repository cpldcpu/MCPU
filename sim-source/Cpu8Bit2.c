/*

   8 Bit CPU2 Emulation

   07.07.2000 Tim Böscke
   22.09.2000 Tim böscke, changed it to 8BitCpu1
   11.01.2001 Tim böscke, changed it to 8BitCpu2

    CPU model static variables

  */

#include "Cpu8Bit2.h"
#include <memory.h>
#include <malloc.h>
#include <stdio.h>

/* new CPU */

Cpu8Bit2State* CPU_New(void)
{
	int i;

	Cpu8Bit2State *mycpu;

	mycpu=(Cpu8Bit2State*)malloc(sizeof(Cpu8Bit2State));
	mycpu->Ram=(u8*)malloc(RamSize*2);

	memset(mycpu->Ram,0,RamSize);

	CPU_Reset(mycpu);

	return mycpu;
}

void CPU_Free(Cpu8Bit2State *state)
{
	free(state->Ram);
	free(state);
}

void CPU_Reset(Cpu8Bit2State *state)
{
	state->PC=0;
	state->CFlag=0;  
	state->Akku=0;
	state->NumInstructions=0;
	state->NumCycles=0;
}

void CPU_LoadProgram(Cpu8Bit2State *state,HexFile *file)
{
	int i,max;

	max=file->length;
	if (max>RamSize) max=RamSize;

	for (i=0; i<max; i++) state->Ram[i]=file->data[i];

}

void CPU_Step(Cpu8Bit2State *state)
{
	u32 IR1,ADR1;

	IR1=state->Ram[state->PC++];
	ADR1=IR1&(RamSize-1);
	state->NumInstructions++;
	state->NumCycles++;

	switch (IR1>>6)
	{
	case 0:	// NOR
		{
			u32 res=((u32)state->Ram[ADR1]|(u32)state->Akku)^0x0ff;
			state->Akku=res;
			state->NumCycles++;
		}
		break;
	case 1:	// ADD
		{
			u32 res=(u32)state->Ram[ADR1]+(u32)state->Akku;
			state->CFlag=res>>8;	// carry from the add!	
			state->Akku=res;
			state->NumCycles++;
		}
		break;
	case 2:	// STA
		{
			state->Ram[ADR1]=state->Akku;
			state->NumCycles++;
		}
		break;
	case 3:	// JCC 
		{
			if (!state->CFlag)
			{
				state->PC=ADR1;
			}
			else
			{
				state->NumCycles++;	// branch not taken
			}
			state->CFlag=0;
		}
		break;
	}

	state->PC&=(RamSize-1);
}

u32	CPU_Disassemble(Cpu8Bit2State *state,u32 adress,char *string)
{
	u32 IR1,ADR1;
	u32 words=1;
	adress&=(RamSize-1);

	IR1=state->Ram[adress];
	ADR1=IR1&(RamSize-1);

	switch (IR1>>6)
	{
	case 0:	// NOR
		{
			sprintf(string,"NOR $%.2X [$%.2X]",ADR1,state->Ram[ADR1]);
		}
		break;
	case 1:	// ADD
		{
			sprintf(string,"ADD $%.2X [$%.2X]",ADR1,state->Ram[ADR1]);
		}
		break;
	case 2:	// STA
		{
			sprintf(string,"STA $%.2X      ",ADR1);
		}
		break;
	case 3:	// JCC 
		{
			sprintf(string,"JCC $%.2X      ",ADR1);
		}
		break;
	}

	return words;
}

u32	CPU_NextAdress(Cpu8Bit2State *state,u32 adress)
{
	return (adress+1)&(RamSize-1);
}

u32	CPU_PreviousAdress(Cpu8Bit2State *state,u32 adress)
{
	
	return (adress-1)&(RamSize-1);
}

/* returns branch target, if instruction is branch. Otherwhise 0xffffffff */

u32	CPU_BranchTarget(Cpu8Bit2State *state,u32 adress)
{
	int IR;
	IR=state->Ram[adress];
	if ((IR&0xc0)!=0xc0) return 0xffffffff;

	return (IR&(RamSize-1));
}

u32 CPU_EvaluateBranch(Cpu8Bit2State *state,u32 adress)
{
	int IR;
	IR=state->Ram[adress];

	if ((IR&0xC0)!=0xc0)
	{
			return CPUC_NOBRANCH;
	}
	else
	{
		if (state->CFlag) return CPUC_BRANCHNOTTAKEN;
		else return CPUC_BRANCHTAKEN;

	}
}
