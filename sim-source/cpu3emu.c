/*

  8 Bit CPU 2 Emulation
  
	04.07.2000 Tim B"oscke
	09.07.2000
	22.09.2000 Tim B"oscke, changed to CPU8BIT1
	12.01.2001 Tim B"oscke, changed to CPU8BIT2
	
	  The first version was C++, but got too bloated because
	  of too generous OOP concept of the hardware.
	 
*/

#include <stdio.h>
#include <conio.h>
#include "hexfileio.h"
#include "ccon.h"
#include "Cpu8Bit2.h"

/* Yeah.. even global variables. True messy Ansi-c *nix hackerstyle .. bah */

#define MAX_BRANCHTARGETS 200
#define MAX_BREAKPOINTS 16

static 	int numbreakpoints=0;
static	int breakpoints[MAX_BREAKPOINTS];
static  int numbranchtargets=0;
static  int	branchtargets[MAX_BRANCHTARGETS];

static void print_memory(Cpu8Bit2State *myCPU,int x,int y,int len,int pos)
{
	int i,j;
	char buf[20];
	int ilen;
	int len2=(len-1)/2;
	int	npos=pos;
	
	for (i=0; i<len2; i++)
	{
		npos=CPU_PreviousAdress(myCPU,npos);
	}
	
	CCON_SetColor(7,0);
	for (i=y; i<(y+len); i++)
	{
		int cf,cb;
		
		cf=7;
		cb=0;
		
		CCON_GotoXY(x,i);
		
		if (npos==pos) 
		{
			cf=0;
			cb^=0xf;
		}
		
		if (npos==(int)myCPU->PC) cb=(cb&0x08)|0x02;
		
		for (j=0; j<numbreakpoints; j++)
		{
			if (npos==breakpoints[j])
			{
				if (cb==0x0f)
					cb=4+8;
				else
					cb=(cb&0x0a)|0x04;
				
				break;
			}
		}
		
		CCON_SetColor(cf,cb);
		printf("%.3X  ",npos);
		
		ilen=CPU_Disassemble(myCPU,npos,buf);
		CCON_GotoXY(x+11,i);
		printf("%s",buf);
		
		CCON_GotoXY(x+5,i);
		printf("%.2X    ",myCPU->Ram[npos]);
		
	//	CCON_GotoXY(x+20,i);
	//	printf(" ");
		
		if (ilen==2) 
		{
			CCON_GotoXY(x+8,i);
			printf("%.2X ",myCPU->Ram[npos+1]);
		}
		
		for (j=0; j<numbranchtargets; j++)	/* TODO:hashing might be a good idea here */
		{
			if (branchtargets[j]==npos)
			{
				CCON_GotoXY(x+3,i);
				printf("<");
				break;
			}
		}
		
		npos=CPU_NextAdress(myCPU,npos);
	}
	
	CCON_SetColor(7,0);
}

static void print_regs(Cpu8Bit2State *myCPU,int x,int y)
{
	int i;
	
	for (i=y; i<(y+16); i++)
	{
		CCON_GotoXY(x,i);
//		printf("R%.2i:%X",i-y,myCPU->Regs[i-y]);
	}
}

static void print_scratch(Cpu8Bit2State *myCPU,int x,int y)
{
	int i;
	
	for (i=y; i<(y+8); i++)
	{
		CCON_GotoXY(x,i);
//		printf("S%.2i:%X",i-y,myCPU->Regs[24+i-y]);
	}
}

static void print_io(Cpu8Bit2State *myCPU,int x,int y)
{
	int i;
	
	for (i=y; i<(y+8); i++)
	{
		CCON_GotoXY(x,i);
//		printf("P%.2i:%X",i-y,myCPU->Regs[16+i-y]);
	}
}

static void print_cpustate(Cpu8Bit2State *myCPU,int x,int y)
{
	CCON_GotoXY(x,y);
	printf("C:%i PC:$%.2X Akku:$%.2X",myCPU->CFlag,myCPU->PC,myCPU->Akku);
}

static void inputstring(int x,int y,int maxlen, int textcolor,int bgcolor, int cursorcolor, char *string)
{
	int fin=0;
	int pos=0;
	unsigned char cin;
	int i;
	
	while (!fin)
	{
		CCON_GotoXY(x,y);
		CCON_SetColor(textcolor,bgcolor);
		printf("%s",string);
		CCON_GotoXY(x+pos,y);
		CCON_SetColor(textcolor,cursorcolor);
		printf("%c",string[pos]);
		
		while (!CCON_KeyHit());
		
		switch (cin=CCON_GetKey())
		{
		case CCK_BACKSPACE:
			{
				if ((pos==(maxlen-1))&&(string[maxlen-1]!=' '))
					string[maxlen-1]=' ';
				else
					if (pos>0)
					{
						pos--;
						for (i=pos; i<(maxlen-1); i++)
							string[i]=string[i+1];
						string[maxlen-1]=' ';
					}
			}
			break;
		case CCK_ENTER:
			{
				CCON_GotoXY(x,y);
				CCON_SetColor(textcolor,bgcolor);
				printf("%s",string);
				fin=1;
			}
			break;
		case CCK_CRSRLEFT:
			{
				if (pos>0) pos--;
			}
			break;
		case CCK_CRSRRIGHT:
			{
				if (pos<maxlen) pos++;
			}
			break;
		case CCK_DEL:
			{
				for (i=pos; i<(maxlen-1); i++)
					string[i]=string[i+1];
				string[maxlen-1]=' ';
			}
			break;
		case CCK_CRSRUP:
		case CCK_CRSRDOWN:
		case CCK_PGUP:
		case CCK_PGDOWN:
		case CCK_INS:
		case CCK_POS1:
		case CCK_END:
		case 0:
			break;
		default:
			{
				if (pos<maxlen)
				{
					for (i=maxlen-2; i>=pos; i--)
						string[i+1]=string[i];
					string[pos++]=cin;
				}
				if (pos==maxlen) pos--;
			}
			break;
		}
	}
}

int main(int argc,char *argv[] )
{
	
	HexFile *myhf;
	char *inputfile="test.o";
    Cpu8Bit2State *myCPU;
	
	char *displaymodes[]={"ALL           ","EVERY 3RD     ","EVERY 10TH    ","EVERY 100TH   ","EVERY 1000TH  ","EVERY 10000TH "};
	char *runmodes[]={"RUN           ","STOP AT BRANCH","S.TAKEN BRANCH"};
	int runmode=0;
	int displaymode=0;
	int exit=0;
	int doit=0;
	int showit=0;
	int showcount=1;
	int showmax=1;
	int single=0;
	u32	viewpos=0;
	u32 tpos;
	int i;	
	if (argc>1) inputfile=argv[1];
	
	myhf=HexFileRead(HFT_SMAL,inputfile); 
	
	if (!myhf) 
	{
		printf("Object File not found!\n");
		return 0;
	}
	
	CCON_Init();

	myCPU=CPU_New();

	CPU_LoadProgram(myCPU,myhf);

	
	/*
	Build branch target table
	*/
	
	tpos=0;
	while (tpos<myhf->length)	/* ATT: bug prone with diff. word size ! */
	{
		u32 bt;
		
		bt=CPU_BranchTarget(myCPU,tpos);
		
		if (bt!=0xffffffff)
		{
			branchtargets[numbranchtargets++]=bt;
			if (numbranchtargets>=MAX_BRANCHTARGETS) break;	/* nuff */
		}
		
		tpos=CPU_NextAdress(myCPU,tpos);
	}
	
	/* Build Screen */
	
	CCON_SetColor(4+8,7);
	CCON_DrawHLine(0,79,0,' ');
	CCON_GotoXY(17,0);
	printf("8Bit CPU2 Emulator v1.0b 4.7.00-12.1.01 Tim Boescke");
	CCON_SetColor(0,7);
	CCON_DrawVLine(0,1,20,' ');
	CCON_DrawVLine(79,1,20,' ');
	CCON_DrawVLine(39,1,20,' ');
	CCON_DrawVLine(40,1,20,' ');
	CCON_DrawHLine(0,79,21,' ');
	
	CCON_SetColor(7,0);
	CCON_GotoXY(44,3);
	printf("Run");CCON_SetColor(7+8,0);printf("M");CCON_SetColor(7,0);printf("ode:");
	CCON_GotoXY(44,4);
	CCON_SetColor(7+8,0);printf("D");CCON_SetColor(7,0);printf("isplayMode:");
	CCON_GotoXY(44,5);
	CCON_SetColor(7+8,0);printf("B");CCON_SetColor(7,0);printf("reakpoints:");
	
	CCON_GotoXY(44,7);
	CCON_SetColor(7+8,0);printf("S");CCON_SetColor(7,0);printf("tep");
	CCON_GotoXY(44,8);
	CCON_SetColor(7+8,0);printf("R");CCON_SetColor(7,0);printf("eset");
	CCON_GotoXY(59,7);
	CCON_SetColor(7+8,0);printf("Q");CCON_SetColor(7,0);printf("uit");
	CCON_GotoXY(59,9);
	CCON_SetColor(7+8,0);printf("Space");CCON_SetColor(7,0);printf(": Toggle run");
	
	CCON_GotoXY(44,9);
	CCON_SetColor(7+8,0);printf("G");CCON_SetColor(7,0);printf("o to:  ");CCON_SetColor(7,1);printf("$    ");
	
	CCON_GotoXY(44,11);
	CCON_SetColor(7,0);
	printf("Use cursor/PgUp/PgDown to scroll");
	
	CCON_SetColor(7,1);
	CCON_GotoXY(2,1);
	printf("Program:");
	/*	
	CCON_GotoXY(24,1);
	printf("Regs:");
	CCON_GotoXY(30,1);
	printf("Scratch:");
	*/
	CCON_SetColor(7,1);
	CCON_GotoXY(59,3);
	printf("%s",runmodes[runmode]);
	CCON_GotoXY(59,4);
	printf("%s",displaymodes[displaymode]);
	CCON_GotoXY(59,5);
	printf("%.2i            ",numbreakpoints);
	
	CCON_SetColor(7,0);
	CCON_GotoXY(44,13);
	printf("Cycles:");
	CCON_GotoXY(44,14);
	printf("Instructions:");
	CCON_GotoXY(44,15);
	printf("Average CPI:");
	
	showit=1;
	
	while (!exit)
	{
		if (((showcount==1)&&(doit))||!doit)
		{
			if (CCON_KeyHit())
			{
				unsigned char c=CCON_GetKey();
				switch (c)
				{
				case CCK_CRSRDOWN:
					{
						{
							viewpos=CPU_NextAdress(myCPU,viewpos);
							showit=1;
						}
					}
					break;
				case CCK_CRSRUP:
					{
						{
							viewpos=CPU_PreviousAdress(myCPU,viewpos);
							showit=1;
						}
					}
					break;
				case CCK_PGDOWN:
					{						
						int i;							
						for (i=0; i<8; i++) viewpos=CPU_NextAdress(myCPU,viewpos);
						showit=1;						
					}
					break;
				case CCK_PGUP:
					{						
						int i;							
						for (i=0; i<8; i++) viewpos=CPU_PreviousAdress(myCPU,viewpos);
						showit=1;
					}
					break;
				case 'b':
					{
						int i,k,nb=1;
						
						for (i=0; i<numbreakpoints; i++)
						{
							if ((int)viewpos==breakpoints[i])
							{
								for (k=i; k<(numbreakpoints-1); k++)
								{
									breakpoints[k]=breakpoints[k+1];
								}
								numbreakpoints--;
								nb=0;
							}
						}
						
						if ((nb)&&(numbreakpoints<MAX_BREAKPOINTS)) breakpoints[numbreakpoints++]=viewpos;
						showit=1;
						
						CCON_SetColor(7,1);
						CCON_GotoXY(59,5);
						printf("%.2i           ",numbreakpoints);
					}
					break;
				case ' ':
					{
						showit=1;
						doit^=1;
					}
					break;
				case 'd':
					{
						displaymode++;
						displaymode%=6;
						CCON_GotoXY(59,4);
						CCON_SetColor(7,1);
						printf("%s",displaymodes[displaymode]);
						switch (displaymode)
						{
						case 0:
							showmax=1;
							break;
						case 1:
							showmax=3;
							break;
						case 2:
							showmax=10;
							break;
						case 3:
							showmax=100;
							break;
						case 4:
							showmax=1000;
							break;
						case 5:
							showmax=10000;
							break;
						case 6:
							showmax=-1;
							break;
						}
						showcount=showmax;
						showit=1;
					}
					break;
				case 'm':
					{
						runmode++;
						runmode%=3;
						CCON_SetColor(7,1);
						CCON_GotoXY(59,3);
						printf("%s",runmodes[runmode]);
						if (runmode==0)
						{
							showcount=1;
							showmax=1;
						}
						else
						{
							switch (displaymode)
							{
							case 0:
								showmax=1;
								break;
							case 1:
								showmax=3;
								break;
							case 2:
								showmax=10;
								break;
							case 3:
								showmax=100;
								break;
							case 4:
								showmax=1000;
								break;
							case 5:
								showmax=10000;
								break;
							case 6:
								showmax=-1;
								break;
							}
							showcount=showmax;	
						}
						showit=1;
					}
					break;
				case 'q':
					{
						exit=1;
					}
					break;
				case 's':
					{
						doit=1;
						single=1;
						showit=1;
					}	
					break;
				case 'r':
					{
						CPU_Reset(myCPU);
						showcount=1;
						showit=1;
						viewpos=0;
						doit=0;
					}
					break;
				case 'j':
					{
						u32 tgt=CPU_BranchTarget(myCPU,viewpos);
						if (tgt!=0xffffffff) viewpos=tgt;
						showit=1;
					}
					break;
				case 'g':
					{
						char inp[]="    ";
						inputstring(53,9,4,7+8,1,8+4,inp);
						sscanf(inp,"%X",&viewpos);
						CCON_GotoXY(53,9);
						/* align to matching instruction */
						viewpos=CPU_PreviousAdress(myCPU,viewpos);
						viewpos=CPU_NextAdress(myCPU,viewpos);
						printf("%.3X ",viewpos);
						showit=1;
					}
					break;
				}
			}
		}
		
		
		if (doit)
		{
			CPU_Step(myCPU);
			viewpos=myCPU->PC;
		}
		
		if ((showit)||((showcount==1)&&doit))
		{
			
			CCON_GotoXY(59,8);
			if ((CPU_BranchTarget(myCPU,viewpos)==0xffffffff)||doit)
			{
				CCON_SetColor(7+8,0);printf("J");CCON_SetColor(7,0);printf("ump");
			}
			else
			{
				CCON_SetColor(7+8,4);printf("J");CCON_SetColor(7,4);printf("ump");
			}
			
			CCON_SetColor(7,4);
			print_cpustate(myCPU,2,2+16+1);
			CCON_SetColor(7,1);
			CCON_GotoXY(59,13);
			printf("%.8i",myCPU->NumCycles);
			CCON_GotoXY(59,14);
			printf("%.8i",myCPU->NumInstructions);
			CCON_GotoXY(59,15);
			if (myCPU->NumInstructions>0) printf("%1.6f",(float)myCPU->NumCycles/(float)myCPU->NumInstructions);
			
			print_memory(myCPU,2,2,16,viewpos);
	//		print_regs(myCPU,24,2);
	//		print_scratch(myCPU,31,2);
	//		print_io(myCPU,31,2+8);
		}
		
		if (--showcount==0) showcount=showmax;
		showit=0;

		if (doit)
		{
			int i;
			for (i=0; i<numbreakpoints; i++)
			{
				if (breakpoints[i]==(int)myCPU->PC)
				{
					doit=0;
					showit=1;
					break;
				}
			}
		}

		
		
		if (doit)
		{
			switch (runmode)
			{
			case 0: /* RUN */
				break;
			case 1: /* STOP AT BRANCH */
				{
					if (CPU_EvaluateBranch(myCPU,myCPU->PC)!=CPUC_NOBRANCH) 
					{
						doit=0;
						showit=1;
					}
				}
			case 2: /* STOP AT TAKEN BRANCH */
				{
					int blerk=CPU_EvaluateBranch(myCPU,myCPU->PC);
					if ((blerk==CPUC_BRANCHTAKEN)||(blerk==CPUC_BRANCHALWAYS))
					{
						doit=0;
						showit=1;
					}
				}
				
			}
		}
		if (single) single=doit=0;
		
		
	}
	
	CPU_Free(myCPU);
	CCON_Exit();
	HexFileFree(myhf);
	return 0;
}
