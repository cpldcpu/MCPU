/*

  console control .. 

  07.07.2000 Tim Böscke

  */

#include "ccon.h"
#include <stdio.h>

/* WIN32 stuff .. */

#ifdef _WIN32
#include <windows.h>
#include <conio.h>

static HANDLE conhandle;
static CONSOLE_CURSOR_INFO curinfo;

void CCON_Init(void)
{
    CONSOLE_SCREEN_BUFFER_INFO scrinf;
	COORD coord;
	DWORD blork;

	conhandle = GetStdHandle(STD_OUTPUT_HANDLE);
	GetConsoleCursorInfo(conhandle,&curinfo);
	curinfo.bVisible=FALSE;
	SetConsoleCursorInfo(conhandle,&curinfo);

	/* clear console */


	GetConsoleScreenBufferInfo(conhandle,&scrinf);

	coord.X=scrinf.srWindow.Left;
	coord.Y=scrinf.srWindow.Top;

	FillConsoleOutputCharacter(conhandle,' ',
		(scrinf.srWindow.Right-scrinf.srWindow.Left+1)*(scrinf.srWindow.Bottom-scrinf.srWindow.Top+1),
			coord,&blork);
}

void CCON_GotoXY(int x,int y)
{
	COORD coord = {x, y};
    SetConsoleCursorPosition(conhandle, coord);
}

void CCON_SetColor(int text,int background)
{
	WORD attrib=0;
	if (text & 1) attrib|=FOREGROUND_BLUE;
	if (text & 2) attrib|=FOREGROUND_GREEN;
	if (text & 4) attrib|=FOREGROUND_RED;
	if (text & 8) attrib|=FOREGROUND_INTENSITY;

	if (background & 1) attrib|=BACKGROUND_BLUE;
	if (background & 2) attrib|=BACKGROUND_GREEN;
	if (background & 4) attrib|=BACKGROUND_RED;
	if (background & 8) attrib|=BACKGROUND_INTENSITY;

	SetConsoleTextAttribute(conhandle,attrib);
}

void CCON_Exit(void)
{
	curinfo.bVisible=TRUE;
	SetConsoleCursorInfo(conhandle,&curinfo);
	CCON_SetColor(7,0);
	CCON_GotoXY(0,24);
}

int  CCON_KeyHit(void)
{
	return kbhit();
}

unsigned char CCON_GetKey(void)
{
	unsigned char cc;
	cc=getch();

	if (cc!=0xe0) return cc;

	cc=getch();

	switch(cc)
	{
	case 0x50:
		return CCK_CRSRDOWN;
	case 0x48:
		return CCK_CRSRUP;
	case 0x4B:
		return CCK_CRSRLEFT;
	case 0x4D:
		return CCK_CRSRRIGHT;
	case 0x49:
		return CCK_PGUP;
	case 0x51:
		return CCK_PGDOWN;
	case 0x52:
		return CCK_INS;
	case 0x53:
		return CCK_DEL;
	case 0x5F:
		return CCK_END;
	case 0x47:
		return CCK_POS1;
	}
}

#endif


/* System independent */

void CCON_DrawHLine(int x1,int x2,int y1,char c)
{
	int i;
	for (i=x1; i<=x2; i++)
	{
		CCON_GotoXY(i,y1);
		printf("%c",c);
	}

}

void CCON_DrawVLine(int x1,int y1,int y2,char c)
{
	int i;
	for (i=y1; i<=y2; i++)
	{
		CCON_GotoXY(x1,i);
		printf("%c",c);
	}

}

void CCON_DrawRect(int x1,int y1,int x2,int y2,char c)
{
	int i;
	
	for (i=y1; i<y2; i++)
		CCON_DrawHLine(x1,x2,i,c);
}
