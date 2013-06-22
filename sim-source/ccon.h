/*

  console control .. 

  07.07.2000 Tim Böscke

  */


#ifndef _CCON_H
#define _CCON_H

/* Startup */

void CCON_Init(void);
void CCON_Exit(void);


void CCON_GotoXY(int x,int y);

/* bits 0-2: RGB, bit 3: Intensity */
void CCON_SetColor(int text,int background);

void CCON_DrawHLine(int x1,int x2,int y1,char c);
void CCON_DrawVLine(int x1,int y1,int y2,char c);
void CCON_DrawRect(int x1,int y1,int x2,int y2,char c);
/* Keyboard */

int  CCON_KeyHit(void);
unsigned char CCON_GetKey(void);


/* HACK: CCON_GetKey remaps certain function keys arbitrary to unused keycodes.. */

#define CCK_CRSRUP		200
#define CCK_CRSRDOWN	201
#define CCK_CRSRLEFT	202
#define CCK_CRSRRIGHT	203

#define CCK_PGUP		204
#define CCK_PGDOWN		205
#define CCK_INS			206
#define CCK_DEL			207
#define CCK_END			208
#define CCK_POS1		209

#define CCK_ENTER		0x0d
#define CCK_BACKSPACE   8

#endif
