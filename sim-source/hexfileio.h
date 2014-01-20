/*
	HexfileIO.H

	Some functions do read Hex files. 
	(SMAL output)

     04.07.2000 Tim B"oscke
  */



#ifndef __HEXFILEIO_H
#define __HEXFILEIO_H

typedef unsigned char u8;
typedef unsigned int  u32;

typedef struct
{
	u8 *data;
	u32 length;
} HexFile;

enum HexFileType{HFT_SMAL};

/* Read File
   returns NULL when failed */

HexFile *HexFileRead(u32 type,char *filename);
void	 HexFileFree(HexFile *data);

#endif
