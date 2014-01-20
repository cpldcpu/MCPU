/*
	HexfileIO.C

	Some functions to read Hex files. 
	(SMAL output)

     04.07.2000 Tim B"oscke	
	 22.09.2000				

	
  */


#include <stdio.h>
#include <malloc.h>
#include <string.h>

#include "hexfileio.h"


/* Read File
   returns NULL when failed */

HexFile *HexFileRead(u32 type,char *filename)
{
	HexFile *hf;
	switch (type)
	{
	case HFT_SMAL:
		{
			int inp2;
			char inp[200]; /* YES.. real bug prone c programming. */
			u32 maxdat=0;
			u32 loc=0;
			u32 size;
			u32 sizemax;

			char cur;

			FILE *mydat=fopen(filename,"rb");

			if (!mydat) return 0;
			
			fscanf(mydat,"%s\n",inp);
			if (strncmp("R=.",inp,3))
			{
					/* no SMAL file */
				fclose(mydat);
				return 0;
			}

				/* determine size of object in bytes */

			size=0;
			sizemax=0;
			cur=fgetc(mydat);
			while (!feof(mydat))
			{
				switch (cur)
				{
				case '.':
					{
						if (fgetc(mydat)=='=')
						{
							fgetc(mydat); /* fetch # */
							fscanf(mydat,"%X\n",&inp2);
							size=inp2;
						}
						break;
					}
				case 'B':
					{
						fgetc(mydat); /* fetch # */
						fscanf(mydat,"%X\n",&inp2);
						size++;
						break;
					}
				default:
					;
				}
				if (size>sizemax) sizemax=size;

				cur=fgetc(mydat);
			}

			if (!sizemax) return 0;

				/* read */

			hf=(HexFile*)malloc(sizeof(HexFile));
			hf->data=(u8*)malloc(sizemax+4);
			hf->length=sizemax;

			memset(hf->data,0,hf->length);

			loc=0;
			fseek(mydat,SEEK_SET,0);

			fscanf(mydat,"%s\n",inp);
			cur=fgetc(mydat);
			while (!feof(mydat))
			{
				switch (cur)
				{
				case '.':
					{
						if (fgetc(mydat)=='=')
						{
							fgetc(mydat); /* fetch # */
							fscanf(mydat,"%X\n",&inp2);
							loc=inp2;
						}
						break;
					}
				case 'B':
					{
						fgetc(mydat); /* fetch # */
						fscanf(mydat,"%X\n",&inp2);
						hf->data[loc]=inp2;
						loc++;
						break;
					}

				default:					
					;
				}
				cur=fgetc(mydat);
			}

		}
		break;
	default:
		return 0;
	}



	return hf;
}


/* free memory */
void HexFileFree(HexFile *data)
{
	if (!data) return;
	free(data->data);
	free(data);
}
