/* smal32.c
   language: C
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


/* Simple macro assembler and linkage editor with a 32 bit word.
   Started by D. W. Jones in the fall 1980 at the University of Iowa.
   rev 4/20/82 includes complete macro and conditional support.
   rev 5/5/82 includes errata and extensions.
   rev 2/5/84 yields 32-bit math, hex conversion.
   rev 12/1/88 to remove NS32000 specific code and run on Berkeley Pascal.
   rev 8/6/96 to translate to C with the aid of the PtoC translator.
   rev 7/27/97 includes command arguments.
   rev 6/11/98 minor bug fix in command line arguments.

  Originally coded in Hull r-mode Pascal for Prime computers:
    this uses 16 bit integer representations and the char type has
    only 64 elements.
  Works compatably in Hull v-mode Pascal for Prime computers:
    this uses 32 bit integer representations and the char type uses
    ASCII codes with the high bit always set to zero.
  Works in Prime Pascal (v-mode) when keyword 'packed' is deleted:
    this uses 32 bit integer representations and the char type uses
    ASCII codes with the high bit always set to one.
  Works in Berkeley Pascal, after date and time inquiry formats were
    changed from Hull v-mode to make this work.
  as modified by E. Wedel, requires 32-bit integers, but only
    where explicitly called for by "oversized" subrange types.
  as transliterated to C by D. Jones, limited to ASCII and 32 bit integers
*/

/* As currently configured, this program takes command line arguments
   to specify input, object and listing files.  The .o and .l suffixes
   are used for object and listing files.  The final suffix present on
   the input file name is removed before adding a .o or .l suffix;
   usually the input file suffix will be .a for assembly, this is not
   checked!  The -L option suppresses listing, the -D option enables
   symbol table dump.
*/

/* This assembler is written assuming that it will run on a
   machine with at least 32 bit two's complement integers.   */

#include <stdio.h>
#include <time.h>

/* boolean values */
#define true 1
#define false 0
#define boolean int

/* number of symbols allowed in symboltable */
#define symsize         500

/* opcodes and macro names allowed in optab */
#define opcodes         200

/* chars in stringpool; should be greater than the average symbol length
   times symsize, plus the average opcode or macro name length times
   opcodes, plus additional space for macro text storage, plus additional
   space for the macro call stack, at a minimum of 8 characters per macro
   nesting level, if no parameters are passed.
   note: poolsize must not be increased above 32767 unless appropriate
   changes are made to pushint and popint  */
#define poolsize        30000

/* offset of null symbol in pool */
#define relsym          1

/* offset for abs symbol (just below pool */
#define abssym          0

/* pool delimiter, an illegal character in programs, presently = control-A */
#define pooldel         '\001'

/* replacement for virgin occurrences of -pooldel- in input stream (ha!) */
#define pdelrep         '~'

/* number of files in input file stack */
#define maxgetl         4

/* number of macro parameters per call.
   note: increasing this above 9 will require changes to
   onepass.getline.getmac and to onepass.macdef.getbody; these currently use
   decimal digits to encode parameter numbers, and must be changed to use
   some other scheme */
#define parmlim         8

/* one more than length of allowed input line */
#define linelen         120

/* last column of listing used for object code info
   note that if listcol is changed to be less than 20, the error messages
   must all be shortened to keep the error markup under the right places
   on the input line.     */
#define listcol         24

/* object code items (bytes,words) listed on a line */
#define listcodes       50

/* field-size allocated to title and sub-title texts */
#define ttlbuflen       28

/* lines per page on the listing device */
#define linesper        56

/* characters per textual file name */
#define filelen         40

/* break character in file system path names.  used for implementation of USE
   in hierarchic file systems.  it is assumed in procedure insert that path
   names starting with pathbrk are absolute, while those starting with other
   characters are relative */
#define pathbrk         '/'

/* max 32-bit signed positive value 2^31-1*/
#define maxposint       2147483647L
#define maxposintdiv10  214748364L   /* maxposint DIV 10 (truncated) */


/* symboltable types */

/* reference to a location in the string pool */
typedef short poolref;

typedef struct value {
	poolref base;   /* symbol from which offset is relocated */
	long offset; /* offset from that symbol */
} value;

/* types of symbol-value associations */
enum {
	def, lab,   /* is it a label or defined value */
	intdef,     /* is the value to be exported */
	usedyet,    /* has value been used this pass */
	setyet      /* has value been set this pass */
};

/* symbol-value association */
typedef struct association {
	/* fields known only to lookup routine */
	poolref id;

	/* fields known only by table users */
	value val;
	long use; /* a set, one bit per type */
} association;



/* opcode types */
typedef enum {
	operr,   /* error */

	/* assembler directives */
	opb,     /* byte (8 bit) constant */
	oph,     /* half-word (16 bit) constant */
	opw,     /* word (32 bit) constant */
	opascii, /* string constant */
	opif,    /* conditional assembly */
	opelseif,
	opelse,
	opendif,
	opint,   /* internal symbol definition */
	opext,   /* external symbol declaration */
	opcomon, /* common declaration */
	opmac,   /* macro declaration */
	opendm,
	opuse,   /* use text from secondary source */
	oplist,  /* listing control directive */
	operror, /* directive to list this line as an error msg */
	opttl,   /* title directive */
	opsbttl, /* subtitle directive */
	opeject, /* page eject directive */
	opstart, /* starting address specification */
	opend,   /* end of file */

	/* macro call type */
	opmcall  /* macro call */

	/* add machine instruction types here */
} optypes;

/* all textual information stored by the assembler is in the string pool */
static char strpool[poolsize - relsym + 1];

/* two data structures share the string pool: permanent text grows up
   from relsym, the last used location is pointed to by          */
static poolref poolpos;

/* transient text grows down from poolsize, organized as a stack.  the
   format of each stack entry is documented in onepass.pushget           */

/* when the stringpool fills up, it is indicated by:     */
static boolean poolfull;

/* some key-words are stored in the pool but not in any symbol table;
   these are pointed to by the following (constant) variables            */
static poolref funcdf, funcfw, functy, funcab, funcrl, funcle;

/* the assembler symbol table for labels etc */
static association symtab[symsize];
typedef short symptr; /* index type into symtab */

/* flag indicating symbol table is full */
static boolean symfull;

/* the assembler symbol table for opcodes, directives, and macro names */
typedef struct _REC_optab {
	poolref id;
	optypes typ;
	long val, subtyp;
} _REC_optab;
typedef int opptr; /* index type for optab */
static _REC_optab optab[opcodes];


/* flag indicating opcode table is full */
static boolean opfull;

/* textual names of input and output files */
typedef char filename[filelen];
static filename outfile, objfile, dumpfile;
static filename infile[maxgetl];

/* shallow stack of source files */
static FILE *inp[maxgetl];

/* listing output file */
static FILE *o;

/* object code file */
static FILE *obj;

/* object code file */
static FILE *dmp;

/* input line buffer types */
typedef char linebuf[linelen];
typedef int inbufptr; /* index into a line buffer */

/* title and subtitle buffers for listing */
static linebuf titlebuf, sbttlbuf;
static inbufptr titlelen, sbttllen;

/* used to determine when to list title */
static long lineonpg;

/* current page in assembly listing */
static long pagenum;

/* global, set in -onepass-' routines */
static long errorcount;

/* date and time buffers for listing -- see getdatetime for details */
static char datestr[16];
static char timestr[9];


/* system date/time access routines */

static void getdatetime()
{
	char *datetime;
	long tloc = time(0);
	datetime = ctime(&tloc);
	
	/* fill datestr with "Mon Jan 16 1973", timestr with "12:00:00" */
	memcpy(&(datestr[0]), &(datetime[0]), 11);
	memcpy(&(datestr[11]), &(datetime[20]), 4);
	datestr[15] = 0;
	memcpy(&(timestr[0]), &(datetime[11]), 8);
	timestr[8] = 0;
}

typedef char string[16];


/* inside smal32, functions simulating 32 bit two's complement machine.
   All of these avoid overflow or underflow; for efficiency, these may be
   re-implemented as external assembly language routines      */

static long add(i, j)
long i, j;
{
	if (i > 0 && j > 0) {
		if (i <= maxposint - j)
			return (i + j);
		else
			return (i - maxposint + j - maxposint - 2);
	} else if (i < 0 && j < 0) {
		if (i >= -maxposint - j - 1)
			return (i + j);
		else
			return (i + maxposint + j + maxposint + 2);
	} else
		return (i + j);
}


static long neg(i)
long i;
{
	if (i == -maxposint - 1)
		return (-maxposint - 1);
	else
		return (-i);
}


static long sub(i, j)
long i, j;
{
	return (add(i, neg(j)));
}


static boolean ugt(i, j)
long i, j;
{
	/* unsigned greater than */
	if (i >= 0) {
		if (j >= 0)
			return (i > j);
		else
			return false;
	} else {  /* i<0 */
		if (j < 0)
			return (i > j);
		else
			return true;
	}
}


static long mod10(i)
long i;
{
	if (i >= 0) {
		return (i % 10);
	} else {
		return (((i + maxposint + 1) % 10 + 8) % 10);
	}
}


static long div10(i)
long i;
{
	if (i >= 0)
		return (i / 10);
	else
		return ((i + maxposint + 1) / 10 + maxposintdiv10 +
			(mod10(i + maxposint + 1) + 8) / 10);
}


static long inot(i)
long i;
{
	return (add(neg(i), -1L));
}


static long iand(i, j)
long i, j;
{
	return (i & j);
}


static long ior(i, j)
long i, j;
{
	return (i | j);
}


static long rightshift(i)
long i;
{
	return (i >> 1);
}


/* inside smal32, procedures for doing output format conversion */

static void writedec(f, v, l)
FILE *f;
int v, l;
{
	/* write v as an l digit decimal number on file f */
	char digits[10];
	long i;
	long sign = ' ';

	if (v < 0) {
		v = -v;
		sign = '-';
	}
	i = 0;
	do {
		digits[i] = (v % 10) + '0';
		v = v / 10;
		i++;
	} while (v > 0);
	while ((--l) > i) {
		putc(' ', f);
	}
	putc(sign, f);
	do {
		i--;
		putc(digits[i], f);
	} while (i > 0);
}

static void writehex(f, v, l)
FILE *f;
long v;
int l;
{
	/* write v as an l digit hex number on file f */
	char digits[8];
	char digit;
	long i;

	if (l > 8) {
		for (i = l - 1; i >= 8; i--)
			putc(' ', f);
		l = 8;
	}
	for (i = 0; i < l; i++) {
		if (v < 0) {
			v = (v + maxposint) + 1;
			digit = v & 15;
			v = v / 16 + 134217728L;
		} else {
			digit = v & 15;
			v /= 16;
		}
		if (digit < 10)
			digits[i] = digit + '0';
		else
			digits[i] = digit - 10 + 'A';
	}
	for (i = l - 1; i >= 0; i--)
		putc(digits[i], f);
}


static void writesym(f, pos)
FILE *f;
poolref pos;
{
	/* write the symbol from the symboltable on the indicated file */
	while (strpool[pos - relsym] != pooldel) {
		putc(strpool[pos - relsym], f);
		pos++;
	}
}


static void genval(siz, offset, base)
long siz, offset;
poolref base;
{
	/* generate siz byte value in object file based on indicated
	   symbol with the given offset from that symbol */
	if (base == abssym) {
		putc('#', obj);
		writehex(obj, offset, siz * 2);
	} else {
		if (offset == 0)
			putc(' ', obj);
		else {
			putc('#', obj);
			writehex(obj, offset, siz * 2);
			putc('+', obj);
		}
			putc('R', obj);
			writesym(obj, base);
	}
	putc('\n', obj);
}


/* inside smal32, procedures for symbol table management */

static void clearsym()
{
	/* initialize all entries in the symbol table to unused */
	symptr i;
	association *WITH;

	/* clearsym */
	symfull = false;
	for (i = 0; i < symsize; i++) {
		WITH = &symtab[i];
		WITH->id = 0;
		WITH->use = 0;
	}
}


static void clearuse()
{
	/* clear symbol table flags in preparation for one pass */
	symptr sym;
	association *WITH;

	/* clearuse */
	for (sym = 0; sym < symsize; sym++) {
		WITH = &symtab[sym];
		WITH->use &= ~((1L << ((long)usedyet))
			     | (1L << ((long)setyet)));
	}
}


static void objsufx()
{
	/* generate suffix to object file which defines all internal symbols */
	symptr sym;
	association *WITH;

	/* objsufx */
	for (sym = 0; sym < symsize; sym++) {
		WITH = &symtab[sym];
		if (((1L << ((long)intdef)) & WITH->use) != 0) {
			putc('R', obj);
			writesym(obj, WITH->id);
			putc('=', obj);
			genval( 4L, WITH->val.offset, WITH->val.base);
		}
	}
}


static poolref putpool(s)
char *s;
{
	poolref Result;
	int i;

	/* putpool */
	poolpos++;
	Result = poolpos;
	for (i = 0; i <= 7; i++) {
		if (s[i] != ' ') {
			strpool[poolpos - relsym] = s[i];
			poolpos++;
		}
	}
	strpool[poolpos - relsym] = pooldel;
	return Result;
}

static long hash(s, modulus)
char *s;
long modulus;
{
	/* it is critical that this hash function match that inside onepass */
	int i;
	long acc;

	/* hash */
	acc = 1;
	for (i = 0; i <= 7; i++) {
		if (s[i] != ' ') {
			acc = (acc * 5 + s[i]) % modulus + 1;
		}
	}
	return acc;
}

static void op(s, t, v, i)
char *s;
optypes t;
long *i;
{
	*i = hash(s, (long)opcodes);
	while (optab[(*i) - 1].id != 0) {
		*i = ((*i) % opcodes) + 1;
	}
	optab[*i - 1].id = putpool(s);
	optab[*i - 1].typ = t;
	optab[*i - 1].val = v;
	optab[*i - 1].subtyp = 0;
}


static void opinit()
{   /* opinit */
	/* initialize the opcode table and string pool (done only once) */
	long i;

	for (i = 1; i <= opcodes; i++)
		optab[i - 1].id = 0;
	poolfull = false;
	opfull = false;
	poolpos = relsym;
	strpool[poolpos - relsym] = pooldel;
	/* null symbol at start of pool is default relocation base */
	op("B       ", opb, 0L, &i);
	op("H       ", oph, 0L, &i);
	op("W       ", opw, 0L, &i);
	op("ASCII   ", opascii, 0L, &i);
	op("IF      ", opif, 0L, &i);
	op("ELSEIF  ", opelseif, 0L, &i);
	op("ELSE    ", opelse, 0L, &i);
	op("ENDIF   ", opendif, 0L, &i);
	op("INT     ", opint, 0L, &i);
	op("EXT     ", opext, 0L, &i);
	op("COMMON  ", opcomon, 0L, &i);
	op("MACRO   ", opmac, 0L, &i);
	op("ENDMAC  ", opendm, 0L, &i);
	op("USE     ", opuse, 0L, &i);
	op("LIST    ", oplist, 0L, &i);
	op("ERROR   ", operror, 0L, &i);
	op("TITLE   ", opttl, 0L, &i);
	op("SUBTITLE", opsbttl, 0L, &i);
	op("PAGE    ", opeject, 0L, &i);
	op("S       ", opstart, 0L, &i);
	op("END     ", opend, 0L, &i);
	/* note: when adding to this list, be sure to adjust the constant
	'opcodes' to reflect the additions; the local procedure
	'op' used above assumes that the opcode table will always
	have some free space.             */

	/* following establish unary function names.. */
	funcdf = putpool("DEF     ");
	funcfw = putpool("FWD     ");
	functy = putpool("TYP     ");
	funcab = putpool("ABS     ");
	funcrl = putpool("REL     ");
	funcle = putpool("LEN     ");
}


/* all kinds of error messages (no more than 31 of them!) */
typedef enum {
	minermsg,
	baddig, /* bad digit in number */
	baddir, /* bad assembly directive */
	badrad, /* bad radix */
	badrel, /* misuse of relocation in expression */
	bounds, /* value out of bounds */
	comexp, /* comma expected */
	fwdref, /* definition must precede use */
	idexp,  /* identifier expected */
	maxuse, /* too many source files */
	baduse, /* cannot open use file */
	miseif, /* missing endif */
	misemc, /* missing endmac */
	misquo, /* missing end quote */
	muldef, /* multiple symbol definition */
	mulstt, /* multiple start directives */
	notfit, /* text too long for line */
	notlab, /* expected label or directive */
	notval, /* expected a value, got something else */
	parexp, /* not a parenthesized list */
	parovf, /* too many macro parameters */
	phase,  /* phase error in label value between passes */
	quoexp, /* quoted string expected */
	unbal,  /* unbalanced parens */
	undef,  /* undefined symbol */
	unfunc, /* bad function */
	erropr, /* error operation */
	unproc, /* unprocessed data at end of line */
	maxermsg
} ermsg;

static char * errormsgs[] = { "---",
/*       12345678901234567890123456   all error messages are 26 chars long */
	"bad digit in number       ",
	"invalid directive         ",
	"bad radix                 ",
	"misuse of relocation      ",
	"value out of bounds       ",
	"comma expected            ",
	"name used before defined  ",
	"symbolic name expected    ",
	"too many use levels       ",
	"cannot open use file      ",
	"missing endif             ",
	"missing endmac            ",
	"missing end quote         ",
	"multiple label definition ",
	"multiple start directives ",
	"text too long for line    ",
	"not a label or directive  ",
	"bad value or expression   ",
	"not a parenthesized list  ",
	"too many macro parameters ",
	"label differed in pass 1  ",
	"quoted string expected    ",
	"unbalanced parentheses    ",
	"undefined symbol          ",
	"invalid function          ",
	"error message             ",
	"comment or eol expected   ",
	"+++"
};

/* types having to do with lexical analysis */
typedef enum {
	id,    /* identifier */
	num,   /* number (hex or decimal) */
	quote, /* quoted string */
	colon, /* : */
	dot,   /* . */
	comma, /* , */
	eq,    /* = */
	gt,    /* > */
	lt,    /* < */
	plus,  /* + */
	minus, /* - */
	notsym, /* \ */
	andsym, /* & */
	orsym, /* ! */
	bpar,  /* ( */
	epar,  /* ) */
	eol,   /* end of line and start of comment */
	junk   /* string of unclassified characters */
} lextypes;

typedef struct lexeme {
	/* start, end of lexeme in line */
	inbufptr pos;
	inbufptr lim;
	lextypes typ;
	union {
		/* value of number */
		long val;
	} UU;
} lexeme;


typedef int parm;

/* static variables used within onepass and affiliated code: */
static boolean firstpass;     /* true on the first pass only */
static boolean allowlist;     /* true on the final pass only */
static boolean permitlisting; /* is a listing file to be opened */
static boolean symtabdump;    /* is a symboltable dump to be created */

char charclass[256]; /* used to classify characters */
/* bits set in charclass to classify characters */
#define isvalid 1
#define ispunc 2
#define isquote 4
#define isdigit 8
#define isupper 16
#define islower 32
#define isalpha (isupper | islower)
#define isalphanum (isalpha | isdigit)

static void classchar( s, c )
char* s;
char c;
{
	/* in charclass, classify all chars in s as c */
	char ch;
	while ((ch = *s) != 0) {
		charclass[ch] |= c;
		s++;
	}
}

static value loc;    /* current assembly location counter */
static value objloc; /* current object code generation location counter */
static long lineno;  /* current line in assembly source */

static lexeme lex;  /* current lexeme */
static lexeme next; /* next lexeme for lookahead use */

/* variables associated with stack on transient end of stringpool */
static poolref poolsp; /* pool stack pointer */
static poolref oldsp;  /* pointer to previous frame in stack */
static int actcnt;    /* count of actual params in current frame */
static poolref actparm[parmlim]; /* pointers to ap's in frame */

/* variables controlling source of input */
static poolref gettext; /* loc in pool from which macro text comes */
static int getlevel; /* file from which non-macro text comes */

static linebuf line; /* line buffers and lexical analysis */
static int pos; /* current input line position */
static int ch;  /* line[pos] */
static inbufptr length; /* current input line length */

/* record of errors on the current line */
static linebuf erbuf; /* markup under error */
static inbufptr ermax; /* max used position in erbuf */
static long erset; /* set of messages to generate */

/* record of code generated by current line */
typedef struct _REC_codebuf {
	value val;
	int form;
} _REC_codebuf;
static _REC_codebuf codebuf[listcodes];
static int codelen;	/* the number of object code chunks not yet listed */
static value codeloc;   /* the location counter of the first object output */

/* listing control variables */
static boolean listing; /* set to (listlevel > 0) and (allowlist) */
static long listlevel;  /* dec on macro call, inc on return */

/* info about last expression */
static value expr;          /* value of expression */
static boolean exprdef;     /* is the value of the expression defined */
static boolean exprundef;   /* does expr. contain fwd ref(s)? */
static inbufptr exprpos, exprlim;    /* position of expression on line */

/* info about opcode decoded on current line */
static optypes optype;
static long opval, opsubt;
static inbufptr oppos, oplim;

/* routines to manage stack in stringpool,
   these routines assume that ord(maxch)>=32; if
   ord(maxch)>32, they may be recoded for greater
   efficiency without any effect on their users   */

static void pushchar(ch)
char ch;
{
	/* push one char onto stack in stringpool */
	if (poolsp > poolpos) {
		strpool[poolsp - relsym] = ch;
		poolsp--;
	} else
		poolfull = true;
}

static poolref pushtext(pos, lim)
inbufptr pos, lim;
{
	/* push the indicated text onto the stack from the line,
	   as a terminated string, return a reference to the first char */
	inbufptr i;

	/* pushtext */
	pushchar(pooldel);
	for (i = lim - 2; i >= pos - 1; i--)
		pushchar(line[i]);
	return (poolsp + 1);
}

static poolref pushitxt(i)
long i;
{
	/* push the indicated integer as a decimal text string, terminated
	   with a pooldel, and return a reference to the first char */
	pushchar(pooldel);
	if (i == 0) {
		pushchar('0');
		return (poolsp + 1);
	}
	while (i != 0) {
		pushchar((char)(mod10(i) + '0'));
		i = div10(i);
	}
}

static char popchar()
{
	/* pop one char from stack in stringpool */
	poolsp++;
	return (strpool[poolsp - relsym]);
}

static void pushint(i)
long i;
{
	/* push an integer onto stack as a sequence of chars */
	pushchar((char)(i & 31));
	pushchar((char)((i / 32) & 31));
	pushchar((char)(i / 1024));
}

static long popint()
{
	/* pop an integer from the stack as a sequence of chars */
	long i;

	i = popchar() * 1024;
	i += popchar() * 32;
	return (i + popchar());
}

static void pushget()
{
	/* push a macro expansion control block on the stack */
	int i;

	listlevel--;
	for (i = 0; i < actcnt; i++)
		pushint((long)actparm[i]);
	pushchar(actcnt);
	pushint((long)oldsp);
	pushint((long)gettext);
	pushchar(getlevel);
	oldsp = poolsp;
}

static void popget()
{
	/* pop a macro expansion control block from the stack */
	int i;

	if (poolfull) {  /* can't pop safely, so go all the way */
		listlevel = 1;
		gettext = 0;
		getlevel = 0;
		return;
	}
	/* can pop one level safely */
	listlevel++;
	poolsp = oldsp;
	getlevel = popchar();
	gettext = popint();
	oldsp = popint();
	actcnt = popchar();
	for (i = actcnt - 1; i >= 0; i--)
		actparm[i] = popint();
}


/* inside smal32.onepass, input/output routines */

static void errmsg(msg, pos, lim)
ermsg msg;
inbufptr pos, lim;
{
	/* record error and position of error in line for later listing */
	inbufptr i;

	erset |= 1L << ((long)msg);
	if (allowlist) {
		errorcount++;   /* for posterity's sake */
		listing = allowlist;   /* force error line to be listed */
		for (i = ermax; i <= pos - 2; i++)
			erbuf[i] = ' ';
		for (i = pos - 1; i <= lim - 2; i++)
			erbuf[i] = '=';
		if (lim > ermax + 1)
			ermax = lim - 1;
	}
}


/* routines used by getline */

static void makeend()
{
	/* put an end directive on the line as result of end file */
	/* makeend */
	line[0] = 'E';
	line[1] = 'N';
	line[2] = 'D';
	length = 3;
	line[3] = ';';   /* char beyond end must be initialized */
}

static void getmac()
{
	/* get one line of text from the string pool copy of macro body */
	long parmnum;
	poolref _parm;
	int i;
	char ch;

	i = 0;
	do {
		/* copy literal text from pool to line */
		ch = strpool[gettext - relsym];
		gettext++;
		while (ch != pooldel && i < linelen - 1) {
			i++;
			line[i - 1] = ch;
			ch = strpool[gettext - relsym];
			gettext++;
		}

		if (ch != pooldel) {  /* isn't space in the line */
			errmsg(notfit, 0, 0);
			while (strpool[gettext - relsym] != pooldel)
				gettext++;
			gettext++;
		}

		ch = strpool[gettext - relsym]; /* char after pooldel */
		gettext++;
		if (charclass[ch] & isdigit) {  /* macro param */
			parmnum = ch - '0';
			if (parmnum <= actcnt) {  /* param exists */
				_parm = actparm[parmnum - 1];
				if (_parm > 0) {  /* param is nonblank */
					ch = strpool[_parm - relsym];
					while (ch != pooldel && i < linelen - 1)
					{   /* loop copying text of parameter */
						i++;
						line[i - 1] = ch;
						_parm++;
						ch = strpool[_parm - relsym];
					}
					if (ch != pooldel)
						errmsg(notfit, 0, 0);
				}
			}
			ch = ' ';   /* force loop to continue */
		}
	} while ((ch != pooldel) && (ch != ','));
	if (ch == pooldel) {
		makeend();
	} else {
		length = i;
		line[i] = ';';   /* this char must be initialized */
	}
}

static void get(f)
FILE *f;
{
	/* read one line from appropriate input file */
	int i;
	int ch;

	i = 0;
	for (;;) {
		ch = getc(f);
		if ((ch == EOF) || (ch == '\n')) break;
		if (i >= (linelen - 1)) break;
		if (ch == pooldel)
			ch = pdelrep;
		if (ch == '\t')	{
			do {
				line[i] = ' ';
				i++;
			} while (((i & 7) != 0) && (i < (linelen - 1)));
		} else {
			line[i] = ch;
			i++;
		}
	}
	if (i >= (linelen - 1))
		errmsg(notfit, 0, 0);
	if ((ch == EOF) & (i == 0)) {
		makeend();
	} else {
		length = i;
		line[i] = ';';   /* this char must be initialized */
	}
}

static void getline()
{
	/*  read one line from the input file, initialize the
	    lexical analysis and listing control variables    */

	erset = 0;
	ermax = 0;
	if (gettext > 0) {
		getmac();
	} else {
		/* lines are only counted at the bottom level! */
		if (getlevel == 1) lineno++;
		get(inp[getlevel - 1]);
	}
	pos = 1;
	lex.typ = eol;
	lex.pos = 1;
	lex.lim = 1;
	next.typ = eol;
	next.pos = 1;
	next.lim = 1;
}

static void settitle(buf, len, pos, lim)
char *buf;
inbufptr *len, pos, lim;
{
	/* fill the indicated title buffer with the indicated part
		 of the input line       */
	inbufptr i;

	*len = 0;
	if (lim - pos > ttlbuflen - 3)
		lim = pos + ttlbuflen - 3;
	for (i = pos - 1; i < lim; i++) {
		(*len)++;
		buf[*len - 1] = line[i];
	}
}

static void newpage()
{
	/* force listing to the start of a new page */
	inbufptr i;

	if (!permitlisting) return;
	if (lineonpg <= 1) return;
	putc('\f', o);   /* formfeed character */
	/* note that instead of using a formfeed, this routine
	    could be written to simply output blank lines and
	    count them with lineonpg until the count was high
	    enough to push the listing to a new page      */
	lineonpg = 1;
	pagenum++;

	/* write title to listing file */
	fputs("SMAL32, rev  6/98.              ", o);
	for (i = 0; i < titlelen; i++)
		putc(titlebuf[i], o);
	for (i = titlelen; i <= ttlbuflen; i++)
		putc(' ', o);
	fputs(timestr, o);
	fputs("  Page ", o);
	writedec(o, pagenum, 1);
	putc('\n', o);   /* linefeed character */
	fputs("                                ", o);
	for (i = 0; i < sbttllen; i++)
		putc(sbttlbuf[i], o);
	for (i = sbttllen; i <= ttlbuflen; i++)
		putc(' ', o);
	fputs( datestr, o );
	putc('\n', o);   /* linefeed character */
	putc('\n', o);
}

static void listline()
{
	/*  list one line, including generated code.
	    if there are any errors, list the error messages  */
	inbufptr i;
	ermsg msg;
	int col;
	int nextcol = 0; /* column on output listing */
	int codepos; /* more permanent buffer ptr */

	if (!permitlisting) return;
	lineonpg++;
	if (lineonpg >= linesper) newpage();
	col = 1;
	codepos = codelen + 1;   /* in case we don't make any code.. */
	if (codelen > 0) {  /* list generated code */
		if (codeloc.base == abssym)
			putc(' ', o);
		else
			putc('+', o);
		writehex(o, codeloc.offset, 6L);
		putc(':', o);
		col = 9;
		i = 0;
		while (i < codelen) {  /* list each code item */
			_REC_codebuf *WITH;
			long width;
			i++;
			WITH = &codebuf[i - 1];
			width = WITH->form * 2;
				
			nextcol = col + width + 2;
			if (nextcol > (listcol + 1)) {
				codepos = i;
				i = codelen;
			} else {
				if (WITH->val.base != abssym)
					putc('+', o);
				else
					putc(' ', o);
				writehex(o, WITH->val.offset, width);
				putc(' ', o);
				col = nextcol;
			}
		}
	}
	for (; col <= listcol; col++)
		putc(' ', o);
	writedec(o, lineno, 6);
	/* text starts in column listcol + 1 */
	fputs("  ", o);
	for (i = 0; i < length; i++) putc(line[i], o);
	/* check for extended code listing */

	/* list ALL generated code */
	while (codepos <= codelen) {
		_REC_codebuf *WITH = &codebuf[codepos - 1];
		long width = WITH->form * 2;

		nextcol += (width + 2);
		if (nextcol > (listcol+1)) {
			putc('\n', o);
			lineonpg++;
			if (lineonpg > linesper) newpage();
			fputs("        ", o);
			nextcol = 9 + width + 2;
		}
		if (WITH->val.base != abssym)
			putc('+', o);
		else
			putc(' ', o);
		writehex(o, WITH->val.offset, width);
		putc(' ', o);
		codepos++;
	}
	codelen = 0;    /* reset record of listed code */
	/* write out all accumulated error messages */
	if (erset != 0) {   /* if */
		for (msg = minermsg; (long)msg <= (long)maxermsg; msg++) {
			if (((1L << ((long)msg)) & erset) != 0) {
				/* put message into listing */
				putc('\n', o);
				lineonpg++;
				if (lineonpg > linesper) newpage();
				fputs( errormsgs[ msg ], o );
				for (col = 26; col <= listcol + 7; col++)
					putc(' ', o);
				for (i = 0; i < ermax; i++)
					putc(erbuf[i], o);
				ermax = 0;

				/* put message to stderr also */
				writedec(stderr, lineno, 6);
				fputs("  ", stderr);
				fputs(errormsgs[ msg ], stderr);
				{
					int l = length;
					if ((l + 26) > 79) l = 79 - 26;
					for (i = 0; i < l; i++) {
						putc(line[i], stderr);
					}
				}
				putc('\n', stderr);
			}
		}
	}
	putc('\n', o);
}

static void putobj(form, offset, base)
int form;
long offset;
poolref base;
{
	/* store value (offset,base) in current loc.
	   (base=abssym is used for absolute values; typically, relocatable
	   values will come from loc.base and loc.offset or expr.base and
	   expr.offset.)
	   use format form (.25, .5 or 1 word), save information for listing */

	if (allowlist) {
		/* first assure that code gets loaded in right loc */
		if (objloc.offset != loc.offset ||
		    objloc.base != loc.base) {
			objloc = loc;
			fputs(".=", obj);
			genval( 4L, loc.offset, loc.base);
		}
		/* save appropriate listing data */
		if (codelen == 0)
			codeloc = loc;
		if (codelen < listcodes) {
			codelen++;
			codebuf[codelen - 1].val.offset = offset;
			codebuf[codelen - 1].val.base = base;
			codebuf[codelen - 1].form = form;
		}
		/* then generate correct object code */
		switch (form) {
			case 1: putc('B', obj); break;
			case 2: putc('H', obj); break;
			case 4: putc('W', obj); break;
		}
		genval( form, offset, base);
		objloc.offset = add(objloc.offset, form);
	}

	loc.offset = add(loc.offset, form);
}

static void putascii(pos, lim)
inbufptr pos, lim;
{
	/* generate object code for ascii string */
	inbufptr i;
	for (i = pos; i < lim; i++) {
		putobj(1L, line[i - 1], abssym);
	}
}



/* procedures used only by nextlex */

static long number(radix)
long radix;
{
	long acch,accl; /* accumulates the value */
	long digit; /* the value of one digit */

	/* assume initially that ch is a valid digit */
	acch = 0;
	accl = 0;
	do {
		if (charclass[ch] & isdigit)
			digit = ch - '0';
		else if (charclass[ch] & isupper)
			digit = (ch - 'A') + 10;
		else
			digit = (ch - 'a') + 10;

		if (digit >= radix) {
			acch = 65536L;
		} else {
			pos++;
			ch = line[pos - 1];
			accl = accl * radix + digit;
			acch = acch * radix + accl / 65536L;
			accl %= 65536L;
		}
	} while ((charclass[ch] & isalphanum) && (acch < 65536L));
	if (digit >= radix) {
		errmsg(baddig, pos, pos + 1);
		while (charclass[ch] & isalphanum) {
			pos++;
			ch = line[pos - 1];
		}
		return 0;
	} else if (acch >= 65536L) {
		while (charclass[ch] & isalphanum) {
			pos++;
			ch = line[pos - 1];
		}
		errmsg(bounds, next.pos, pos);
		return 0;
	} else if (acch >= 32768L)
		return ((acch - 32768L) * 65536L - maxposint + accl - 1);
	else
		return (acch * 65536L + accl);
}


/* inside smal32.onepass, lexical analysis routines */

static void nextlex()
{
	/* save the next lexeme information in the current lexeme
	   variable, then read a new next one from the input line */

	lex = next;
	while (line[pos - 1] == ' ') pos++;
	ch = line[pos - 1];
	next.pos = pos;
	if (ch == ';')
		next.typ = eol;
	else if (charclass[ch] & ispunc) {
		switch (ch) {   /* case */

		case ':': next.typ = colon; break;
		case '.': next.typ = dot; break;
		case ',': next.typ = comma; break;
		case '=': next.typ = eq; break;
		case '>': next.typ = gt; break;
		case '<': next.typ = lt; break;
		case '+': next.typ = plus; break;
		case '-': next.typ = minus; break;
		case '\\': next.typ = notsym; break;
		case '&': next.typ = andsym; break;
		case '!': next.typ = orsym; break;
		case '(': next.typ = bpar; break;
		case ')': next.typ = epar; break;

		}
		pos++;
	} else if (charclass[ch] & isdigit) {
		next.typ = num;
		next.UU.val = number(10L);
		if (ch == '#') {
			if (next.UU.val > 36 || next.UU.val < 2) {
				next.UU.val = 36;
				errmsg(badrad, next.pos, pos);
			}
			pos++;
			ch = line[pos - 1];
			if (charclass[ch] & isalphanum)
				next.UU.val = number(next.UU.val);
			else
				errmsg(baddig, next.pos, pos);
		}
	} else if (charclass[ch] & isalpha) {
		next.typ = id;
		do {
			pos++;
			ch = line[pos - 1];
		} while (charclass[ch] & isalphanum);
	} else if (ch == '#') {
		pos++;
		ch = line[pos - 1];
		if (charclass[ch] & isalphanum) {
			next.typ = num;
			next.UU.val = number(16L);
		} else   /* if */
			next.typ = junk;
	} else if (charclass[ch] & isquote) {
		char mark = ch;
		next.typ = quote;
		do {
			pos++;
		} while (line[pos - 1] != mark && pos <= length);
		if (pos <= length)
			pos++;
		else
			errmsg(misquo, next.pos, next.pos + 1);
	} else {
		do {
			pos++;
			ch = line[pos - 1];
		} while ((charclass[ch] & isvalid) == 0);
			next.typ = junk;
	}
	next.lim = pos;

	/* invalid lexeme */
}

static void startup()
{
	/* setup for processing one line of input */
	getline(); /* read input line */
	nextlex(); /* read first lexeme */
	nextlex(); /* read second lexeme (allow lookahead) */

	/* start parsing by looking for valid start of line */
	while ( (lex.typ != id) &&
		(lex.typ != eol) &&
		(lex.typ != dot)
	) {
		errmsg(notlab, lex.pos, lex.lim);
		nextlex();
	}
}

/* inside smal32.onepass, string pool and symbol table management */

static void putch(ch)
char ch;
{
	/* put one char into permanent end of stringpool */
	if (poolsp > poolpos) {  /* there is room in pool */
		poolpos++;
		strpool[poolpos - relsym] = ch;
	} else  /* there isn't room */
		poolfull = true;
}

static poolref putpool_(pos, lim)
inbufptr pos, lim;
{
	/* put the string between pos and lim-1 on the current line into
		 the string pool, returning it's index in the pool.  the string
		 delimiter is appended to the string in the pool.  it is assumed
		 that the string will fit (the caller must guarantee this)    */
	poolref Result;
	inbufptr i;

	poolpos++;
	Result = poolpos;
	for (i = pos - 1; i <= lim - 2; i++) {
		strpool[poolpos - relsym] = line[i];
		poolpos++;
	}
	strpool[poolpos - relsym] = pooldel;
	return Result;
}

static boolean poolfit(pos, lim)
inbufptr pos, lim;
{
	/* check to see if text between pos and lim will fit in stringpool */
	/* poolfit */
	return (poolsp - poolpos > lim - pos);
}

static boolean poolcmp(poolpos, pos, lim)
poolref poolpos;
inbufptr pos, lim;
{
	/* compare the string starting at poolpos in the stringpool with
		 that between pos and lim on the current line, return true if
		 they are the same; this relies on the fact that the string
		 delimiter in the stringpool will never occur in the line   */
	while (strpool[poolpos - relsym] == line[pos - 1]) {
		poolpos++;
		pos++;
	}
	return (strpool[poolpos - relsym] == pooldel && pos == lim);
}

static long hash_(pos, lim, modulus)
inbufptr pos, lim;
long modulus;
{
	/* compute hash of lexeme between pos and lim,
		 return a value between 1 and modulus (inclusive)
		 this hash function must match that in "opinit"  */
	long acc;
	inbufptr p;

	/* hash */
	acc = 1;
	for (p = pos - 1; p <= lim - 2; p++) {
		acc = (acc * 5 + line[p]) % modulus + 1;
	}
	return acc;
}

static symptr lookup(pos, lim)
inbufptr pos, lim;
{
	/* find the symbol between pos and lim on the current line in
	   the symbol table, return the index into the table where it
	   was found or inserted; if it could not be inserted, return
	   zero      */
	symptr Result, s, olds;
	association *WITH;

	/* lookup */
	s = hash_(pos, lim, (long)symsize);
	olds = s;
	Result = 0;   /* default return value */
	do {
		WITH = &symtab[s - 1];
		if (WITH->id != 0) {   /* with */
			if (poolcmp(WITH->id, pos, lim)) {
	Result = s;
	s = olds;   /* terminate loop */
			} else {
	if (s < symsize)
		s++;
	else
		s = 1;
	if (s == olds)
		symfull = true;
			}
		} else {  /* found unused table entry */
			if (poolfit(pos, lim)) {
				/* put the symbol in the pool and table */
				WITH->id = putpool_(pos, lim);
				Result = s;
			} else  /* no room in pool for sym */
				poolfull = true;
			s = olds;   /* terminate loop */
		}
	} while (s != olds);
	return Result;
}

static void symdump()
{
	/* dump entire contents of symbol table */
        symptr i;
        association *WITH;

        for (i = 0; i < symsize; i++) {
                WITH = &symtab[i];
		if (WITH->id != 0) {  /* have nonblank entry */
                        writesym(dmp, WITH->id);
                        putc('=', dmp);
                        putc('\t', dmp); /* sort field delimiter */
			/* could use following */
			/* genval(..., 4L, WITH->val.offset, WITH->val.base);*/

			/* instead, we do it locally to get sortable format */
			if (WITH->val.base != abssym) {
				if (WITH->val.base = relsym) {
					fputs("REL(0)", dmp);
				} else {
					writesym(dmp, WITH->val.base);
				}
				putc('+', dmp);
			}
                        putc('\t', dmp);

			putc('#', dmp);
			writehex(dmp, WITH->val.offset, 8);
        		putc('\n', dmp);
		}
        }
}

static opptr oplookup(pos, lim)
inbufptr pos, lim;
{
	/* find the symbol between pos and lim on the current line in
		 the opcode table, return the index into the table where it
		 was found or should be put; return 0 if it isn't found and
		 the table is full         */
	opptr Result, s, olds;
	_REC_optab *WITH;

	s = hash_(pos, lim, (long)opcodes);
	olds = s;
	Result = 0;   /* default return value */
	do {
		WITH = &optab[s - 1];
		if (WITH->id != 0)   /* with */
		{  /* have nonblank entry */
			if (poolcmp(WITH->id, pos, lim)) {  /* found it */
	Result = s;
	s = olds;   /* terminate loop */
			} else {
	if (s < opcodes)
		s++;
	else
		s = 1;
			}
		} else {  /* found vacancy */
			Result = s;
			s = olds;   /* terminate loop */
		}
	} while (s != olds);
	return Result;
}


/* inside smal32.onepass, utility parsing procedures */

static void getcomma()
{
	/* skip the comma, complain if there isn't one */
	if (lex.typ == comma)
		nextlex();
	else
		errmsg(comexp, lex.pos, lex.lim);
}

static void skipbal()
{
	/* skip to maching end paren when given begin paren */
	long nest;
	lexeme par;

	/* assert lex.typ = bpar */
	nest = 1;
	par = lex;
	do {
		nextlex();
		if (lex.typ == bpar)
			nest++;
		else if (lex.typ == epar)
			nest--;
	} while (nest >= 1 && lex.typ != eol);
	if (lex.typ == eol)
		errmsg(unbal, par.pos, par.lim);
}

static void expression();

static void value_()
{
	/* parse values of an expression of the form
	    <value> ::= <ident> ! <num> ! . ! ( <expression> )
		      ! <string> ! <identifier> ( <argument> )
	   return the value of the value in expr */
	symptr symbol;
	lexeme op, par;
	long i;

	association *WITH;

	exprpos = lex.pos;
	exprlim = lex.lim;
	exprundef = false;
	if (lex.typ == num) {  /* got a  + 1number */
		expr.offset = lex.UU.val;
		expr.base = abssym;
		exprdef = true;   /* read over number */
		nextlex();
	} else if (lex.typ == quote) {
		expr.base = 0;
		expr.offset = 0;
		exprdef = true;
		i = lex.lim - lex.pos;
		if (i > 6)
			errmsg(bounds, lex.pos, lex.lim);
		else if (i > 2) {
			long FORLIM = lex.lim - lex.pos - 3;
			for (i = 0; i <= FORLIM; i++)
				expr.offset = expr.offset * 256 +
					line[lex.pos + i];
		}
		nextlex();
	} else if ((lex.typ == id) && (next.typ == bpar)) {

		/*  do a named (unary) function  */
		op = lex;
		nextlex();   /* skip operator name */
		par = lex;
		nextlex();   /* skip opening paren */
		expr.base = abssym;
		expr.offset = 1;   /* default to ambiguous */
		exprdef = true;   /* default to defined */

		if (poolcmp(funcdf, op.pos, op.lim)) {
			if (lex.typ == id)   /* skip operand */
				symbol = lookup(lex.pos, lex.lim);
			else {
				symbol = 0;
				errmsg(idexp, lex.pos, lex.lim);
			}
			nextlex();
			if (symbol != 0)
				expr.offset =
			-(((1L<<((long)setyet)) & symtab[symbol - 1].use) != 0);
		} else if (poolcmp(funcfw, op.pos, op.lim)) {
			if (lex.typ == id)   /* skip operand */
				symbol = lookup(lex.pos, lex.lim);
			else {
				symbol = 0;
				errmsg(idexp, lex.pos, lex.lim);
			}
			nextlex();
			if (symbol != 0)
				expr.offset =
			-(((1L<<((long)usedyet)) & symtab[symbol - 1].use)!=0 &&
			  ((1L<<((long)setyet)) & symtab[symbol - 1].use)==0);
		} else if (poolcmp(functy, op.pos, op.lim)) {
			expression();
			expr.offset = expr.base;
			expr.base = abssym;
		} else if (poolcmp(funcab, op.pos, op.lim)) {
			expression();
			expr.base = abssym;
		} else if (poolcmp(funcrl, op.pos, op.lim)) {
			expression();
			if (expr.base == abssym) {
				expr.base = relsym;
			} else {
				errmsg(badrel, exprpos, exprlim);
			}
		} else if (poolcmp(funcle, op.pos, op.lim)) {
			if (lex.typ == epar)
				expr.offset = 0;
			else {
				expr.offset = lex.pos;
				if (lex.typ == bpar)
					skipbal();
				while ((next.typ != eol)&&(next.typ != epar)) {
					nextlex();
					if (lex.typ == bpar)
					  skipbal();
				}
				expr.offset = lex.lim - expr.offset;
				nextlex();
			}
		} else {
			errmsg(unfunc, op.pos, op.lim);
			while ((lex.typ != epar) && (lex.typ != eol)) nextlex();
		}
		if (lex.typ == epar) {
			exprpos = op.pos;
			exprlim = lex.lim;
			/* skip end paren */
			nextlex();
		} else {
			errmsg(unbal, par.pos, par.lim);
		}

	} else if (lex.typ == id) {
		/*  ah, just an ordinary identifier..  */
		symbol = lookup(lex.pos, lex.lim);
		if (symbol > 0) {   /* read over identifier */
			WITH = &symtab[symbol - 1];
			WITH->use |= 1L << ((long)usedyet);
			if (((1L << ((long)def)) & WITH->use) != 0 ||
			   ((1L << ((long)lab)) & WITH->use) != 0)
			{
				expr = WITH->val;
				exprdef = true;
				exprundef = (((1L << ((long)setyet)) & WITH->use) == 0);
			} else {
				errmsg(undef, lex.pos, lex.lim);
				expr.offset = 0;
				expr.base = abssym;
				exprdef = false;
			}
		} else {   /* if */
			expr.offset = 0;   /* no err on full table */
			expr.base = abssym;
			exprdef = true;   /* pretend it's defined */
		}
		nextlex();

	} else if (lex.typ == dot) {
		expr = loc;
		exprdef = true;   /* read over dot */
		nextlex();

	} else if (lex.typ == bpar) {
		par = lex;
		nextlex();
		expression();
		if (lex.typ == epar) {
			exprpos = par.pos;
			exprlim = lex.lim;
			nextlex();
		} else
			errmsg(unbal, par.pos, par.lim);

	} else {
		/* got something else */
		errmsg(notval, lex.pos, lex.lim);
		expr.offset = 0;
		expr.base = abssym;
		exprdef = false;
		if (((1L << ((long)lex.typ)) & ((1L << ((long)epar)) |
		 (1L << ((long)eol)) | (1L << ((long)comma)))) == 0)
		/* read over whatever it is */
			nextlex();
	}

	exprundef = (!exprdef || exprundef);
}

static void term()
{
	/* parse terms of an expression of the form
			    <term> ::= [ <unary> ] <value>
		 return the value of the term in expr */
	/* unary operator */
	lexeme op;

	if (((1L << ((long)lex.typ)) & ((1L << ((long)plus)) |
	 (1L << ((long)minus)) | (1L << ((long)notsym)))) == 0)
	{  /* unary */
		value_();
		return;
	}
	op = lex;   /* read over unary operator */
	nextlex();   /* get value to be modified */
	value_();
	exprpos = op.pos;
	switch (op.typ) {   /* case */

	case plus:
		/* blank case */
		break;

	case minus:
		if (expr.base == abssym)
			expr.offset = neg(expr.offset);
		else
			errmsg(badrel, op.pos, op.lim);
		break;

	case notsym:
		if (expr.base == abssym)
			expr.offset = inot(expr.offset);
		else
			errmsg(badrel, op.pos, op.lim);
		break;
	}

	/* no unary operator */
}



/* inside smal32.onepass, procedures to parse expressions */

static void expression()
{
	/* parse expressions of the form
	    <expression> ::= <term> ! <expression> <binop> <term>
	   return the value of the expression in expr */
	value acc;        /* the accumulator */
	boolean accdef;   /* is the accumulator defined */
	boolean accundef; /* does acc. have fwd ref(s)? */
	lexeme op;        /* what operator was found */
	inbufptr expos;   /* position of start of expression */

	/* get leading term */
	term();
	acc = expr;
	accdef = exprdef;
	accundef = exprundef;
	expos = exprpos;   /* save pos for error handlers */
	while ( (lex.typ == plus) ||
		(lex.typ == minus) ||
		(lex.typ == gt) ||
		(lex.typ == lt) ||
		(lex.typ == eq) ||
		(lex.typ == andsym) ||
		(lex.typ == orsym)
	) {   /* while loop processing terms */
		op = lex;   /* skip over operator */
		nextlex();
		if ( (op.typ == lex.typ) &&
		     ( (op.typ == gt) ||
		       (op.typ == lt)
		) ) { /* skip over remainder of operator */
		      /* it is a shift operator: >> or << */
			nextlex();
			if (op.typ == gt)
				op.typ = epar;
			else
				op.typ = bpar;
		}
		/* get following term */
		term();
		if (!exprdef) {
			accdef = false;
			continue;
		}
		switch (op.typ) {   /* case */

		case plus:   /* plus */
			acc.offset = add(acc.offset, expr.offset);
			if (acc.base == abssym)
				acc.base = expr.base;
			else if (expr.base != abssym) {
				errmsg(badrel, op.pos, op.lim);
				acc.base = abssym;
			}
			break;

		case minus:   /* minus */
			acc.offset = sub(acc.offset, expr.offset);
			if (acc.base == expr.base)
				acc.base = abssym;
			else if (expr.base != abssym) {
				errmsg(badrel, op.pos, op.lim);
				acc.base = abssym;
			}
			break;

		case gt:
		case lt:
		case eq:
			if (acc.base == expr.base) {
				switch (op.typ) {   /* case */

				case gt:
					acc.offset =-(acc.offset > expr.offset);
					break;

				case lt:
					acc.offset =-(acc.offset < expr.offset);
					break;

				case eq:
					acc.offset =-(acc.offset ==expr.offset);
					break;
				}
				acc.base = abssym;
			} else {   /* gt,lt,eq */
				errmsg(badrel, op.pos, op.lim);
				acc.base = abssym;
				acc.offset = 1;   /* neither true nor false */
			}
			break;

		case andsym:
		case orsym:
			if (acc.base == abssym && expr.base == abssym) {
				switch (op.typ) {   /* case */

				case andsym:
					acc.offset=iand(acc.offset,expr.offset);
					break;

				case orsym:
					acc.offset =ior(acc.offset,expr.offset);
					break;
				}
			} else   /* andsym,orsym */
				errmsg(badrel, op.pos, op.lim);
			break;

		case bpar:
		case epar:
			if (acc.base == abssym && expr.base == abssym) {
				if (expr.offset > 32)
					expr.offset = 32;
				while (expr.offset > 0) {
				    switch (op.typ) {

				    case bpar:
					acc.offset =add(acc.offset, acc.offset);
					break;

				    case epar:
					acc.offset = rightshift(acc.offset);
					break;
				    }
				    expr.offset--;
				}
			} else {
				errmsg(badrel, op.pos, op.lim);
			}  /* shift operators */
			break;
		}
	}
	expr = acc;
	exprdef = accdef;
	exprundef = (exprundef || accundef || !exprdef);
	exprpos = expos;
}

static void expresbal()
{
	/* evaluate expressions, assuring balanced parens */
	/* expresbal */
	expression();
	while (lex.typ == epar) {
		errmsg(unbal, lex.pos, lex.lim);
		nextlex();
	}
}

static boundval( v, min, max )
long *v;
long min, max;
{
	/* check to see v is between min and max */
	if ((*v < min) || (*v > max)) {
		errmsg( bounds, exprpos, exprlim );
		*v = min;
	}
}

static boolean predicate()
{   /* evaluate expression */
	/* evaluate predicates for if and elseif directives */
	boolean Result;

	expresbal();
	Result = false;   /* default */
	if (expr.base == abssym) {
		if (expr.offset == -1)
			return true;
	} else
		errmsg(badrel, exprpos, exprlim);
	return Result;
}


/* inside smal32.onepass, processing of key syntactic elements */

static void labeldef()
{
	/* parse label definition; define label and handle multiples */
	symptr symbol;

	/* labeldef */
	/* assume that (lex.typ = id) and (next.typ = colon) */
	symbol = lookup(lex.pos, lex.lim);
	if (symbol > 0) {  /* the symbol is in the table */
		association *WITH = &symtab[symbol - 1];
		if (((1L << ((long)setyet)) & WITH->use) != 0) {
			errmsg(muldef, lex.pos, lex.lim);
		} else if (((1L << ((long)lab)) & WITH->use) != 0) {
			if (WITH->val.offset != loc.offset ||
			    WITH->val.base != loc.base) {
				errmsg(phase, lex.pos, lex.lim);
			}
		} else {
			WITH->val = loc;
		}
		WITH->use |= (1L << ((long)lab)) | (1L << ((long)setyet));
	}
	nextlex();   /* read over id */
	nextlex();   /* read over colon */
}

static void definition()
{
	/* parse and process definition of form <id> = <expression> */
	symptr symbol;
	association *WITH;

	/* definition */
	symbol = lookup(lex.pos, lex.lim);
	if (((1L << ((long)lab)) & symtab[symbol - 1].use) != 0)
	/* read over id */
		errmsg(muldef, lex.pos, lex.lim);
	nextlex();   /* read over eq */
	nextlex();
	expresbal();
	if (symbol <= 0)
		return;
	WITH = &symtab[symbol - 1];
	WITH->use |= 1L << ((long)setyet);
	if (exprdef) {
		WITH->use |= 1L << ((long)def);
		WITH->val = expr;
	}
}

static void origin()
{   /* skip dot */
	/* parse and process definitions of the form . = <expression> */
	/* origin */
	nextlex();   /* skip eq */
	nextlex();
	expresbal();
	loc = expr;
}

static void opcode()
{

	/*  parse opcode field, including detection of data width
		specifying suffices.  Resultant info is returned via globals
		optype, opval, opsubt, opsufi, opsufi2, opsuff and opsufcc. */
	opptr i;

	oppos = lex.pos;
	oplim = lex.lim;
	optype = operr;

	i = oplookup(oppos, oplim);
	if (i != 0) {
		if (optab[i - 1].id != 0) {
			_REC_optab *WITH = &optab[i - 1];
			optype = WITH->typ;
			opval = WITH->val;
			opsubt = WITH->subtyp;
		}
	}

	nextlex();
}


static void getop()
{
	/* skip and ignore labels on a line, return the opcode globally
		 suppress any error messages encountered in parsing the line */
	startup();

	while (lex.typ == id && next.typ == colon) {   /* skip id */
		nextlex();   /* skip colon */
		nextlex();
	}
	if (lex.typ == id) {
		if (next.typ == eq)
			optype = operr;
		else
			opcode();
	} else
		optype = operr;
	erset = 0;
}


/* inside smal32.onepass, processing of external symbol linkages */

static void internl()
{
	/* parse and process internal symbol definitions */
	symptr symbol;

	if (lex.typ == id) {   /* read over internal symbol name */
		symbol = lookup(lex.pos, lex.lim);
		if (symbol > 0) {
			association *WITH = &symtab[symbol - 1];
			WITH->use |= 1L << ((long)intdef);
			if ((WITH->use & ((1L << ((long)def)) | (1L << ((long)lab)))) == 0)
			/* if with */
			  errmsg(undef, lex.pos, lex.lim);
		}
	} else
		errmsg(idexp, lex.pos, lex.lim);
	nextlex();
}

static void makeext(symbol)
symptr symbol;
{
	/* make or verify that the current symbol is external */
	/* used only from externl and comdef */

	association *WITH = &symtab[symbol - 1];
	if (((1L << ((long)setyet)) & WITH->use) != 0)
		errmsg(muldef, lex.pos, lex.lim);
	else if (((1L << ((long)usedyet)) & WITH->use) != 0)
		errmsg(fwdref, lex.pos, lex.lim);
	else if (((1L << ((long)lab)) & WITH->use) != 0) {
		if (WITH->val.base != WITH->id || WITH->val.offset != 0)
			errmsg(muldef, lex.pos, lex.lim);
	} else {
		WITH->val.base = WITH->id;
		WITH->val.offset = 0;
	}
	WITH->use |= (1L << ((long)lab)) | (1L << ((long)setyet));   /* with */

	/* symbol previously unused */
}

static void externl()
{
	/* parse and process external symbol declarations */
	symptr symbol;

	/* externl */
	if (lex.typ == id) {   /* read over external symbol name */
		symbol = lookup(lex.pos, lex.lim);
		if (symbol > 0)
			makeext(symbol);
	} else
		errmsg(idexp, lex.pos, lex.lim);
	nextlex();
}

static void comdef()
{
	/* parse and process common declarations */
	symptr symbol;

	if (lex.typ != id) {
		errmsg(idexp, lex.pos, lex.lim);
		return;
	}
	symbol = lookup(lex.pos, lex.lim);
	if (symbol > 0) {
		association *WITH = &symtab[symbol - 1];
		makeext(symbol);   /* read over common name */
		nextlex();
		getcomma();   /* get common size or maximum location */
		expresbal();
		if (expr.base != abssym && expr.base != WITH->id) {
			errmsg(badrel, exprpos, exprlim);
			return;
		}
		expr.offset += expr.offset & 1; /* halfword align size */
		expr.offset += expr.offset & 2; /* word align size */
		if (!allowlist)
			return;
		fputs("IF\\DEF(S", obj);
		writesym(obj, WITH->id);
		fputs(")\n S", obj);
		writesym(obj, WITH->id);
		fputs("=#", obj);
		writehex(obj, expr.offset, 4L);
		fputs("\nENDIF\nIF\\DEF(R", obj);
		writesym(obj, WITH->id);
		fputs(")\n R", obj);
		writesym(obj, WITH->id);
		fputs("=C\n C=C+S", obj);
		writesym(obj, WITH->id);
		fputs("\n CT=.\n .=C\n .=CT\nENDIF\n",obj);
		return;
	}
	nextlex();
	getcomma();   /* skip over size */
	expresbal();

	/* symbol table full */
	/* skip over name */
	/* common name missing */
}


/* inside smal32.onepass, processing of text insertions */

static void insert_()
{
	/* process use directive (push one input file on stack) */
	int i;
	inbufptr pos;
	int startpos;

	if (lex.typ != quote) {
		errmsg(quoexp, lex.pos, lex.lim);
		nextlex();
		return;
	}
	if (getlevel >= maxgetl) {
		errmsg(maxuse, lex.pos, lex.lim);
		nextlex();
		return;
	}

	/* put together the file name */
	pos = lex.pos + 1;
	startpos = 1;
	if (line[pos - 1] != pathbrk) {  /* relative path */
		memcpy(infile[getlevel], infile[getlevel - 1],
			sizeof(filename));
		for (i = 1; i < filelen; i++) {
			if (infile[getlevel][i - 1] == pathbrk)
			startpos = i + 1;
		}
	}
	for (i = startpos - 1; i < filelen; i++) {
		if (pos < lex.lim - 1) {
			infile[getlevel][i] = line[pos - 1];
			pos++;
		} else
			infile[getlevel][i] = 0;
	}

	/* try to open the file */
	if (inp[getlevel] != NULL) {
		inp[getlevel] = freopen(infile[getlevel], "r", inp[getlevel]);
	} else {
		inp[getlevel] = fopen(infile[getlevel], "r");
	}

	if (inp[getlevel] == NULL) {  /* see if it worked */
		errmsg(baduse, lex.pos, lex.lim);
	} else {
		/* if it worked, save current source */
		pushget();
		getlevel++;
		gettext = 0; /* set source to file, not macro */
		actcnt = 0;  /* set to no macro parameters */
	}
	nextlex();
}

/* static variables for macdef: */
static poolref parmtab[parmlim];
static int parms;
static poolref _oldsp;

static void getpar()
{   /* getpar */
	/* parse one formal parameter of form:
			  <param> := <id>  !  ( <id> )  !  = <id>
		 corresponding to by name, by list of names, and by value */
	/* the parameter identifier */
	lexeme _parm;
	/* the position of the begin paren */
	lexeme par;
	/* the type of the formal parameter */
	char typ = ' ';

	if (parms < parmlim) {   /* read over parameter */
		if (lex.typ == id) {  /* name parameter */
			_parm = lex;
			typ = 'a';
		} else if (lex.typ == eq) {
			nextlex();
			if (lex.typ == id) {
				typ = '=';
				_parm = lex;
			} else {
				_parm.typ = eol;
				errmsg(idexp, lex.pos, lex.lim);
			}
		} else if (lex.typ == bpar) {
			par = lex;   /* hold onto position for errors */
			/* skip over paren */
			nextlex();
			if (lex.typ == id) {
				typ = '(';
				_parm = lex;
				nextlex(); /* skip over identifier */
				if (lex.typ != epar)
					errmsg(unbal, par.pos, par.lim);
			} else {
				_parm.typ = eol;
				errmsg(idexp, lex.pos, lex.lim);
			}
		} else {
			_parm.typ = eol;
			errmsg(idexp, lex.pos, lex.lim);
		}
		if (_parm.typ == id) {  /* have a good param */
			parms++;
			if (firstpass) {
				parmtab[parms-1] =
					pushtext(_parm.pos, _parm.lim);
				putch(typ);
			}
		}
	} else {
		errmsg(parovf, lex.pos, lex.lim);
	}
	nextlex();
}


static parm lookup_(pos, lim)
inbufptr pos, lim;
{
	/* lookup candidate formal parameter name */
	parm Result, i;

	/* lookup */
	Result = 0;
	i = 0;
	while (i < parms) {
		i++;
		if (poolcmp(parmtab[i - 1], pos, lim)) {
			Result = i;
			i = parms;
		}
	}
	return Result;
}

static void getbody()
{
	/* parse macro body of form: <body> ::= <linesequence> endmac */
	/* counter to find right endmac when nested */
	long nest;
	inbufptr pos;
	/* progress pointers for parameter scan */
	inbufptr lim;
	/* identity of current parameter */
	parm parmnum;

	/* getbody */
	nest = 1;
	do {
		if (listing) listline();
		getop();
		if (optype == opmac)
			nest++;
		else if (optype == opendm)
			nest--;
		else if (optype == opend)
			nest = 0;
		if (nest > 0 && firstpass) {  /* save text */
			pos = 1;
			while (pos <= length) {
				while (!(charclass[line[pos - 1]] & isalpha)
				       && (pos <= length)
				) {
					/* copy non identifier text */
					if (line[pos - 1] == '\'') {
						/* squeeze out quote marks */
						pos++;
						/* assert line[length+1]=';' */
						while (line[pos - 1] == '\'') {
						    putch('\'');
						    pos++;
						}
					} else {
						/* pass through simple text */
						putch(line[pos - 1]);
						pos++;
					}
				}
				if (pos > length) break;
				/* found an identifier */
				lim = pos;
				while (charclass[line[lim - 1]] & isalphanum)
					lim++;
				/* length of identifier known */
				parmnum = lookup_(pos, lim);
				if (parmnum > 0) {
					/* identifier is a macro formal parm */
					putch(pooldel);
					putch(parmnum + '0');
				} else {
					/* identifier is just text */
					for (pos--; pos <= lim - 2; pos++)
						putch(line[pos]);
				}
				pos = lim;
			}
			putch(pooldel);
			putch(',');
		}
	} while (nest >= 1);
	poolsp = _oldsp; /* pop formal parameter table from stack */
	if (optype == opend) {  /* found end of file, not endm */
		errmsg(misemc, 0, 0);
		popget();
	}
	if (firstpass) {
		putch(pooldel);   /* two pooldel's in a row end a macro */
		putch(pooldel);
	}
}

static void macdef()
{
	/* process macro definitions of the form:
	   macro <name> [ <param> [ , <param> ]* ] <body>          */
	opptr m;

	parms = 0;
	_oldsp = poolsp;   /* mark stack top, allowing temporary use */
	if (lex.typ == id) {  /* have macro name */
		m = oplookup(lex.pos, lex.lim);
		if (m <= 0)   /* skip over macro name */
		{  /* no room in table */
			opfull = true;
		} else if (firstpass) {
			if (optab[m - 1].id == 0) {  /* first definition */
			    if (poolfit(lex.pos, lex.lim)) {
				optab[m - 1].id = putpool_(lex.pos, lex.lim);
				optab[m - 1].typ = opmcall;
				optab[m - 1].val = poolpos + 1;
			    }
			} else {  /* redefinition */
				optab[m - 1].typ = opmcall;
				optab[m - 1].val = poolpos + 1;
			}
		}
		nextlex();
		if (lex.typ != eol) {
			getpar();
			while (lex.typ != eol) {
				getcomma();
				getpar();
			}
		}
		if (firstpass)   /* mark end of parmtypes */
			putch(pooldel);

	} else {  /* missing macro name */
		errmsg(idexp, lex.pos, lex.lim);   /* skip over junk */
		nextlex();
	}
	if (lex.typ != eol && erset == 0)
		errmsg(unproc, lex.pos, lex.lim);
	getbody();
}

/* static variables for maccall: */
static poolref _poolpos;

static void getpar_()
{
	/* parse a parameter of the form:
	    <param> ::= [ <lexeme> ]*
		     !  <expression>
		     !  ( [ <lexeme> ]* ) [ : <expr> [ : <expr> ] ]  */
	char typ;     /* indicates expected parameter type */
	inbufptr pos; /* location of parameter */
	inbufptr lim; /* position info for begin paren */
	lexeme par;

	typ = strpool[_poolpos - relsym];
	_poolpos++;
	actcnt++;
	if (((1L << ((long)lex.typ)) &
	    ((1L << ((long)comma)) | (1L << ((long)eol)))) != 0)
	{  /* parameter missing */
		actparm[actcnt - 1] = 0;
		return;
	}
	if (typ == 'a') {
		pos = lex.pos;
		lim = lex.lim;
		while (((1L << ((long)lex.typ)) &
		       ((1L << ((long)eol)) | (1L << ((long)comma)))) == 0) {
			if (lex.typ == bpar)
				skipbal();
			else if (lex.typ == epar)
				errmsg(unbal, lex.pos, lex.lim);
			lim = lex.lim;
			nextlex();
		}
		actparm[actcnt - 1] = pushtext(pos, lim);
		return;
	}
	if (typ == '=') {
		expresbal();
		/* recall that text is pushed backwards */
		pushitxt(expr.offset); /* end with decimal offset */
		if (expr.base != abssym) {
			/* add in correction for relocatable symbols */
			pushchar('+');
			if (expr.base == relsym) {
				/* actual param is REL(0)+offset */
				pushchar(')');
				pushchar('0');
				pushchar('(');
				pushchar('L');
				pushchar('E');
				pushchar('R');
				actparm[actcnt - 1] = poolsp + 1;
			} else {
				/* actual param is external+offset */
				short int i = expr.base - relsym;
				short int j;
				for (j = i; strpool[j] != pooldel; j++);
				for (j--; j >= i; j--) {
					pushchar(strpool[j]);
				}
			}
		}
		actparm[actcnt - 1] = poolsp + 1;
		return;
	}
	if (typ != '(')
		return;
	if (lex.typ != bpar) {
		errmsg(parexp, lex.pos, lex.lim);
		nextlex();
		return;
	}
	par = lex;   /* skip paren at list head */
	nextlex();
	pos = lex.pos;
	lim = pos;   /* default for empty string */
	while (((1L << ((long)lex.typ)) &
	       ((1L << ((long)eol)) | (1L << ((long)epar)))) == 0) {
		if (lex.typ == bpar)
			skipbal();
		lim = lex.lim;
		nextlex();
	}
	if (lex.typ == eol)
		errmsg(unbal, par.pos, par.lim);
	else
		nextlex();
	if (lex.typ == colon) {  /* substring */
		nextlex();
		if (lex.typ != colon) {   /* get start */
			expresbal();
			if (expr.base != abssym)
				errmsg(badrel, exprpos, exprlim);
			if (expr.offset > 1) {
				if (expr.offset > lim - pos)
					pos = lim;
				else
					pos += expr.offset - 1;
			}
		}
		if (lex.typ == colon) {
			nextlex();   /* get length */
			expresbal();
			if (expr.base != abssym)
				errmsg(badrel, exprpos, exprlim);
			if (expr.offset < lim - pos) {
				if (expr.offset < 1)
					lim = pos;
				else
					lim = pos + expr.offset;
			}
		}
	}
	if (pos >= lim)
		actparm[actcnt - 1] = 0;
	else
		actparm[actcnt - 1] = pushtext(pos, lim);
}

static void maccall(poolpos_)
poolref poolpos_;
{
	/* call macro who's text is stored at poolpos in string pool;
		 form of call is:
		 <name> [ <param> [ , <param> ]* ]      */

	/* save previous macro expansion status block */
	_poolpos = poolpos_;
	if (poolfull) {
		/* stop bad calls */
	} else {
		pushget();
		actcnt = 0;
		while (strpool[_poolpos-relsym] != pooldel && lex.typ != eol) {
			getpar_();
			if (lex.typ != eol) getcomma();
		}
		if (lex.typ != eol)
			errmsg(parovf, lex.pos, lex.lim);
		while (strpool[_poolpos - relsym] != pooldel)
			_poolpos++;
		gettext = _poolpos + 1;   /* set to read macro text from pool */
	}
}


/* inside smal32.onepass, processing of conditional directives */

static void findend()
{
	/* skip over lines until a line with endif or end opcode found */
	long nest;

	nest = 0;
	do {   /* read and check following lines */
		/* first list previous line */
		if (listing) listline();

		getop();
		if (optype == opif)
			nest++;
		else if (optype == opendif)
			nest--;
	} while (optype != opend && nest >= 0);
	if (optype == opend) {
		errmsg(miseif, 0, 0);
		popget();
	}
}

static void findelse()
{
	/* skip lines until one with an else, elseif <true>, or end found */
	do {   /* read over then parts (allowing multiple elseif's) */
		if (lex.typ != eol && erset == 0)
			errmsg(unproc, lex.pos, lex.lim);
		do {
			/* list line to be skipped */
			if (listing) listline();
			getop();
			while (optype == opif) {
				findend();
				/* list end */
				if (listing) listline();
				if (optype == opend)
					optype = opendif;
					/* don't complain twice */
				else
					getop();
			}
		} while (((1L << ((long)optype)) &
			 ((1L << ((long)opend)) | (1L << ((long)opendif)) |
			  (1L << ((long)opelse)) | (1L << ((long)opelseif)))) == 0);
		if (optype == opelseif) {
			if (predicate())
				optype = opelse;
		}
	} while (((1L << ((long)optype)) & ((1L << ((long)opend)) |
		  (1L << ((long)opendif)) | (1L << ((long)opelse)))) == 0);
	if (optype == opend) {
		errmsg(miseif, 0, 0);
		popget();
	}
}


/* inside smal32, main assembly procedure for one pass through source */

static void onepass()
{
	/*  perform one assembly pass.  produce output if listing=true  */
	/* max val of loc.offset when loc.base = relsym */
	long maxrel;
	/* starting address, if specified */
	value startloc;
	/* detects multiple start dirs */
	boolean seenstart;

	poolsp = poolsize;
	gettext = 0;
	getlevel = 0;
	oldsp = 0;
	actcnt = 0;   /* put a dummy level on the stack to allow clean end */
	pushget();
	getlevel = 1;   /* setup for reading at normal source level */
	listlevel = 1;   /* setup so it will list only main level source */
	lineonpg = linesper;   /* force leading page-eject */
	codelen = 0;    /* no code yet accumulated for listing */
	sbttllen = 0;	/* no subtitle on first page-eject */
	lineno = 0;
	errorcount = 0;
	loc.offset = 0;
	loc.base = relsym;
	maxrel = 0;
	objloc = loc;
	seenstart = false;   /* setup symbol table for pass */
	clearuse();

	while (getlevel >= 1) {   /* process one line per iteration */
		listing = (listlevel > 0 && allowlist);
		startup(); /* setup for processing a line */

		while (lex.typ == id && next.typ == colon) {
			labeldef();
		}

		/* now know that if lex.typ = id, then next.typ <> colon */
		if (lex.typ == id) {
			if (next.typ == eq)  /* process definitions */
				definition();
			else {   /* if */
				opcode();
				switch (optype) { /* opcode class */

	case operr:
		errmsg(baddir, oppos, oplim);
		break;

	case opb:  /* 8 bit byte */
		expresbal();
		boundval(&expr.offset, -128L, 255L);
		putobj(1L, expr.offset, expr.base);
		while (lex.typ != eol) {
			getcomma();
			expresbal();
			boundval(&expr.offset, -128L, 255L);
			putobj(1L, expr.offset, expr.base);
		}
		break;

	case oph:  /* 16 bit half-word */
		expresbal();
		boundval(&expr.offset, -32768L, 65535L);
		putobj(2L, expr.offset, expr.base);
		while (lex.typ != eol) {
			getcomma();
			expresbal();
			boundval(&expr.offset, -32768L, 65535L);
			putobj(2L, expr.offset, expr.base);
		}
		break;

	case opw:  /* 32 bit word */
		expresbal();
		putobj(4L, expr.offset, expr.base);
		while (lex.typ != eol) {
			getcomma();
			expresbal();
			putobj(4L, expr.offset, expr.base);
		}
		break;

	case opascii: /* ascii string */
		if (lex.typ != quote) {
			errmsg(quoexp, lex.pos, lex.lim);
		} else {
			putascii(lex.pos + 1, lex.lim - 1);
		}
		nextlex();
		while (lex.typ != eol) {
			getcomma();
			if (lex.typ == quote) {
				putascii(lex.pos + 1, lex.lim - 1);
				nextlex();
			} else {
				expresbal();
				boundval(&expr.offset, -128L, 255L);
				putobj(1L, expr.offset, expr.base);
			}
		}
		break;
		
	case opif:
		if (!predicate())
			findelse();
		break;

	case opelse:
	case opelseif:
		findend();
		break;

	case opendif:
		/* blank case */
		break;

	case opint:  /* internal definition */
		internl();
		while (lex.typ == comma) {
			nextlex();
			internl();
		}
		break;

	case opext:  /* external definition */
		externl();
		while (lex.typ == comma) {
			nextlex();
			externl();
		}
		break;

	case opcomon:   /* common definition */
		comdef();
		break;

	case opmac:   /* process macro definition */
		macdef();
		break;

	case opendm:
		errmsg(baddir, oppos, oplim);
		break;

	case opuse:   /* insert text from alt file */
		insert_();
		break;

	case oplist:  /* control listing */
		expresbal();
		if (expr.base != abssym)
			errmsg(badrel, exprpos, exprlim);
		else {
			listlevel += expr.offset;
			if (expr.offset < 0 && erset == 0)
			  listing = (listlevel > 0 && allowlist);
		}
		break;

	case operror: /* force listing as error msg */
		lex.typ = eol;  /* ignore rest of this line */
		errmsg(erropr, 0, 0);
		break;

	case opttl:  /* title directive */
		settitle(titlebuf, &titlelen, lex.pos, length);
		lex.typ = eol;
		break;

	case opsbttl:
		if (firstpass)
			listline();
		if (allowlist)
			settitle(sbttlbuf, &sbttllen, lex.pos, length);
		if (listing)
			newpage();
		lex.typ = eol;
		break;

	case opeject:
		if (listing)
			lineonpg = linesper - 1;
		break;

	case opstart:
		if (!seenstart) {  /* set start addr */
			seenstart = true;
			expresbal();
			startloc = expr;
		} else  /* multiple start addr's */
			errmsg(mulstt, oppos, oplim);
		break;

	case opend:
		popget();
		break;

	case opmcall:   /* call the indicated macro */
		maccall((int)opval);
		break;
	}
			}
		} else if (lex.typ == dot && next.typ == eq)
			origin();
		else if (lex.typ != eol) {
			errmsg(baddir, lex.pos, lex.lim);
			nextlex(); /* skip over junk to avoid extra errors */
		}
		if (lex.typ != eol && erset == 0)
			errmsg(unproc, lex.pos, lex.lim);
		if (listing)
			listline();
		if (loc.base == relsym) {
			if (ugt(loc.offset, maxrel))
				maxrel = loc.offset;
		}
	}
	if (!allowlist)
		return;
	/* make sure relocatable size is aligned on 32 bit boundary */
	maxrel += maxrel & 1;  /* halfword align */
	maxrel += maxrel & 2;  /* word align */
	/* make sure object code ends at maxrel */
	if (loc.base != relsym || loc.offset != maxrel) {
		fputs(".=", obj);
		genval( 4L, maxrel, relsym);
	}
	/* put starting address out */
	if (seenstart) {
		putc('S', obj);
		genval( 4L, startloc.offset, startloc.base);
	}
	if (!permitlisting) return;
	/* note error count in listing */
	if (errorcount == 0) {
		fputs("                    no errors\n", o);
		return;
	} else {
		fputs("    ", o);
		if (errorcount == 1)
			fputs(" 1 error", o);
		else {
			fputs("    ", o);
			writedec(o, errorcount, 1);
			fputs(" errors", o);
		}
		fputs(" in this assembly\n", o);
	}
}


/* inside smal32, procedures to do file setup and takedown */

static void getfiles(argc, argv)
int argc;
char *argv[];
{
	/*  get file name from command line, store it in global "infile"
	    suffix it with ".l" for "outfile", ".o" for "objfile"
	    and ".d" for "dumpfile" */
	int dot, i;

	infile[0][0] = 0;

	permitlisting = true;
	symtabdump = false;
	for (i = 1; i < argc; i++) { /* for each argument */
		if (argv[i][0] == '-') {
			if (argv[i][1] == 'L') {
				permitlisting = false;
			} else if (argv[i][1] == 'D') {
				symtabdump = true;
			} else {
				fputs("** invalid command line argument **\n",
					stderr);
				exit(-1); /* error */
			}
		} else {
			int j;
			if (infile[0][0] != 0) {
				fputs("** multiple input files **\n", stderr);
				exit(-1); /* error */
			}
			for (j = 0; argv[i][j] != 0; j++) {
				if (j < filelen-3) infile[0][j] = argv[i][j];
			}
			while (j < filelen) {
				infile[0][j] = 0;
				j++;
			}
			infile[0][filelen-1] = 0;
		}
	}
	if (infile[0][0] == 0) {
		fputs("** no input file specified **\n", stderr);
		exit(-1); /* error */
	}

	dot = 0;
	titlelen = 0;
	for (i = 0; i < filelen; i++) {
		char ch = infile[0][i];
		titlebuf[i] = ch;
		outfile[i] = ch;
		objfile[i] = ch;
		dumpfile[i] = ch;
		if (ch == '.') {
			dot = i;
		}
		if (i < (ttlbuflen-2)) {
			titlebuf[i] = ch;
			if (ch != 0) titlelen = i;
		}
		if ((ch == 0) && (dot == 0)) { /* no suffix on file name */
			dot = i;
		}
	}

	outfile[dot] = '.';
	outfile[dot + 1] = 'l';
	outfile[dot + 2] = 0;
	objfile[dot] = '.';
	objfile[dot + 1] = 'o';
	objfile[dot + 2] = 0;
	dumpfile[dot] = '.';
	dumpfile[dot + 1] = 'd';
	dumpfile[dot + 2] = 0;
}


/* inside smal32 */

main(argc, argv)
int argc;
char *argv[];
{
	{ /* setup to classify characters */
		int i;
		for (i = 0; i < 256; i++) charclass[i] = 0;
	}
	classchar( "abcdefghijklmnopqrstuvwxyz", islower | isvalid );
	classchar( "ABCDEFGHIJKLMNOPQRSTUVWXYZ", isupper | isvalid );
	classchar( "0123456789", isdigit | isvalid );
	classchar( ":.,=><+-\\@&!()[]", ispunc | isvalid );
	classchar( "'\"", isquote | isvalid );
	classchar( " ;#", isvalid );

	obj = NULL;
	dmp = NULL;
	o = NULL;
	{
		int i;
		for (i = 0; i < maxgetl; i++) {
			inp[i] = NULL;
		}
	}
	getfiles(argc, argv);

	inp[0] = fopen(infile[0], "r");
	if (inp[0] == NULL) {
		fputs("** cannot open input file **\n", stderr);
		exit(-1); /* abnormal termination */
	}
	if (permitlisting) {
		o = fopen(outfile, "w");
		if (o == NULL) {
			fputs("** cannot open object file **\n", stderr);
			exit(-1); /* abnormal termination */
		}
	}
	obj = fopen(objfile, "w");
	if (obj == NULL) {
		fputs("** cannot open listing file **\n", stderr);
		exit(-1); /* abnormal termination */
	}
	clearsym();
	opinit();
	lineonpg = 1;
	pagenum = 0;
	getdatetime();

	firstpass = true;
	allowlist = false;
	onepass();

	if (poolfull) {
		fputs("** string pool overflowed on pass 1 **\n", stderr);
		exit(-1); /* abnormal termination */
	}
	inp[0] = freopen(infile[0], "r", inp[0]);
	if (inp[0] == NULL) {
		fputs("** cannot reopen input file **\n", stderr);
		exit(-1); /* abnormal termination */
	}
	fputs("R=.\n", obj);   /* firstpass */
	
	firstpass = false;
	allowlist = true;
	onepass();
	objsufx();

	putchar('\n');
	fputs("  ", stderr);
	if (errorcount == 0) {
		fputs(" no errors\n", stderr);
	} else if (errorcount == 1) {
		fputs(" 1 error\n", stderr);
	} else {
		writedec( stderr, errorcount, 1 );
		fputs(" errors\n", stderr);
	}
	if (symfull)
		fputs("** symbol table overflowed **\n", stderr);
	if (poolfull)
		fputs("** string pool overflowed **\n", stderr);
	if (opfull)
		fputs("** macro name table full **\n", stderr);

	if (symtabdump) {
		dmp = fopen(dumpfile, "w");
		if (dmp == NULL) {
			fputs("** cannot open dump file **\n", stderr);
		} else {
			symdump();
		}
	}

	/* now close all files */
	{
		int i;
		for (i = 0; i < maxgetl; i++) {
			if (inp[i] != NULL)
				fclose(inp[i]);
		}
	}

	if (o != NULL)
		fclose(o);
	if (obj != NULL)
		fclose(obj);
	if (dmp != NULL)
		fclose(dmp);
	exit(0); /* normal termination */
}
