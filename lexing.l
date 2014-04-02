%e  1019
%p  2807
%n  371
%k  284
%a  1213
%o  1117

/* Octal digits */
O   [0-7]
/* Decimal digits */
D   [0-9]
/* Positive decimal digits */
NZ  [1-9]
/* Letters and underscore */
L   [a-zA-Z_]
/* Alphanumeric */
A   [a-zA-Z_0-9]
/* Hexadecimal */
H   [a-fA-F0-9]
/* Hexadecimal prefix */
HP  (0[xX])
/* Exponential notation */
E   ([Ee][+-]?{D}+)
/* P-notation? */
P   ([Pp][+-]?{D}+)
/* Real-number suffixes */
FS  (f|F|l|L)
/* Integer suffixes */
IS  (((u|U)(l|L|ll|LL)?)|((l|L|ll|LL)(u|U)?))
/* Character prefixes */
CP  (u|U|L)
/* String prefixes */
SP  (u8|u|U|L)
/* Some kind of suffix */
ES  (\\(['"\?\\abfnrtv]|[0-7]{1,3}|x[a-fA-F0-9]+))
/* Whitespace */
WS  [ \t\v\n\f]

%{
#include <stdio.h>
#include "grammar.tab.h"

extern void yyerror(const char *);  /* prints grammar violation message */

%}

%%

{L}{A}*					{ return IDENTIFIER; }

{HP}{H}+{IS}?				{ return I_CONSTANT; }
{NZ}{D}*{IS}?				{ return I_CONSTANT; }
"0"{O}*{IS}?				{ return I_CONSTANT; }
{CP}?"'"([^'\\\n]|{ES})+"'"		{ return I_CONSTANT; }

{D}+{E}{FS}?				{ return F_CONSTANT; }
{D}*"."{D}+{E}?{FS}?			{ return F_CONSTANT; }
{D}+"."{E}?{FS}?			{ return F_CONSTANT; }
{HP}{H}+{P}{FS}?			{ return F_CONSTANT; }
{HP}{H}*"."{H}+{P}{FS}?			{ return F_CONSTANT; }
{HP}{H}+"."{P}{FS}?			{ return F_CONSTANT; }

({SP}?\"([^"\\\n]|{ES})*\"{WS}*)+	{ return STRING_LITERAL; }

"..."                                   { return ELLIPSIS; }
[\W|WS]+                                { return OPERATOR; }	


{WS}					{ /* whitespace separates tokens */ }
.					{ /* discard bad characters */ }

%%

int yywrap(void)        /* called at end of input */
{
    return 1;           /* terminate now */
}

