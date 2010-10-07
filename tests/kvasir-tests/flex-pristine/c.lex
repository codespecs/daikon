%{

  /* A simple C99 lexer, derived from the one I wrote for Harmonia
     http://harmonia.cs.berkeley.edu/harmonia/projects/langs/c/index.html */

#define RETURN_TOKEN(tok) printf("Got " # tok " token\n");

%}

%x INCOMMENT

HEXDIGIT	    [0-9a-fA-F]
		    /* On 8-bit-byte machines,
		       only \\[0-3][0-7]{0,2} would be legal */
OCTESCAPE	    \\[0-7]{1,3}
HEXESCAPE	    \\x{HEXDIGIT}+
		    /* Universal character names are C99 only */
UNIV		    \\u{HEXDIGIT}{4}|\\U{HEXDIGIT}{8}
ESCAPE              (\\[ntvbrfa\n\\?'"])|{OCTESCAPE}|{HEXESCAPE}|{UNIV}
STRING              \"([^\\\n"]|{ESCAPE})*\"
CHARLIT             \'([^\\\n']|{ESCAPE})*\'
WSCHAR		    [ \n\t\f\v\r]
		    /* Backslash-newline can really occur anywhere,
		       but this handles the most common case */
WHITESPACE	    {WSCHAR}+|({WSCHAR}*\\\n)+{WSCHAR}*
IDENT		    [_a-zA-Z]([_a-zA-Z0-9]|{UNIV})*
DIGIT		    [0-9]
NUMBER		    {DIGIT}+
ZNUMBER		    ([1-9]{DIGIT}*)|0
INTEGER		    {ZNUMBER}|(0[0-7]+)|(0[xX][0-9a-fA-F]+)
EXPONENT	    [Ee][+-]?[0-9]+
FRACTIONAL	    ([0-9]+\.)|([0-9]*\.[0-9]+)
DECFLOAT	    {FRACTIONAL}{EXPONENT}?|[0-9]+{EXPONENT}
		    /* Hexidecimal floats are C99 only */
HEXFRACT	    {HEXDIGIT}*\.{HEXDIGIT}+|{HEXDIGIT}+\.
BINEXP		    [pP][+-]?[0-9]+
HEXFLOAT	    0[xX]({HEXFRACT}|{HEXDIGIT}+){BINEXP}
FLOAT		    {DECFLOAT}|{HEXFLOAT}
DIRECTIVE	    {WSCHAR}*#(.*\\\n)*.*
%%

{WHITESPACE}    { RETURN_TOKEN(WSPC); }

<INCOMMENT>"*/" { BEGIN(INITIAL); RETURN_TOKEN(COMMENT); }
<INCOMMENT>.|\n { yymore(); break; }
"/*"            { BEGIN(INCOMMENT); yymore(); break; }
"//".*		{ RETURN_TOKEN(COMMENT); /* C99 */ }
^{DIRECTIVE}\n	{ RETURN_TOKEN(PREPROC); }

"["		{ RETURN_TOKEN(LBRACK); }
"]"		{ RETURN_TOKEN(RBRACK); }
"<:"		{ RETURN_TOKEN(LBRACK); /* C99 */ }
":>"		{ RETURN_TOKEN(RBRACK); /* C99 */ }
"("		{ RETURN_TOKEN(LPAREN); }
")"		{ RETURN_TOKEN(RPAREN); }
"{"		{ RETURN_TOKEN(LBRACE); }
"}"		{ RETURN_TOKEN(RBRACE); }
"<%"		{ RETURN_TOKEN(LBRACE); /* C99 */ }
"%>"		{ RETURN_TOKEN(RBRACE); /* C99 */ }
"."		{ RETURN_TOKEN(DOT); }
"->"		{ RETURN_TOKEN(PTR); }

"++"		{ RETURN_TOKEN(INC); }
"--"		{ RETURN_TOKEN(DEC); }
"&"		{ RETURN_TOKEN(BAND); }
"*"		{ RETURN_TOKEN(TIMES); }
"+"		{ RETURN_TOKEN(PLUS); }
"-"		{ RETURN_TOKEN(MINUS); }
"~"		{ RETURN_TOKEN(BNOT); }
"!"		{ RETURN_TOKEN(NOT); }

"/"		{ RETURN_TOKEN(DIV); }
"%"		{ RETURN_TOKEN(MOD); }
"<<"		{ RETURN_TOKEN(LSHIFT); }
">>"		{ RETURN_TOKEN(RSHIFT); }
"<"		{ RETURN_TOKEN(LT); }
">"		{ RETURN_TOKEN(GT); }
"<="		{ RETURN_TOKEN(LE); }
">="		{ RETURN_TOKEN(GE); }
"=="		{ RETURN_TOKEN(EQ); }
"!="		{ RETURN_TOKEN(NE); }
"^"		{ RETURN_TOKEN(BXOR); }
"|"		{ RETURN_TOKEN(BOR); }
"&&"		{ RETURN_TOKEN(LAND); }
"||"		{ RETURN_TOKEN(LOR); }

"?"		{ RETURN_TOKEN(QUESTION); }
":"		{ RETURN_TOKEN(COLON); }
";"		{ RETURN_TOKEN(SEMI); }
"..."		{ RETURN_TOKEN(ELLIPSIS); }

"="		{ RETURN_TOKEN(ASSIGN); }
"*="		{ RETURN_TOKEN(MULASSIGN); }
"/="		{ RETURN_TOKEN(DIVASSIGN); }
"%="		{ RETURN_TOKEN(MODASSIGN); }
"+="		{ RETURN_TOKEN(ADDASSIGN); }
"-="		{ RETURN_TOKEN(SUBASSIGN); }
"<<="		{ RETURN_TOKEN(SHLASSIGN); }
">>="		{ RETURN_TOKEN(SHRASSIGN); }
"&="		{ RETURN_TOKEN(ANDASSIGN); }
"^="		{ RETURN_TOKEN(XORASSIGN); }
"|="		{ RETURN_TOKEN(ORASSIGN); }

","		{ RETURN_TOKEN(COMMA); }


{INTEGER}			   { RETURN_TOKEN(INT_CONST); }
{INTEGER}[Uu]			   { RETURN_TOKEN(INT_CONST_U); }
{INTEGER}[Ll]			   { RETURN_TOKEN(INT_CONST_L); }
{INTEGER}([Uu][Ll]|[Ll][Uu])	   { RETURN_TOKEN(INT_CONST_UL); }
{INTEGER}(ll|LL)		   { RETURN_TOKEN(INT_CONST_LL); /* C99 */ }
{INTEGER}([Uu](ll|LL)|(ll|LL)[Uu]) { RETURN_TOKEN(INT_CONST_ULL); /* C99 */ }

{FLOAT}[fF]			   { RETURN_TOKEN(FLOAT_CONST); }
{FLOAT}				   { RETURN_TOKEN(DOUBLE_CONST); }
{FLOAT}[lL]			   { RETURN_TOKEN(LONGDOUBLE_CONST); }

{STRING}	{ RETURN_TOKEN(STR_CONST); }
L{STRING}	{ RETURN_TOKEN(WIDE_STR_CONST); }
{CHARLIT}	{ RETURN_TOKEN(CHAR_CONST); }
L{CHARLIT}	{ RETURN_TOKEN(WIDE_CHAR_CONST); }

auto		{ RETURN_TOKEN(AUTO);}
break		{ RETURN_TOKEN(BREAK);}
case		{ RETURN_TOKEN(CASE);}
char		{ RETURN_TOKEN(CLANGCHAR);}
const		{ RETURN_TOKEN(CONST);}
continue	{ RETURN_TOKEN(CONTINUE);}
default		{ RETURN_TOKEN(DEFAULT);}
do		{ RETURN_TOKEN(DO);}
double		{ RETURN_TOKEN(DOUBLE);}
else            { RETURN_TOKEN(ELSE);}
enum		{ RETURN_TOKEN(ENUM);}
extern		{ RETURN_TOKEN(cEXTERN);}
float		{ RETURN_TOKEN(CLANGFLOAT);}
for		{ RETURN_TOKEN(FOR);}
goto		{ RETURN_TOKEN(GOTO);}
if		{ RETURN_TOKEN(IF);}
inline		{ RETURN_TOKEN(INLINE); /* C99 */ }
int		{ RETURN_TOKEN(CLANGINT);}
long		{ RETURN_TOKEN(CLANGLONG);}
register	{ RETURN_TOKEN(REGISTER);}
restrict	{ RETURN_TOKEN(RESTRICT); /* C99 */ }
return		{ RETURN_TOKEN(RETURN);}
short		{ RETURN_TOKEN(CLANGSHORT);}
signed		{ RETURN_TOKEN(SIGNED);}
sizeof		{ RETURN_TOKEN(SIZEOF); }
static		{ RETURN_TOKEN(STATIC);}
struct		{ RETURN_TOKEN(STRUCT);}
switch		{ RETURN_TOKEN(SWITCH);}
typedef		{ RETURN_TOKEN(TYPEDEF);}
union		{ RETURN_TOKEN(UNION);}
unsigned	{ RETURN_TOKEN(UNSIGNED);}
void		{ RETURN_TOKEN(VOID);}
volatile	{ RETURN_TOKEN(VOLATILE);}
while		{ RETURN_TOKEN(WHILE);}
_Bool		{ RETURN_TOKEN(_BOOL); /* C99 */ }
_Complex	{ RETURN_TOKEN(_COMPLEX); /* C99 */ }
_Imaginary	{ RETURN_TOKEN(_IMAGINARY); /* C99 */ }

{IDENT}		{ RETURN_TOKEN(IDSYM); }

	/* lexical errors */

<*>.|\n          printf("Skipping unexpected garbage\n");

%%
