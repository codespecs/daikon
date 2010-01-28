/*
 * File: den.c
 *
 * (c) P. Kleiweg 1997 - 2005
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2,
 * or (at your option) any later version.
 *
 */

#define denVERSION "1.27"

#define __NO_MATH_INLINES

#ifdef __WIN32__
#define my_PATH_SEP '\\'
#else
#define my_PATH_SEP '/'
#endif

#ifdef __MSDOS__
#  ifndef __COMPACT__
#    error Memory model COMPACT required
#  endif  /* __COMPACT__  */
#  include <dir.h>
#  include <io.h>
#else  /* Unix  */
#  include <unistd.h>
#endif  /* Unix */
#include <ctype.h>
#include <errno.h>
#include <float.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef enum { CLS, LBL } NODETYPE;
typedef enum { RECT, TRI, ARC } LINKTYPE;
typedef enum { FALSE = 0, TRUE = 1} BOOL;

typedef struct {
    int
        index,
        group [2];
    float
        value;
    char
        *text;
    NODETYPE
        node [2];
    union {
        int
            cluster;
        char
            *label;
    } n [2];
} CLUSTER;


CLUSTER
    *cl = NULL;

#define BUFSIZE 1024

BOOL
    example = FALSE,
    eXtended = FALSE,
    patterns = FALSE,
    symbols = FALSE,
    numbers = FALSE,
    colorlink = FALSE,
    colorlabel = FALSE,
    labels = TRUE,
    evenodd = FALSE,
    oe = FALSE,
    ruler = TRUE,
    first = TRUE,
    mindefined = FALSE,
    use_rainbow = FALSE,
    use_bright = FALSE,
    use_usercolours = FALSE;

LINKTYPE
    linktype = RECT;

#define maxCOLORS 19

float
    colors [maxCOLORS][3] = {
        { 0.0, 0.0, 1.0 },
        { 0.0, 1.0, 0.0 },
        { 0.0, 1.0, 1.0 },
        { 1.0, 0.0, 0.0 },
        { 1.0, 0.0, 1.0 },
        { 1.0, 1.0, 0.0 },
        { 0.0, 0.0, 0.5 },
        { 0.0, 0.5, 0.0 },
        { 0.0, 0.5, 0.5 },
        { 0.5, 0.0, 0.0 },
        { 0.5, 0.0, 0.5 },
        { 0.5, 0.5, 0.0 },
        { 0.3, 0.3, 0.7 },
        { 0.3, 0.7, 0.3 },
        { 0.3, 0.7, 0.7 },
        { 0.7, 0.3, 0.3 },
        { 0.7, 0.3, 0.7 },
        { 0.7, 0.7, 0.3 },
        { 0.3, 0.3, 0.3 } };

char
    *pat [maxCOLORS] = {
	"4 4 {< 7f7fdfdf         >}",
	"2 2 {< 7fbf             >}",
	"6 6 {< bfa3bff717f7     >}",
	"4 4 {< 7fffdfff         >}",
	"4 4 {< 1fff4fff         >}",
	"3 4 {< 7f7fbfbf         >}",
	"3 3 {< 7fbfdf           >}",
	"4 4 {< 9fff0fff         >}",
	"8 6 {< 017d55d710ff     >}",
	"4 6 {< bf5fbfef57ef     >}",
	"6 4 {< ff17ffa3         >}",
	"2 4 {< 5fffbfff         >}",
	"8 8 {< 515f51ff15f515ff >}",
	"3 4 {< 7f7f7f9f         >}",
	"6 6 {< 27bf93fb4bef     >}",
	"4 8 {< bf5f5fbfef5f5fef >}",
	"5 4 {< 9f6f6fff         >}",
	"6 6 {< af27ff27afff     >}",
	"8 8 {< df99fdf7dfccfd7f >}" 
    },
    *sym [] = {
	"    Symsize -2 div dup rmoveto\n"
	"    Symsize 0 rlineto\n"
	"    0 Symsize rlineto\n"
	"    Symsize neg 0 rlineto\n"
	"    closepath\n"
	"    gsave 1 setgray fill grestore\n"
	"    stroke\n",

	"    currentpoint\n"
	"    Symsize 2 div 0 rmoveto\n"
	"    Symsize 2 div 0 360 arc\n"
	"    closepath\n"
	"    gsave 1 setgray fill grestore\n"
	"    stroke\n",

	"    Symsize -2 div dup rmoveto\n"
	"    Symsize 0 rlineto\n"
	"    Symsize -2 div Symsize rlineto\n"
	"    closepath\n"
	"    gsave 1 setgray fill grestore\n"
	"    stroke\n",

	"    currentpoint\n"
	"    Symsize -2 div 0 rmoveto\n"
	"    Symsize 0 rlineto\n"
	"    Symsize 2 div sub moveto\n"
	"    0 Symsize rlineto\n"
	"    stroke\n",

	"    Symsize -2 div dup rmoveto\n"
	"    Symsize dup rlineto\n"
	"    0 Symsize neg rmoveto\n"
	"    Symsize neg Symsize rlineto\n"
	"    stroke\n",

	"    Symsize 2 div\n"
	"    dup neg 0 exch rmoveto\n"
	"    dup dup rlineto\n"
	"    dup dup neg exch rlineto\n"
	"    neg dup rlineto\n"
	"    closepath\n"
	"    gsave 1 setgray fill grestore\n"
	"    stroke\n",

	"    Symsize 2 div dup rmoveto\n"
	"    Symsize neg 0 rlineto\n"
	"    Symsize 2 div Symsize neg rlineto\n"
	"    closepath\n"
	"    gsave 1 setgray fill grestore\n"
	"    stroke\n",

	"    currentpoint\n"
	"    sym0\n"
	"    moveto\n"
	"    sym4\n",

	"    currentpoint\n"
	"    sym3\n"
	"    moveto\n"
	"    sym4\n",

	"    currentpoint\n"
	"    sym5\n"
	"    moveto\n"
	"    sym3\n",

	"    currentpoint\n"
	"    sym1\n"
	"    moveto\n"
	"    sym3\n",

	"    gsave\n"
	"        1 setgray\n"
	"        currentpoint\n"
	"        0 Symsize -2 div rmoveto\n"
	"        Symsize 2 div Symsize rlineto\n"
	"        Symsize neg 0 rlineto\n"
	"        closepath\n"
	"        fill\n"
	"        moveto\n"
	"        0 Symsize 2 div rmoveto\n"
	"        Symsize -2 div Symsize neg rlineto\n"
	"        Symsize 0 rlineto\n"
	"        closepath\n"
	"        fill\n"
	"    grestore\n"
	"    0 Symsize -2 div rmoveto\n"
	"    Symsize 2 div Symsize rlineto\n"
	"    Symsize neg 0 rlineto\n"
	"    Symsize 2 div Symsize neg rlineto\n"
	"    0 Symsize rmoveto\n"
	"    Symsize -2 div Symsize neg rlineto\n"
	"    Symsize 0 rlineto\n"
	"    Symsize -2 div Symsize rlineto\n"
	"    stroke\n",

	"    currentpoint\n"
	"    sym0\n"
	"    moveto\n"
	"    sym3\n",

	"    currentpoint\n"
	"    Symsize 2 div 0 rmoveto\n"
	"    2 copy Symsize 2 div 0 360 arc\n"
	"    closepath\n"
	"    gsave 1 setgray fill grestore\n"
	"    moveto\n"
	"    gsave\n"
	"        clip\n"
	"        sym4\n"
	"    grestore\n"
	"    stroke\n",

	"    currentpoint\n"
	"    sym0\n"
	"    moveto\n"
	"    2 setlinejoin\n"
	"    sym2\n"
	"    0 setlinejoin\n",

	"    Symsize -2 div dup rmoveto\n"
	"    Symsize 0 rlineto\n"
	"    0 Symsize rlineto\n"
	"    Symsize neg 0 rlineto\n"
	"    closepath\n"
	"    fill\n",

	"    currentpoint\n"
	"    Symsize 2 div 0 rlineto\n"
	"    Symsize 2 div 0 360 arc\n"
	"    closepath\n"
	"    fill\n",

	"    Symsize -2 div dup rmoveto\n"
	"    Symsize 0 rlineto\n"
	"    Symsize -2 div Symsize rlineto\n"
	"    closepath\n"
	"    fill\n",

	"    Symsize 2 div\n"
	"    dup neg 0 exch rmoveto\n"
	"    dup dup rlineto\n"
	"    dup dup neg exch rlineto\n"
	"    neg dup rlineto\n"
	"    closepath\n"
	"    fill\n"
    };

#define defFONTSIZE 8

int
    arg_c,
    currentgroup = -1,
    labelcount = 0,
    *groups = NULL,
    ngroup = 1,
    max_colors = 0,
    n_colors = maxCOLORS,
    fontsize = defFONTSIZE,
    PSlevel = 1,
    top,
    inputline = 0,
    max = 0,
    used = 0,
    urx = 459,
    width = 300,
    leftmargin = 150,
    leftmargin2;

float
    **usercolors = NULL,
    LineSkip = -1,
    LineSkip2 = -1,
    RulerSkip = -1,
    exponent = 1.0,
    maxlabelwidth = 0.0,
    maxlabelwidth1 = 0.0,
    maxlabelwidth2 = 0.0,
    maxvalue = -FLT_MAX,
    minvalue = FLT_MAX;

char
    *outfile = NULL,
    *colorfile = NULL,
    **arg_v,
    *fontname,
    buffer [BUFSIZE + 1],
    buf2 [ BUFSIZE + 1],
    *programname,
    *no_mem_buffer,
    Out_of_memory [] = "Out of Memory!";

FILE
    *fp,
    *fp_out;

int
    *fontwidths,
    helvetica [] = {
	   0,  278,  278,  278,  278,  278,  278,  278,
	 278,  278,  278,  278,  278,  278,  278,  278,
	 278,  278,  278,  278,  278,  278,  278,  278,
	 278,  278,  278,  278,  278,  278,  278,  278,
	 278,  278,  355,  556,  556,  889,  667,  222,
	 333,  333,  389,  584,  278,  584,  278,  278,
	 556,  556,  556,  556,  556,  556,  556,  556,
	 556,  556,  278,  278,  584,  584,  584,  556,
        1015,  667,  667,  722,  722,  667,  611,  778,
	 722,  278,  500,  667,  556,  833,  722,  778,
	 667,  778,  722,  667,  611,  722,  667,  944,
	 667,  667,  611,  278,  278,  278,  469,  556,
	 222,  556,  556,  500,  556,  556,  278,  556,
	 556,  222,  222,  500,  222,  833,  556,  556,
	 556,  556,  333,  500,  278,  556,  500,  722,
	 500,  500,  500,  334,  260,  334,  584,  278,
	 278,  278,  278,  278,  278,  278,  278,  278,
	 278,  278,  278,  278,  278,  278,  278,  278,
	 278,  333,  333,  333,  333,  333,  333,  333,
	 333,  278,  333,  333,  278,  333,  333,  333,
	 278,  333,  556,  556,  556,  556,  260,  556,
	 333,  737,  370,  556,  584,  333,  737,  333,
	 400,  584,  333,  333,  333,  556,  537,  278,
	 333,  333,  365,  556,  834,  834,  834,  611,
	 667,  667,  667,  667,  667,  667, 1000,  722,
	 667,  667,  667,  667,  278,  278,  278,  278,
	 722,  722,  778,  778,  778,  778,  778,  584,
	 778,  722,  722,  722,  722,  667,  667,  611,
	 556,  556,  556,  556,  556,  556,  889,  500,
	 556,  556,  556,  556,  278,  278,  278,  278,
	 556,  556,  556,  556,  556,  556,  556,  584,
	 611,  556,  556,  556,  556,  500,  556,  500   },
    times [] = {
	   0,  250,  250,  250,  250,  250,  250,  250,
	 250,  250,  250,  250,  250,  250,  250,  250,
	 250,  250,  250,  250,  250,  250,  250,  250,
	 250,  250,  250,  250,  250,  250,  250,  250,
	 250,  333,  408,  500,  500,  833,  778,  333,
	 333,  333,  500,  564,  250,  564,  250,  278,
	 500,  500,  500,  500,  500,  500,  500,  500,
	 500,  500,  278,  278,  564,  564,  564,  444,
	 921,  722,  667,  667,  722,  611,  556,  722,
	 722,  333,  389,  722,  611,  889,  722,  722,
	 556,  722,  667,  556,  611,  722,  722,  944,
	 722,  722,  611,  333,  278,  333,  469,  500,
	 333,  444,  500,  444,  500,  444,  333,  500,
	 500,  278,  278,  500,  278,  778,  500,  500,
	 500,  500,  333,  389,  278,  500,  500,  722,
	 500,  500,  444,  480,  200,  480,  541,  250,
	 250,  250,  250,  250,  250,  250,  250,  250,
	 250,  250,  250,  250,  250,  250,  250,  250,
	 278,  333,  333,  333,  333,  333,  333,  333,
	 333,  250,  333,  333,  250,  333,  333,  333,
	 250,  333,  500,  500,  500,  500,  200,  500,
	 333,  760,  276,  500,  564,  333,  760,  333,
	 400,  564,  300,  300,  333,  500,  453,  250,
	 333,  300,  310,  500,  750,  750,  750,  444,
	 722,  722,  722,  722,  722,  722,  889,  667,
	 611,  611,  611,  611,  333,  333,  333,  333,
	 722,  722,  722,  722,  722,  722,  722,  564,
	 722,  722,  722,  722,  722,  722,  556,  500,
	 444,  444,  444,  444,  444,  444,  667,  444,
	 444,  444,  444,  444,  278,  278,  278,  278,
	 500,  500,  500,  500,  500,  500,  500,  564,
	 500,  500,  500,  500,  500,  500,  500,  500  };

void
    process_args (void),
    setclgroups (int cluster, int group),
    process (int n),
    process_width (int n),
    trim (void),
    psstring (void),
    *s_malloc (size_t size),
    *s_realloc (void *block, size_t size),
    get_programname (char const *argv0),
    errit (char const *format, ...),
    syntax (int);

float
    psstringwidth (char const *s);

char
    *get_arg (void),
    *s_strdup (char const *s);

BOOL
    my_getline_bool (BOOL required);

int main (int argc, char *argv [])
{
    int
        i,
        j,
        k,
        n,
	x1,
	y1,
	x2,
	y2;
    float
        f,
        step,
	r,
	g,
	b;
    BOOL
        found,
	int2float;
    time_t
        tp;

    no_mem_buffer = (char *) malloc (1024);

    get_programname (argv [0]);

    if (argc == 1 && isatty (fileno (stdin)))
	syntax (0);

    fontname = "Helvetica";
    fontwidths = helvetica;

    arg_c = argc;
    arg_v = argv;
    process_args ();

    if (outfile) {
	fp_out = fopen (outfile, "w");
	if (! fp_out)
	    errit ("Creating file \"%s\": %s", outfile, strerror (errno));
    } else
	fp_out = stdout;

    if (example) {
	fputs (
	    "# Example cluster file\n"
	    "1 .12\n"
	    "L Norwegian\n"
	    "L Swedish\n"
	    "2 .15\n"
	    "C 1\n"
	    "L Danish\n"
	    "3 .3\n"
	    "L Dutch\n"
	    "L German\n"
	    "4 .35 Nordic group\n"
	    "L Icelandic\n"
	    "C 2\n"
	    "5 .7\n"
	    "C 4\n"
	    "C 3\n",
	    fp_out
	    );
	if (outfile)
	    fclose (fp_out);
	return 0;
    }

    if (patterns || symbols || numbers) {
	if (colorlabel || colorlink)
	    errit ("No colours with %s", patterns ? "patterns" : (symbols ? "symbols" : "numbers"));
	if ((ngroup < 2 || ngroup > n_colors) && ! numbers)
	    errit ("Illegal number of %s: %i", patterns ? "patterns" : "symbols", ngroup);
	if (patterns)
	    PSlevel = 2;
	labels = FALSE;
    }

    if ((colorlink || colorlabel) && (ngroup < 2 || ngroup > n_colors) && ! use_rainbow)
	errit ("Invalid number of groups with coloured labels or links. Try rainbow colours.");

    if (use_rainbow && ! (colorlink || colorlabel))
	errit ("Missing option -c and/or -C with rainbow colours");

    if (colorlabel && ! labels)
	errit ("Colour for no labels\n");

    if (fontsize < 4)
	errit ("fontsize too small");
    if (fontsize > 20)
	errit ("fontsize too large");

    if (evenodd && ! labels)
	errit("Placement of labels in two colums without labels");

    if (use_usercolours && (colorlabel || colorlink)) {
       int2float = FALSE;
       n_colors = 0;
       fp = fopen (colorfile, "r");
       if (! fp)
	   errit ("Opening file \"%s\": %s", colorfile, strerror (errno));
       inputline = 0;
       while (my_getline_bool (FALSE)) {
           if (n_colors == max_colors) {
               max_colors += 16;
               usercolors = (float **) s_realloc (usercolors, max_colors * sizeof (float**));
           }
           if (sscanf (buffer, "%f %f %f", &r, &g, &b) != 3)
                errit ("Missing value(s) for in file \"%s\", line %i", colorfile, inputline);
           if (r < 0 || r > 255)
                errit ("Red component out of range in file \"%s\", line %i", colorfile, inputline);
           if (g < 0 || g > 255)
                errit ("Green component out of range in file \"%s\", line %i", colorfile, inputline);
           if (b < 0 || b > 255)
                errit ("Blue component out of range in file \"%s\", line %i", colorfile, inputline);
           if (r > 1 || g > 1 || b > 1)
               int2float = TRUE;
           usercolors [n_colors] = s_malloc (3 * sizeof (float));
           usercolors [n_colors][0] = r;
           usercolors [n_colors][1] = g;
           usercolors [n_colors][2] = b;
           n_colors++;
       }
       fclose (fp);
       if (int2float)
           for (i = 0; i < n_colors; i++)
               for (j = 0; j < 3; j++)
                   usercolors [i][j] /= 255;
       inputline = 0;
    }

    switch (arg_c) {
        case 1:
            if (isatty (fileno (stdin)))
                syntax (1);
            fp = stdin;
            break;
	case 2:
            fp = fopen (arg_v [1], "r");
            if (! fp)
                errit ("Opening file \"%s\": %s", arg_v [1], strerror (errno));
            break;
	default:
            syntax (1);
    }

    while (my_getline_bool (FALSE)) {
        if (used == max) {
            max += 256;
            cl = (CLUSTER *) s_realloc (cl, max * sizeof (CLUSTER));
        }
        if (sscanf (buffer, "%i %g%n", &(cl [used].index), &(cl [used].value), &i) < 2)
            errit ("Syntax error at line %i: \"%s\"", inputline, buffer);
	if (cl [used].value > maxvalue)
	    maxvalue = cl [used].value;
        memmove (buffer, buffer + i, strlen (buffer + i) + 1);
        trim ();
        if (buffer [0] && buffer [0] != '#') {
            psstring ();
            cl [used].text = s_strdup (buffer);
        } else
            cl [used].text = NULL;
        for (n = 0; n < 2; n++) {
            my_getline_bool (TRUE);
            switch (buffer [0]) {
  	        case 'l':
                case 'L':
                    cl [used].node [n] = LBL;
                    buffer [0] = ' ';
                    trim ();
                    psstring ();
                    if ((f = psstringwidth (buffer)) > maxlabelwidth)
                        maxlabelwidth = f;
                    cl [used].n [n].label = s_strdup (buffer);
                    break;
  	        case 'c':
                case 'C':
                    cl [used].node [n] = CLS;
                    if (sscanf (buffer + 1, "%i", &(cl [used].n [n].cluster)) != 1)
                        errit ("Missing cluster number at line %i", inputline);
                    break;
		default:
                    errit ("Syntax error at line %i: \"%s\"", inputline, buffer);
	    }
        }
        used++;
    }

    if (argc == 2)
        fclose (fp);

    if (!used)
        errit ("No data");

    /* replace indexes */
    for (i = 0; i < used; i++)
        for (j = 0; j < 2; j++)
            if (cl [i].node [j] == CLS)
                for (k = 0; k < used; k++)
		    if (cl [i].n [j].cluster == cl [k].index) {
                        cl [i].n [j].cluster = k;
                        break;
		    }

    /* locate top node */
    top = 0;
    do {
        found = FALSE;
        for (i = 1; i < used; i++)
            if ((cl [i].node [0] == CLS && cl [i].n [0].cluster == top) ||
                (cl [i].node [1] == CLS && cl [i].n [1].cluster == top)
            ) {
                top = i;
                found = TRUE;
                break;
	    }
    } while (found);

    if (! mindefined) {
        for (i = 0; i < used; i++)
            if (cl [i].value < minvalue)
                minvalue = cl [i].value;
        if (minvalue > 0)
            minvalue = 0;
    }

    for (i = 0; i < used; i++)
        if (
            cl [i].text
         && (f = psstringwidth (cl [i].text)
               + leftmargin + ((float) width) / pow (maxvalue - minvalue, exponent) * pow (cl [i].value - minvalue, exponent) + 5.0) > urx
        )
            urx = f;

    if (ngroup > 1) {

	/* divide into color groups */
	j = 0;
	for (i = 0; i < used; i++) {
	    cl [i].group [0] = cl [i].group [1] = 1;
	    for (k = 0; k < 2; k++)
		if (cl [i].node [k] == LBL)
		    j++;
	}
	if (ngroup > j)
	    errit ("Too many groups");
	groups = (int *) s_malloc (ngroup * sizeof (int));
	groups [0] = top;
	for (n = 1; n < ngroup; n++) {
	    f = - FLT_MAX;
	    for (i = 0; i < n; i++)
		if (groups [i] < used && cl [groups [i]].value > f) {
		    j = i;
		    f = cl [groups [i]].value;
		}
	    cl [groups [j]].group [0] = n + 1;
	    cl [groups [j]].group [1] = j + 1;
	    groups [n] = (cl [groups [j]].node [0] == CLS) ? cl [groups [j]].n [0].cluster : (INT_MAX);
	    groups [j] = (cl [groups [j]].node [1] == CLS) ? cl [groups [j]].n [1].cluster : (INT_MAX);
	    setclgroups (groups [n], n + 1);
	    /* setclgroups (groups [j], j + 1); */
	}

    }

    if (labels) {
	if (LineSkip < 0) {
	    LineSkip = 1.2 * fontsize;
	    if (evenodd)
		LineSkip /= 2;
	}
	if (LineSkip2 < 0) {
	    LineSkip2 = 1.5 * LineSkip;
	    if (evenodd)
		LineSkip2 *= 2;
	}
	if (RulerSkip < 0)
	    RulerSkip = 1.5 * LineSkip + 4;
    } else {
	if (LineSkip2 < 0)
	    LineSkip2 = 4;
	if (LineSkip < 0) {
	    LineSkip = 2;
	    if ((used - 1) * LineSkip + (ngroup - 1) * LineSkip2 > 530) {
		LineSkip = (530 - (ngroup - 1) * LineSkip2) / (used - 1);
	    }
	}
	if (RulerSkip < 0)
	    RulerSkip = LineSkip2 + 4;
    }

    fputs (
        "%!PS-Adobe-3.0 EPSF-3.0\n"
        "%%BoundingBox: ",
        fp_out
    );

    x1 = labels ? (leftmargin - 6 - maxlabelwidth) : leftmargin - 10;
    if (evenodd) {
	process_width (top);
	leftmargin2 = leftmargin - 4 - maxlabelwidth1;
	x1 = leftmargin2 - 6 - maxlabelwidth2;
    }
    if (patterns)
	x1 = 100;
    if (symbols)
	x1 = 120;
    if (numbers)
	x1 = 110;
    x2 = urx;
    y1 = 700 - (used - ngroup + 1) * LineSkip - (ngroup - 1) * LineSkip2;
    if (ruler)
	y1 -= RulerSkip + fontsize + 1;
    else if (labels)
	y1 -= fontsize / 2;
    y2 = 701;
    if (labels)
	y2 += fontsize / 2;

    fprintf (fp_out, "%i %i %i %i\n", x1, y1, x2, y2);

    fputs (
        "%%Creator: ",
        fp_out
    );
    fprintf (fp_out, "%s", programname);
    fputs (
        ", Version " denVERSION ", (c) P. Kleiweg 1997 - 2005\n"
        "%%CreationDate: ",
        fp_out
    );
    time (&tp);
    fputs (asctime (localtime (&tp)), fp_out);

    if (argc == 2) {
        fputs ("%%Title: ", fp_out);
        fprintf (fp_out, "%s %i\n", arg_v [1], ngroup);
    }

    fputs ("%%LanguageLevel: ", fp_out);
    fprintf (fp_out, "%i\n", PSlevel);

    fputs (
        "%%EndComments\n"
        "\n"
        "64 dict begin\n"
        "\n",
        fp_out
    );
    fprintf (
	fp_out,
	"/EXP { %g exp } def\n"
        "/FontName /%s def\n"
        "/FontSize %i def\n"
        "/LineSkip %g def\n"
        "/LineSkip2 %g def\n"
	"\n",
	exponent,
	fontname,
        fontsize,
	LineSkip,
	LineSkip2
    );
    fprintf (
	fp_out,
        "/TopMargin 700 def\n"
	"/LeftMargin %i def\n",
	leftmargin
    );
    if (evenodd)
	fprintf (
	    fp_out,
	    "/LeftMargin2 %i def\n",
	    leftmargin2
	);
    fprintf (
	fp_out,
	"\n"
        "/Width %i def\n"
        "\n",
	width
    );

    if (ruler) {
	fprintf (
            fp_out,
	    "/RulerSkip %g def\n"
	    "/RulerStep ",
	    RulerSkip
	);
	step = pow (10, ceil (log10 (maxvalue - minvalue)) - 1);
	if ((maxvalue - minvalue) / step > 6.0)
	    step *= 2.0;
	else if ((maxvalue - minvalue) / step < 3.0)
	    step *= 0.5;
	fprintf (fp_out, "%g def\n\n", step);
    }

    if (colorlink)
	fputs (
	    "/clw { 1 setlinewidth } bind def\n"
	    "/blw { .5 setlinewidth } bind def\n"
	    "\n",
	    fp_out
	);
    else
	fputs (
	    ".5 setlinewidth\n"
	    "\n",
	    fp_out
	);

    if (evenodd)
	fputs ("\n/oelinewidth .2 def\n\n", fp_out);

    if (patterns) {
	fputs (
            "<<\n"
            "    /PatternType 1\n"
            "    /PaintType 1\n"
            "    /TilingType 1\n"
            "    /PaintProc {\n"
            "        begin\n"
            "            XStep\n"
            "            YStep\n"
            "            1\n"
            "            [ 1 0 0 1 0 0 ]\n"
            "            data\n"
            "            image\n"
            "        end\n"
            "    }\n"
            ">>\n"
            "/pdict exch def\n"
            "\n"
            "% stack in:  /label width height patterndata\n"
            "% stack out: -\n"
            "/defpattern {\n"
            "    /pat exch def\n"
            "    /y exch def\n"
            "    /x exch def\n"
            "    pdict /BBox [ 0 0 x y ] put\n"
            "    pdict /XStep x put\n"
            "    pdict /YStep y put\n"
            "    pdict /data pat put\n"
            "    pdict [ 72 60 div 0 0\n"
            "            72 60 div 0 0 ] makepattern\n"
            "    def\n"
            "} bind def\n"
	    "\n",
	    fp_out
	);
	for (i = 0; i < ngroup; i++)
	    fprintf (fp_out, "/c%-2i %s defpattern\n", i + 1, pat [i]);
	fputs ("\n", fp_out);
    }

    if (numbers)
	fputs ("/NumFontSize 12 def\n\n", fp_out);

    if (symbols) {
	fputs (
	    "/Symsize 8 def\n"
	    "/Symlw .7 def\n"
	    "\n",
	    fp_out
	);
	for (i = 0; i < ngroup; i++)
	    fprintf (fp_out, "/c%-2i /sym%-2i def\n", i + 1, i);
	fputs ("\n", fp_out);
    }

    fputs (
        "%%% End of User Options %%%\n"
	"\n",
        fp_out
    );

    if (patterns)
	fputs (
	    "/c0 {\n"
	    "    COL {\n"
	    "        Y YY ne {\n"
	    "            /Pattern setcolorspace P setcolor\n"
	    "            100 Y moveto\n"
	    "            140 Y lineto\n"
	    "            140 YY LineSkip add lineto\n"
	    "            100 YY LineSkip add lineto\n"
	    "            closepath\n"
	    "            fill\n"
	    "            0 setgray\n"
	    "        } if\n"
	    "        /COL false def\n"
	    "    } if\n"
	    "} bind def\n"
	    "\n",
	    fp_out
	);

    if (symbols) {
	for (i = 0; i < ngroup; i++)
	    fprintf (fp_out, "/sym%-2i {\n%s} bind def\n\n", i, sym [i]);
	fputs (
	    "/c0 {\n"
	    "    COL {\n"
	    "        140 Y moveto\n"
	    "        135 Y lineto\n"
	    "        135 YY lineto\n"
	    "        140 YY lineto\n"
	    "        stroke\n"
	    "        currentlinewidth\n"
	    "        130 Symsize 2 div sub  Y YY add 2 div moveto\n"
	    "        gsave\n"
	    "            1 setgray\n"
	    "            Symsize -2 div 3 sub dup rmoveto\n"
	    "            Symsize 6 add 0 rlineto\n"
	    "            0 Symsize 6 add rlineto\n"
	    "            Symsize 6 add neg 0 rlineto\n"
	    "            closepath\n"
	    "            fill\n"
	    "        grestore\n"
	    "        Symlw setlinewidth\n"
	    "        P cvx exec\n"
	    "        setlinewidth\n"
	    "        /COL false def\n"
	    "    } if\n"
	    "} bind def\n"
	    "\n",
	    fp_out
	);
    }

    if (numbers) {
	fputs (
	    "/c0 {\n"
	    "    COL {\n"
	    "        140 Y moveto\n"
	    "        135 Y lineto\n"
	    "        135 YY lineto\n"
	    "        140 YY lineto\n"
	    "        stroke\n"
	    "        130 Y YY add 2 div Shift sub moveto\n"
	    "        P 10 string cvs\n"
	    "        dup stringwidth pop neg 0 rmoveto show\n"
	    "        /COL false def\n"
	    "    } if\n"
	    "} bind def\n"
	    "\n",
	    fp_out
	);
    }

    if (patterns || symbols || numbers)
	fputs (
	    "/col {\n"
	    "    c0\n"
	    "    /Y y def\n"
	    "    /YY y def\n"
	    "    /P exch def\n"
	    "    /COL true def\n"
	    "} bind def\n"
	    "\n"
	    "/COL false def\n"
	    "\n",
	    fp_out
	);

    if (colorlink || colorlabel) {
	fprintf (fp_out, "/SETCOLOR { set%scolor } bind def\n", use_rainbow ? "hsb" : "rgb");
	fprintf (fp_out, "/CURRENTCOLOR { current%scolor } bind def\n", use_rainbow ? "hsb" : "rgb");
	fprintf (fp_out, "/c0 {\n    0 setgray%s\n    /COL false def\n} bind def\n", colorlink ? "\n    blw" : "");
	if (use_rainbow)
            for (i = 0; i < ngroup; i++)
                fprintf (
                    fp_out,
                    "/c%i { %.4f 1 %s } def\n",
                    i + 1,
                    ((float) i) / ngroup,
                    ((i % 2) && ! use_bright) ? ".6" : "1"
                );
	else
	    for (i = 0; i < ngroup; i++)
		fprintf (
		    fp_out,
		    "/c%i { %g %g %g } def\n",
		    i + 1,
		    use_usercolours ? usercolors [i][0] : colors [i][0],
		    use_usercolours ? usercolors [i][1] : colors [i][1],
		    use_usercolours ? usercolors [i][2] : colors [i][2]
		);
	fputs (
	    "/col {\n"
	    "    SETCOLOR\n",
	    fp_out
	);
	if (colorlink)
	    fputs ("    clw\n", fp_out);
	fputs (
	    "    /COL true def\n"
	    "} bind def\n"
	    "\n"
	    "c0\n"
	    "\n",
	    fp_out
	);
    }

    if (eXtended && PSlevel == 1)
        fputs (
            "/ISOLatin1Encoding\n"
            "[/.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef\n"
            "/.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef\n"
            "/.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef\n"
            "/.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef\n"
            "/space /exclam /quotedbl /numbersign /dollar /percent /ampersand\n"
            "/quoteright /parenleft /parenright /asterisk /plus /comma /minus /period\n"
            "/slash /zero /one /two /three /four /five /six /seven /eight /nine\n"
            "/colon /semicolon /less /equal /greater /question /at /A /B /C /D /E /F\n"
            "/G /H /I /J /K /L /M /N /O /P /Q /R /S /T /U /V /W /X /Y /Z /bracketleft\n"
            "/backslash /bracketright /asciicircum /underscore /quoteleft /a /b /c /d\n"
            "/e /f /g /h /i /j /k /l /m /n /o /p /q /r /s /t /u /v /w /x /y /z\n"
            "/braceleft /bar /braceright /asciitilde /.notdef /.notdef /.notdef\n"
            "/.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef\n"
            "/.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /dotlessi /grave\n"
            "/acute /circumflex /tilde /macron /breve /dotaccent /dieresis /.notdef\n"
            "/ring /cedilla /.notdef /hungarumlaut /ogonek /caron /space /exclamdown\n"
            "/cent /sterling /currency /yen /brokenbar /section /dieresis /copyright\n"
            "/ordfeminine /guillemotleft /logicalnot /hyphen /registered /macron\n"
            "/degree /plusminus /twosuperior /threesuperior /acute /mu /paragraph\n"
            "/periodcentered /cedilla /onesuperior /ordmasculine /guillemotright\n"
            "/onequarter /onehalf /threequarters /questiondown /Agrave /Aacute\n"
            "/Acircumflex /Atilde /Adieresis /Aring /AE /Ccedilla /Egrave /Eacute\n"
            "/Ecircumflex /Edieresis /Igrave /Iacute /Icircumflex /Idieresis /Eth\n"
            "/Ntilde /Ograve /Oacute /Ocircumflex /Otilde /Odieresis /multiply\n"
            "/Oslash /Ugrave /Uacute /Ucircumflex /Udieresis /Yacute /Thorn\n"
            "/germandbls /agrave /aacute /acircumflex /atilde /adieresis /aring /ae\n"
            "/ccedilla /egrave /eacute /ecircumflex /edieresis /igrave /iacute\n"
            "/icircumflex /idieresis /eth /ntilde /ograve /oacute /ocircumflex\n"
            "/otilde /odieresis /divide /oslash /ugrave /uacute /ucircumflex\n"
            "/udieresis /yacute /thorn /ydieresis]\n"
            "def\n"
            "\n",
            fp_out
        );

    if (eXtended)
        fputs (
            "/RE {\n"
            "    findfont\n"
            "    dup maxlength dict begin {\n"
            "        1 index /FID ne { def } { pop pop } ifelse\n"
            "    } forall\n"
            "    /Encoding exch def\n"
            "    dup /FontName exch def\n"
            "    currentdict end definefont pop\n"
            "} bind def\n"
            "\n"
            "/Font-ISOlat1 ISOLatin1Encoding FontName RE\n",
	    fp_out
	);

    if (numbers) {
	fprintf (fp_out, "/Font1 %s findfont FontSize scalefont def\n", eXtended ? "/Font-ISOlat1" : "FontName");
	fputs (
	    "/Font2 FontName findfont NumFontSize scalefont def\n"
	    "\n"
	    "Font1 setfont\n",
	    fp_out
	);
    } else
	fprintf (fp_out, "%s findfont FontSize scalefont setfont\n\n", eXtended ? "/Font-ISOlat1" : "FontName");

    fputs (
        "gsave\n"
        "    newpath\n"
        "    0 0 moveto\n"
        "    (Ag) false charpath\n"
        "    pathbbox\n"
        "grestore\n"
        "/Up exch def\n"
	"pop\n"
	"neg /Down exch def\n"
	"pop\n",
	fp_out
    );
    if (numbers)
	fputs ("Font2 setfont\n", fp_out);
    fputs (
        "gsave\n"
        "    newpath\n"
        "    0 0 moveto\n",
	fp_out
    );
    fprintf (fp_out, "    (%c) false charpath\n", numbers ? '1' : 'x');
    fputs (
        "    pathbbox\n"
        "grestore\n"
        "2 div /Shift exch def\n"
	"pop\n"
	"pop\n"
	"pop\n"
	"\n"
        "/y TopMargin def\n"
        "\n",
        fp_out
    );

    fprintf (fp_out, "/Min %g def\n/Max %g def\n\n", minvalue, maxvalue);

    if (evenodd)
	fputs ("/oe false def\n\n", fp_out);

    fputs (
        "/nl {\n"
        "  /y y LineSkip add LineSkip2 sub def\n"
        "} bind def\n"
        "\n",
	fp_out
    );

    if (patterns || symbols || numbers)
	fputs (
	    "/Cstroke {\n"
	    "    COL { /YY y def } if\n"
	    "    stroke\n"
	    "} bind def\n"
	    "\n",
	    fp_out
	);
    else if (colorlabel && ! colorlink)
	fputs (
	    "/Cstroke {\n"
	    "    COL {\n"
	    "        CURRENTCOLOR\n"
	    "        0 setgray\n"
	    "        stroke\n"
	    "        SETCOLOR\n"
	    "    } {\n"
	    "        stroke\n"
	    "    } ifelse\n"
	    "} bind def\n"
	    "\n",
	    fp_out
	);
    else
	fputs (
	    "/Cstroke {\n"
	    "    stroke\n"
	    "} bind def\n"
	    "\n",
	    fp_out
	);

    if (colorlabel)
	fputs (
	    "/Cshow {\n"
	    "    COL {\n"
	    "        gsave\n"
	    "            -1 -1 Down sub rmoveto\n"
	    "            dup stringwidth pop 2 add dup 0 rlineto\n"
	    "            0 Down Up add 2 add rlineto\n"
	    "            neg 0 rlineto\n"
	    "            closepath\n"
	    "            fill\n"
	    "        grestore\n"
	    "        gsave\n"
	    "            currentgray .4 gt { 0 } { 1 } ifelse setgray\n"
	    "            show\n"
	    "        grestore\n"
	    "    } {\n"
	    "        show\n"
	    "    } ifelse\n"
	    "} bind def\n"
	    "\n",
	    fp_out
	);
    else if (colorlink)
	fputs (
	    "/Cshow {\n"
	    "    COL {\n"
	    "        CURRENTCOLOR\n"
	    "        0 setgray\n"
	    "        4 -1 roll\n"
	    "        show\n"
	    "        SETCOLOR\n"
	    "    } {\n"
	    "        show\n"
	    "    } ifelse\n"
	    "} bind def \n"
	    "\n",
	    fp_out
	);
    else if (numbers)
	fputs (
	    "/Cshow {\n"
	    "    Font1 setfont\n"
	    "    show\n"
	    "    Font2 setfont\n"
	    "} bind def\n"
	    "\n",
	    fp_out
	);
    else
	fputs (
	    "/Cshow {\n"
	    "    show\n"
	    "} bind def\n"
	    "\n",
	    fp_out
	);

    if (labels) {
	fputs (
	    "% stack in:  (label)\n"
	    "% stach out: x y\n"
	    "/l {\n"
	    "    dup stringwidth pop\n"
	    "    neg LeftMargin add 4 sub y Shift sub moveto\n",
	    fp_out
	);
	if (evenodd)
	    fputs (
	        "    oe {\n"
		"        LeftMargin2 LeftMargin sub 0 rmoveto\n"
		"        Cshow\n"
		"        gsave\n"
		"            LeftMargin2 y moveto\n"
		"            LeftMargin 4 sub y lineto\n"
		"            0 setgray\n"
		"            oelinewidth setlinewidth\n"
		"            stroke\n"
		"        grestore\n"
		"        /oe false def\n"
		"    } {\n"
		"        Cshow\n"
		"        /oe true def\n"
		"    } ifelse\n",
		fp_out
	    );
	else
	    fputs (
	        "    Cshow\n",
		fp_out
	    );
	fputs (
	    "    LeftMargin y\n"
	    "    /y y LineSkip sub def\n"
	    "} bind def\n"
	    "\n",
	    fp_out
	);
    } else
	fputs (
	    "% stack in:  -\n"
	    "% stach out: x y\n"
	    "/l {\n"
	    "    LeftMargin y\n"
	    "    /y y LineSkip sub def\n"
	    "} bind def\n"
	    "\n",
	    fp_out
	);

    fputs (
        "% stack in:  x1 y1 x2 y2 value\n"
	"% stack out: x3 y3\n"
	"/c {\n"
	    "    Min sub EXP Width mul Max Min sub EXP div LeftMargin add\n",
	fp_out
    );
    if (linktype == RECT)
	fputs (
	    "    5 1 roll\n"
	    "    3 index 3 index moveto\n"
	    "    4 index 3 index lineto\n"
	    "    4 index 1 index lineto\n"
	    "    1 index 1 index lineto\n"
	    "    Cstroke\n"
	    "    exch pop\n"
	    "    add 2 div\n"
	    "    exch pop\n",
	    fp_out
	);
    else if (linktype == ARC)
	fputs (
	    "    /x3 exch def\n"
	    "    /y2 exch def\n"
	    "    /x2 exch def\n"
	    "    /y1 exch def\n"
	    "    /x1 exch def\n"
	    "    /y3 y1 y2 add 2 div def\n"
	    "    x1 y1 moveto\n"
	    "    x3 x1 sub .552284 mul x1 add y1\n"
	    "    x3 y1 y3 sub .552284 mul y3 add\n"
	    "    x3 y3 curveto\n"
	    "    x3 y2 y3 sub .552284 mul y3 add\n"
	    "    x3 x2 sub .552284 mul x2 add y2\n"
	    "    x2 y2 curveto\n"
	    "    Cstroke\n"
	    "    x3 y3\n",
	    fp_out
	);
    else
	fputs (
	    "    /x3 exch def\n"
	    "    /y2 exch def\n"
	    "    /x2 exch def\n"
	    "    /y1 exch def\n"
	    "    /x1 exch def\n"
	    "    /y3 y1 y2 add 2 div def\n"
	    "    x1 y1 moveto\n"
	    "    x3 y3 lineto\n"
	    "    x2 y2 lineto\n"
	    "    Cstroke\n"
	    "    x3 y3\n",
	    fp_out
	);
    fputs (
        "} bind def\n"
	"\n",
	fp_out
    );

    fputs (
        "% stack in:  x y (text)\n"
        "% stack out: x y\n"
        "/n {\n"
        "    2 index 3 add\n"
	"    2 index 2 add Down add\n"
        "    moveto\n"
        "    Cshow\n"
        "} bind def\n"
        "\n",
        fp_out
    );

    process (top);

    fputs (
        "pop pop\n"
        "\n",
	fp_out
     );

    if (ruler) {
	fputs ("% This draws the ruler\n", fp_out);
	if (numbers)
	    fputs ("Font1 setfont\n", fp_out);
	fputs (
	    "/setmark1 {\n"
	    "    Min sub EXP Width mul Max Min sub EXP div LeftMargin add\n"
	    "    y moveto\n"
	    "    0 2 rlineto\n"
	    "    stroke\n"
	    "} bind def\n"
	    "\n"
	    "/setmark {\n"
	    "    dup\n"
	    "    Min sub EXP Width mul Max Min sub EXP div LeftMargin add\n"
	    "    y moveto\n"
	    "    gsave\n"
	    "        0 4 rlineto\n"
	    "        stroke\n"
	    "    grestore\n"
	    "    0 FontSize neg rmoveto\n"
	    "    20 string cvs\n"
	    "    dup stringwidth pop 2 div neg 0 rmoveto\n"
	    "    show\n"
	    "} bind def\n"
	    "\n"
	    "/y y LineSkip add RulerSkip sub def\n"
	    "0 RulerStep 5 div Max {\n"
	    "    dup Min ge { setmark1 } { pop } ifelse\n"
	    "} for\n"
	    "0 RulerStep Max {\n"
	    "    dup Min ge { setmark } { pop } ifelse\n"
	    "} for\n"
	    "RulerStep neg 5 div dup Min {\n"
	    "    dup Max le { setmark1 } { pop } ifelse\n"
	    "} for\n"
	    "RulerStep neg dup Min {\n"
	    "    dup Max le { setmark } { pop } ifelse\n"
	    "} for\n"
	    "LeftMargin y moveto\n"
	    "Width 0 rlineto stroke\n"
	    "\n"
	    "% This draws the vertical line for X equals 0\n"
	    "Min 0 lt Max 0 gt and {\n"
	    "    Min neg EXP Width mul Max Min sub EXP div LeftMargin add\n"
	    "    dup\n"
	    "    y\n"
	    "    moveto\n"
	    "    TopMargin FontSize 2 div add\n"
	    "    lineto\n"
	    "    [ 3 ] 0 setdash\n"
	    "    stroke\n"
	    "} if\n"
	    "\n",
	    fp_out
	);
    }

    fputs (
        "end\n"
        "showpage\n"
        "%%EOF\n",
        fp_out
    );

    if (outfile)
	fclose (fp_out);

    return 0;
}

void setclgroups (int cluster, int group)
{
    int
        i;

    if (cluster < used)
        for (i = 0; i < 2; i++) {
            cl [cluster].group [i] = group;
            if (cl [cluster].node [i] == CLS)
                setclgroups (cl [cluster].n [i].cluster, group);
	}
}

void process (int i)
{
    int
        j;

    for (j = 0; j < 2; j++) {
        if (cl [i].node [j] == CLS)
            process (cl [i].n [j].cluster);
        else {
	    if (ngroup > 1 && currentgroup != cl [i].group [j]) {
                currentgroup = cl [i].group [j];
                if (first)
                    first = FALSE;
                else
                    fprintf (fp_out, "nl\n"); 
		if (colorlabel || colorlink || patterns || symbols || numbers)
		    fprintf (fp_out, "%s%i col\n", numbers ? "" : "c", currentgroup);
                labelcount = 0;
            }
	    if (labels)
		fprintf (fp_out, "(%s) l\n", cl [i].n [j].label);
	    else
		fprintf (fp_out, "l\n");
            labelcount++;
	}
    }
    if (ngroup > 1 && labelcount == 1 && (colorlink || colorlabel || patterns || symbols || numbers))
	    fputs ("c0\n", fp_out);

    fprintf (fp_out, "%g c\n", cl [i].value);
    if (cl [i].text)
        fprintf (fp_out, "(%s) n\n", cl [i].text);
    labelcount--;
}

void process_width (int i)
{
    int
        j;
    float
	f;

    for (j = 0; j < 2; j++) {
        if (cl [i].node [j] == CLS)
            process_width (cl [i].n [j].cluster);
        else {
	    f = psstringwidth (cl [i].n [j].label);
	    if (oe) {
		if (f > maxlabelwidth2)
		    maxlabelwidth2 = f;
		oe = FALSE;
	    } else {
		if (f > maxlabelwidth1)
		    maxlabelwidth1 = f;
		oe = TRUE;
	    }
	}
    }
}

void psstring ()
{
    int
        i,
        j;
    unsigned char
        *p;

    p = (unsigned char *)buffer;
    j = 0;
    for (i = 0; p [i]; i++) {
        if (j + 4 > BUFSIZE)
            errit ("String too long: \"%s\"", buffer);
        if (p [i] == '(' ||
            p [i] == ')' ||
            p [i] == '\\'
        ) {
            buf2 [j++] = '\\';
            buf2 [j++] = p [i];
	} else if (p [i] < 32 || p [i] > 126) {
            buf2 [j++] = '\\';
            sprintf (buf2 + j, "%03o", (unsigned) p [i]);
            j += 3;
            eXtended = TRUE;
        } else
            buf2 [j++] = p [i];
    }
    buf2 [j] = '\0';
    strcpy (buffer, buf2);
}

float psstringwidth (char const *s)
{
    char
        octal [4];
    float
        f;
    int
        i,
        c;

    f = 0.0;
    octal [3] = '\0';
    for (i = 0; s [i]; i++) {
        if (s [i] == '\\') {
            i++;
            if (s [i] == '\\' || s [i] == '(' || s [i] == ')')
                c = s [i];
            else {
                memcpy (octal, s + i, 3);
                c = (int)strtol (octal, (char**)NULL, 8);
                i += 2;
            }
        } else
            c = s [i];
        f += fontwidths [c];
    }
    return f / 1000.0 * (float)fontsize;
}

BOOL my_getline_bool (BOOL required)
/* Lees een regel in
 * Plaats in buffer
 * Negeer lege regels en regels die beginnen met #
 */
{
    int
        i;

    for (;;) {
        if (fgets (buffer, BUFSIZE, fp) == NULL) {
            if (required)
                errit ("Unexpected end of file");
            else
                return FALSE;
        }
        inputline++;
        i = strlen (buffer);
        while (i && isspace ((unsigned char) buffer [i - 1]))
            buffer [--i] = '\0';
        i = 0;
        while (buffer [i] && isspace ((unsigned char) buffer [i]))
            i++;
        if (buffer [i] == '#')
            continue;
        if (buffer [i]) {
            memmove (buffer, buffer + i, strlen (buffer) + 1);
            return TRUE;
        }
    }
}

void trim ()
{
    int
        i;

    i = 0;
    while (buffer [i] && isspace ((unsigned char) buffer [i]))
        i++;
    if (i)
        memmove (buffer, buffer + i, strlen (buffer + i) + 1);

    i = strlen (buffer);
    while (i && isspace ((unsigned char) buffer [i - 1]))
        buffer [--i] = '\0';
}

void *s_malloc (size_t size)
{
    void
        *p;

    p = malloc (size);
    if (! p) {
        free (no_mem_buffer);
        errit (Out_of_memory);
    }
    return p;
}

void *s_realloc (void *block, size_t size)
{
    void
        *p;

    if (block == NULL)
	p = malloc (size);
    else
	p = realloc (block, size);
    if (! p) {
        free (no_mem_buffer);
        errit (Out_of_memory);
    }
    return p;
}

char *s_strdup (char const *s)
{
    char
        *s1;

    if (s) {
        s1 = (char *) s_malloc (strlen (s) + 1);
        strcpy (s1, s);
    } else {
        s1 = (char *) s_malloc (1);
        s1 [0] = '\0';
    }
    return s1;
}

void errit (char const *format, ...)
{
    va_list
	list;

    fprintf (stderr, "\nError %s: ", programname);

    va_start (list, format);
    vfprintf (stderr, format, list);

    fprintf (stderr, "\n\n");

    exit (1);
}

void get_programname (char const *argv0)
{
#ifdef __MSDOS__
    char
        name [MAXFILE];
    fnsplit (argv0, NULL, NULL, name, NULL);
    programname = strdup (name);
#else
    char
        *p;
    p = strrchr (argv0, my_PATH_SEP);
    if (p)
        programname = strdup (p + 1);
    else
        programname = strdup (argv0);
#endif    
}

void process_args ()
{
    while (arg_c > 1 && arg_v [1][0] == '-') {
        switch (arg_v [1][1]) {
            case '2':
                PSlevel = 2;
                break;
	    case 'a':
		linktype = ARC;
		break;
            case 'b':
		minvalue = atof (get_arg ());
		mindefined = TRUE;
                break;
	    case 'C':
		colorlabel = TRUE;
		break;
	    case 'c':
		colorlink = TRUE;
		break;
	    case 'E':
		example = TRUE;
		break;
	    case 'e':
		exponent = atof (get_arg ());
		break;
	    case 'f':
		fontsize = atoi (get_arg ());
		break;
	    case 'h':
                use_rainbow = TRUE;
                use_bright = FALSE;
		break;
	    case 'H':
                use_rainbow = use_bright = TRUE;
		break;
	    case 'I':
		numbers = TRUE;
		patterns = symbols = FALSE;
		break;
	    case 'L':
		labels = FALSE;
		break;
	    case 'n':
		ngroup = atoi (get_arg ());
		break;
	    case 'o':
		outfile = get_arg ();
		break;
	    case 'p':
		evenodd = TRUE;
		leftmargin = 200;
		width = 250;
		break;
	    case 'P':
		patterns = TRUE;
		numbers = symbols = FALSE;
		break;
	    case 'Q':
		symbols = TRUE;
		numbers = patterns = FALSE;
		break;
	    case 'R':
		ruler = FALSE;
		break;
	    case 'r':
		RulerSkip = atof (get_arg ());
		break;
	    case 'S':
		LineSkip2 = atof (get_arg ());
		break;
	    case 's':
		LineSkip = atof (get_arg ());
		break;
	    case 'T':
		fontname = "Times-Roman";
		fontwidths = times;
		break;
	    case 't':
		linktype = TRI;
		break;
	    case 'u':
                colorfile = get_arg ();
		use_usercolours = TRUE;
                break;
            default:
                errit ("Illegal option '%s'", arg_v [1]);
        }
	arg_c--;
	arg_v++;
    }
}

char *get_arg ()
{
    if (arg_v [1][2])
        return arg_v [1] + 2;

    if (arg_c == 2)
        errit ("Missing argument for '%s'", arg_v [1]);

    arg_v++;
    arg_c--;
    return arg_v [1];
}

void syntax (int err)
{
    fprintf (
	     err ? stderr : stdout,
        "\n"
	"Grouped Colour Dendrogram Generator, Version " denVERSION "\n"
	"(c) P. Kleiweg 1997 - 2005\n"
	"\n"
        "Usage: %s [-2] [-a] [-b float] [-c] [-C] [-e float] [-f int] [-h] [-H]\n"
        "\t\t[-L] [-n int] [-o filename] [-p] [-r float] [-R] [-s float] [-S float]\n"
        "\t\t[-t] [-T] [-u colour file] [cluster file] > file.ps\n"
	"\n"
	"Usage: %s -I|-P|-Q -n int [-a] [-b float] [-e float] [-f int]\n"
        "\t\t[-o filename] [-r float] [-R] [-s float] [-S float]\n"
        "\t\t[-t] [-T] [cluster file] > file.ps\n"
	"\n"
	"Usage: %s -E [-o filename]\n"
	"\n"
        "\t-2 : PostScript level 2 (default: level 1)\n"
	"\t-a : curved links (default: rectangular)\n"
        "\t-b : base offset, may be negative\n"
        "\t     (default: minimum of 0.0 and smallest value in cluster file)\n"
        "\t-c : colour links (with number of groups from 2 through %i)\n"
        "\t-C : colour labels (with number of groups from 2 through %i)\n"
	"\t-e : exponent (default: 1.0)\n"
	"\t-E : example cluster file\n"
	"\t-f : fontsize (default: %i)\n"
        "\t-h : use rainbow colours, light and dark, instead of standard colours\n"
	"\t\t(no limit to number of groups)\n"
        "\t-H : use rainbow colours, light, instead of standard colours\n"
	"\t\t(no limit to number of groups)\n"
	"\t-I : cluster numbers (implies: -L)\n"
	"\t-L : no labels\n"
        "\t-n : number of groups (default: 1)\n"
	"\t-o : output file\n"
	"\t-p : placement of labels in two columns\n"
	"\t-P : patterns (implies: -2 -L, with number of groups from 2 through %i)\n"
	"\t-Q : symbols (implies: -L, with number of groups from 2 through %i)\n"
	"\t-r : line skip for ruler\n"
	"\t-R : no ruler\n"
	"\t-s : line skip within groups\n"
	"\t-S : line skip between groups\n"
	"\t-t : triangular links (default: rectangular)\n"
	"\t-T : font Times-Roman (default: Helvetica)\n"
	"\t-u : user-defined colours\n"
	"\n",
	programname,
	programname,
	programname,
	n_colors,
	n_colors,
	defFONTSIZE,
	n_colors,
	n_colors
    );
    exit (err);
}
