/*
 * file: clgroup.c
 *
 * (c) P. Kleiweg 2000 - 2004
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2,
 * or (at your option) any later version.
 *
 */

#define clgroupVERSION "1.22"

#define __NO_MATH_INLINES

#ifdef __WIN32__
#define my_PATH_SEP '\\'
#else
#define my_PATH_SEP '/'
#endif

#ifdef __MSDOS__
#ifndef __COMPACT__
#error Memory model COMPACT required
#endif  /* __COMPACT__  */
#include <dir.h>
#include <io.h>
#include <math.h>
#else   /* Unix */
#include <unistd.h>
#endif  /* __MSDOS__  */
#include <ctype.h>
#include <errno.h>
#include <float.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX(A, B) ((A) > (B) ? (A) : (B))

typedef enum { CLS, LBL } NODETYPE;
typedef enum { FALSE = 0, TRUE = 1} BOOL;

typedef struct _cluster {
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

typedef struct {
    char
        *label;
    int
        group;
} LEAF;

LEAF
    *leaf;

#define BUFSIZE 1024

BOOL
    indexed = FALSE,
    ngroups_defined = FALSE;

int
    arg_c,
    *groups,
    ngroup,
    top,
    inputline = 0,
    max = 0,
    used = 0;

char
    **arg_v,
    *outfile = NULL,
    buffer [BUFSIZE + 1],
    qbuffer [2 * BUFSIZE + 2],
    *programname,
    *no_mem_buffer,
    Out_of_memory [] = "Out of Memory!";

FILE
    *fp,
    *fp_out;

void
    setclgroups (int cluster, int group),
    trim (void),
    *s_malloc (size_t size),
    *s_realloc (void *block, size_t size),
    get_programname (char const *argv0),
    process_args (void),
    errit (char const *format, ...),
    warn (char const *format, ...),
    syntax (int err);

char
    *get_arg (void),
    *s_strdup (char const *s);

char const
    *quote (char const *s);

BOOL
    my_getline_bool (BOOL required);

int
    lcmp (void const *p1, void const *p2);

int main (int argc, char *argv [])
{
    int
        i,
        j,
        k,
        n,
	g1,
	g2;
    float
        f;

    no_mem_buffer = (char *) malloc (1024);

    get_programname (argv [0]);

    if (argc == 1)
	syntax (0);

    arg_c = argc;
    arg_v = argv;
    process_args ();

    if (! ngroups_defined)
	syntax (1);

    if (ngroup < 2)
        errit ("Illegal number of color groups: %i", ngroup);

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
        if (sscanf (buffer, "%i %f%n", &(cl [used].index), &(cl [used].value), &i) < 2)
            errit ("Syntax error at line %i: \"%s\"", inputline, buffer);
        memmove (buffer, buffer + i, strlen (buffer + i) + 1);
        trim ();
        if (buffer [0] && buffer [0] != '#') {
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

    if (fp != stdin)
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
    for (i = 0; i < used; i++)
        cl [i].index = 1;
    for (i = 0; i < used; i++)
        for (j = 0; j < 2; j++)
            if (cl [i].node [j] == CLS)
                cl [cl [i].n [j].cluster].index = 0;
    for (i = 0; i < used; i++)
        if (cl [i].index) {
            top = i;
            break;
	}

    /* divide into groups */
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
        groups [n] = (cl [groups [j]].node [0] == CLS) ? cl [groups [j]].n [0].cluster : INT_MAX; 
        groups [j] = (cl [groups [j]].node [1] == CLS) ? cl [groups [j]].n [1].cluster : INT_MAX;
	setclgroups (groups [n], n + 1);
    }

    leaf = (LEAF *) s_malloc ((used + 1) * sizeof (LEAF));

    n = 0;
    for (i = 0; i < used; i++)
	for (j = 0; j < 2; j++)
	    if (cl [i].node [j] == LBL) {
		leaf [n].label = cl [i].n [j].label;
		leaf [n++].group = indexed ? cl [i].group [j] : - cl [i].group [j];
	    }
    qsort (leaf, used + 1, sizeof (LEAF), lcmp);
    
    if (! indexed) {
	g2 = 0;
	for (i = 0; i <= used; i++)
	    if (leaf [i].group < 0) {
		g1 = leaf [i].group;
		g2++;
		for (j = i; j <= used; j++)
		    if (leaf [j].group == g1)
			leaf [j].group = g2;
	    }
    }

    if (outfile) {
	fp_out = fopen (outfile, "w");
	if (! fp_out)
	    errit ("Creating file \"%s\": %s", outfile, strerror (errno));
    } else
	fp_out = stdout;

    if (indexed)
	for (i = 0; i <= used; i++)
	    fprintf (fp_out, "%4i\t%s\n", leaf [i].group, quote (leaf [i].label));
    else {
	n = 0;
	for (i = 0; i <= used; i++)
	    if (leaf [i].group > n) {
		if (n)
		    fprintf (fp_out, "\n");
		n = leaf [i].group;
		for (j = i; j <= used; j++)
		    if (leaf [j].group == n)
			fprintf (fp_out, "%s\n", leaf [j].label);
	    }
    }

    if (outfile)
	fclose (fp_out);

    return 0;
}

void process_args ()
{
    while (arg_c > 1 && arg_v [1][0] == '-') {
        switch (arg_v [1][1]) {
            case 'n':
                ngroup = atoi (get_arg ());
		ngroups_defined = TRUE;
                break;
            case 'o':
                outfile = get_arg ();
                break;
            case 'i':
                indexed = TRUE;
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

char const *quote (char const *s)
{
    int
	i,
	j;

    for (;;) {
	if (s [0] == '"')
	    break;
	for (i = 0; s [i]; i++)
	    if (isspace ((unsigned char) s [i]))
		break;
	if (s [i])
	    break;

	return s;
    }

    j = 0;
    qbuffer [j++] = '"';
    for (i = 0; s [i]; i++) {
	if (s [i] == '"' || s [i] == '\\')
	    qbuffer [j++] = '\\';
	qbuffer [j++] = s [i];
    }
    qbuffer [j++] = '"';
    qbuffer [j] = '\0';
    return qbuffer;
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

int lcmp (void const *p1, void const *p2)
{
    return strcmp (((LEAF *)p1)->label, ((LEAF *)p2)->label);
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

void warn (char const *format, ...)
{
    va_list
	list;

    fprintf (stderr, "\nWarning %s: ", programname);

    va_start (list, format);
    vfprintf (stderr, format, list);

    fprintf (stderr, "\n\n");
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

void syntax (int err)
{
    fprintf (
	err ? stderr : stdout,
        "\n"
        "Cluster Grouping, Version " clgroupVERSION "\n"
        "(c) P. Kleiweg 2000 - 2004\n"
	"\n"
        "Usage: %s -n int [-i] [-o filename] [cluster file]\n"
        "\n"
        "\t-n : Number of groups\n"
	"\t-i : Indexed output\n"
	"\t-o : Output file\n"
        "\n",
	programname
    );
    exit (err);
}

#ifdef __MSDOS__
/* link floating point scanner */
void dummy ()
{
    sqrt (0);
}
#endif
