/*
 * file: cluster.c
 *
 * (c) P. Kleiweg 1998, 1999, 2001
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2,
 * or (at your option) any later version.
 *
 */

#define my_VERSION "1.02"

#define __NO_MATH_INLINES

#ifdef __WIN32__
#define my_PATH_SEP '\\'
#else
#define my_PATH_SEP '/'
#endif

#ifdef __MSDOS__
#  ifndef __COMPACT__
#    error Memory model COMPACT required
#  endif /* __COMPACT__ */
#  include <dir.h>
#  include <io.h>
#else /* Unix */
#  include <unistd.h>
#endif /* Unix */

#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <values.h>

#ifndef MAXFLOAT
#include <float.h>
#define MAXFLOAT FLT_MAX
#endif

#define BUFSIZE 2048

#define MIN(A, B) ((A) < (B) ? (A) : (B))
#define MAX(A, B) ((A) > (B) ? (A) : (B))

typedef struct
{
    int
        used,
        n_items,
        cl1,
        cl2,
        cluster;
    float
        f;
} CLUSTER;

CLUSTER
    *cl;

float
    **diff;

int
    size,
    input_line = 0,
    sorted = 1;

char
    *outfile = NULL,
    **labels,
    *programname,
    buffer [BUFSIZE],
    *no_mem_buffer,
    Out_of_memory [] = "Out of memory",
    a_sl [] = "Single Link (Nearest Neighbor)",
    a_cl [] = "Complete Link",
    a_ga [] = "Group Average",
    a_wa [] = "Weighted Average",
    a_uc [] = "Unweighted Centroid (Centroid)",
    a_wc [] = "Weighted Centroid (Median)",
    a_wm [] = "Ward's Method (Minimum Variance)";

FILE
    *fp_out;

void
    update_sl (int i),
    update_cl (int i),
    update_ga (int i),
    update_wa (int i),
    update_uc (int i),
    update_wc (int i),
    update_wm (int i),
    get_programname (char const *argv0),
    syntax (void),
    errit (char const *format, ...),
    *s_malloc (size_t size),
    *s_realloc (void *block, size_t size);
char
    *s_strdup (const char *s);
char const
    *sortclus (int i);
int
    getline (FILE *fp, int required, char const *filename);

int main (int argc, char *argv [])
{
    int
        i,
        j,
        k,
        p1 = 0,
        p2 = 0;
    float
        d;
    char
        *infile = NULL,
        *name_update = NULL;
    FILE
        *fp = NULL;
    time_t
        tp;
    void
        (*update_function)(int);
 
    no_mem_buffer = (char *) malloc (1024);

    get_programname (argv [0]);

    update_function = NULL;
    while (argc > 1 && argv [1][0] == '-') {
        if ((! strcmp (argv [1], "-sl")) || ! strcmp (argv [1], "-n")) {
                update_function = update_sl;
                name_update =  a_sl;
        } else if (! strcmp (argv [1], "-cl")) {
                update_function = update_cl;
                name_update =  a_cl;
        } else if (! strcmp (argv [1], "-ga")) {
                update_function = update_ga;
                name_update =  a_ga;
        } else if (! strcmp (argv [1], "-wa")) {
                update_function = update_wa;
                name_update =  a_wa;
        } else if (! strcmp (argv [1], "-uc")) {
                update_function = update_uc;
                name_update =  a_uc;
        } else if (! strcmp (argv [1], "-wc")) {
                update_function = update_wc;
                name_update =  a_wc;
        } else if ((! strcmp (argv [1], "-wm")) || ! strcmp (argv [1], "-w")) {
                update_function = update_wm;
                name_update =  a_wm;
	} else if (argv [1][1] == 'o') {
	    if (argv [1][2])
		outfile = argv [1] + 2;
	    else {
		argv++;
		argc--;
		if (argc == 1)
		    errit ("Missing argument for option -o");
		outfile = argv [1];
	    }
        } else if (! strcmp (argv [1], "-u")) {
                sorted = 0;
	} else
                syntax ();
        argc--;
        argv++;
    }
    if (! update_function)
        syntax ();

    switch (argc) {
	case 1:
	    if (isatty (fileno (stdin)))
		syntax ();
	    fp = stdin;
	    infile = "<stdin>";
	    break;
	case 2:
	    infile = argv [1];
	    fp = fopen (infile, "r");
	    if (! fp)
		errit ("Opening file \"%s\": %s", infile, strerror (errno));
	    break;
	default:
	    syntax ();
    }

    getline (fp, 1, infile);
    if (sscanf (buffer, "%i", &size) != 1)
	errit (
	    "file \"%s\", line %i\nTable size expected",
	    infile,
	    input_line
	);
    labels = (char **) s_malloc (size * sizeof (char *));
    for (i = 0; i < size; i++) {
        getline (fp, 1, infile);
        labels [i] = s_strdup (buffer);
    }

    diff = (float **) s_malloc ((2 * size - 1) * sizeof (float *));
    for (i = 0; i < 2 * size - 1; i++)
        diff [i] = (float *) s_malloc ((2 * size - 1) * sizeof (float));
    for (i = 0; i < size; i++) {
        diff [i][i] = 0.0;
        for (j = 0; j < i; j++) {
            getline (fp, 1, infile);
            if (sscanf (buffer, "%f", &d) != 1)
                errit ("file \"%s\", line %i\nValue expected", infile, input_line);
            diff [j][i] = diff [i][j] = d;
	}
    }

    if (fp != stdin)
	fclose (fp);

    cl = (CLUSTER *) s_malloc ((2 * size - 1) * sizeof (CLUSTER));
    for (i = 0; i < size; i++) {
        cl [i].used = 0;
        cl [i].n_items = 1;
        cl [i].cl1 = cl [i].cl2 = -1;
        cl [i].f = 0.0;
        cl [i].cluster = i;
    }

    for (i = size; i < 2 * size - 1; i++) {
        cl [i].used = 0;
        d = MAXFLOAT;
        for (j = 0; j < i; j++)
            if (! cl [j].used)
  	        for (k = 0; k < j; k++)
	            if ((! cl [k].used) && (diff [j][k] < d)) {
                        p1 = j;
                        p2 = k;
                        d = diff [j][k];
	            }
        cl [i].n_items = cl [p1].n_items + cl [p2].n_items;
        cl [p1].used = cl [p2].used = 1;
        cl [i].cl1 = p1;
        cl [i].cl2 = p2;
        cl [i].f = d;
        cl [i].cluster = i;

        update_function (i);
    }

    if (outfile) {
	fp_out = fopen (outfile, "w");
	if (! fp_out)
	    errit ("Creating file \"%s\": %s", outfile, strerror (errno));
    } else
	fp_out = stdout;

    time (&tp);
    fprintf (
	fp_out,
        "# Created by %s, (c) Peter Kleiweg 1998, 1999, 2001\n"
        "# More info: http://www.let.rug.nl/~kleiweg/clustering/\n"
        "# Input file: %s\n"
        "# Clustering algorithm: %s\n"
        "# Date: %s\n",
        programname,
        infile,
        name_update,
        asctime (localtime (&tp))
    ); 

    if (sorted)
        sortclus (2 * size - 2);
    for (i = size; i < 2 * size - 1; i++) {
        fprintf (fp_out, "%i %f\n", i, cl [i].f);
        if (cl [i].cl1 < size)
            fprintf (fp_out, "L %s\n", labels [cl [i].cl1]);
        else
            fprintf (fp_out, "C %i\n", cl [i].cl1);
        if (cl [i].cl2 < size)
            fprintf (fp_out, "L %s\n", labels [cl [i].cl2]);
        else
            fprintf (fp_out, "C %i\n", cl [i].cl2);
    }

    if (outfile)
	fclose (fp_out);

    return 0;
}

/* Single Link (Nearest Neighbor) */
void update_sl (int i)
{
    int
        j,
        cl1,
        cl2;

    cl1 = cl [i].cl1;
    cl2 = cl [i].cl2;
    diff [i][i] = 0.0;
    for (j = 0; j < i; j++)
        diff [i][j] = diff [j][i] = MIN (diff [cl1][j], diff [cl2][j]);

    for (j = 0; j < i; j++)
        if (cl [j].cluster == cl [i].cl1 ||
            cl [j].cluster == cl [i].cl2
        )
            cl [j].cluster = i;
}

/* Complete Link */
void update_cl (int i)
{
    int
        j,
        cl1,
        cl2;

    cl1 = cl [i].cl1;
    cl2 = cl [i].cl2;
    diff [i][i] = 0.0;
    for (j = 0; j < i; j++)
        diff [i][j] = diff [j][i] = MAX (diff [cl1][j], diff [cl2][j]);

    for (j = 0; j < i; j++)
        if (cl [j].cluster == cl [i].cl1 ||
            cl [j].cluster == cl [i].cl2
        )
            cl [j].cluster = i;
}

/* Group Average */
void update_ga (int i)
{
    int
        j,
        p1,
        p2;

    p1 = cl [i].cl1;
    p2 = cl [i].cl2;
    diff [i][i] = 0.0;
    for (j = 0; j < i; j++)
        diff [j][i] = diff [i][j] =
            (   ((float) cl [p1].n_items) * diff [j][p1]
              + ((float) cl [p2].n_items) * diff [j][p2] )
            / ((float) (cl [p1].n_items + cl [p2].n_items));
}

/* Weighted Average */
void update_wa (int i)
{
    int
        j,
        p1,
        p2;

    p1 = cl [i].cl1;
    p2 = cl [i].cl2;
    diff [i][i] = 0.0;
    for (j = 0; j < i; j++)
        diff [j][i] = diff [i][j] =
            (diff [j][p1] + diff [j][p2]) * 0.5;
}

/* Unweighted Centroid */
void update_uc (int i)
{
    int
        j,
        p1,
        p2;

    p1 = cl [i].cl1;
    p2 = cl [i].cl2;
    diff [i][i] = 0.0;
    for (j = 0; j < i; j++)
        diff [j][i] = diff [i][j] =
          (   ((float) cl [p1].n_items) * diff [j][p1]
            + ((float) cl [p2].n_items) * diff [j][p2]
            - ((float) cl [p1].n_items) * ((float) cl [p2].n_items) /
              ((float) (cl [p1].n_items + cl [p2].n_items)) *
              diff [p1][p2]  )
          / ((float) (cl [p1].n_items + cl [p2].n_items));
}

/* Weighted Centroid */
void update_wc (int i)
{
    int
        j,
        p1,
        p2;

    p1 = cl [i].cl1;
    p2 = cl [i].cl2;
    diff [i][i] = 0.0;
    for (j = 0; j < i; j++)
        diff [j][i] = diff [i][j] =
            (diff [j][p1] + diff [j][p2]) * 0.5
          - diff [p1][p2] * 0.25;
}

/* Ward's Method (Minimum Variance) */
void update_wm (int i)
{
    int
        j,
        p1,
        p2;

    p1 = cl [i].cl1;
    p2 = cl [i].cl2;
    diff [i][i] = 0.0;
    for (j = 0; j < i; j++)
        diff [j][i] = diff [i][j] =
            (   ((float) (cl [j].n_items + cl [p1].n_items)) * diff [j][p1]
              + ((float) (cl [j].n_items + cl [p2].n_items)) * diff [j][p2]
              - ((float) cl [j].n_items) * diff [p1][p2] )
            / ((float) (cl [j].n_items + cl [p1].n_items + cl [p2].n_items));
}

char const *sortclus (int i)
{
    int
        n;
    char const
        *s1,
        *s2;

    if (cl [i].cl1 < size)
        s1 = labels [cl [i].cl1];
    else
        s1 = sortclus (cl [i].cl1);
    if (cl [i].cl2 < size)
        s2 = labels [cl [i].cl2];
    else
        s2 = sortclus (cl [i].cl2);
    if (strcmp (s1, s2) > 0) {
        n = cl [i].cl1;
        cl [i].cl1 = cl [i].cl2;
        cl [i].cl2 = n;
        return s2;
    } else
        return s1;
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

char *s_strdup (const char *s)
{
    char
	*p = strdup (s);
    if (! p) {
	free (no_mem_buffer);
	errit (Out_of_memory);
    }
    return p;
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

int getline (FILE *fp, int required, char const *filename)
/* Lees een regel in
 * Plaats in buffer
 * Negeer lege regels en regels die beginnen met #
 * Verwijder leading/trailing white space
 */
{
    int
	i;

    for (;;) {
	if (fgets (buffer, BUFSIZE, fp) == NULL) {
	    if (required)
		errit ("Unexpected end of file in \"%s\"", filename);
	    else
		return 0;
	}
	input_line++;
	i = strlen (buffer);
	while (i && isspace (buffer [i - 1]))
	    buffer [--i] = '\0';
	i = 0;
	while (buffer [i] && isspace (buffer [i]))
	    i++;
	if (buffer [i] == '#')
	    continue;
	if (buffer [i]) {
	    memmove (buffer, buffer + i, strlen (buffer + i) + 1);
	    return 1;
	}
    }
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

void syntax ()
{
    fprintf (
	stderr,
	"\nData Clustering, Version " my_VERSION "\n"
	"(c) P. Kleiweg 1998, 1999, 2001\n"
        "\n"
	"Usage: %s -sl|-cl|-ga|-wa|-uc|-wc|-wm [-o filename] [-u] [difference table file]\n"
	"\n"
        "Clustering algorithm:\n"
        "\t-sl : %s, also: -n\n"
        "\t-cl : %s\n"
        "\t-ga : %s\n"
        "\t-wa : %s\n"
        "\t-uc : %s\n"
        "\t-wc : %s\n"
        "\t-wm : %s, also: -w\n\n"
	"\t-o  : output file\n"
        "\t-u  : unsorted\n\n",
	programname,
        a_sl,
        a_cl,
        a_ga,
        a_wa,
        a_uc,
        a_wc,
        a_wm
    );
    exit (0);
}
