/*
 * file: difftbl.c
 *
 * (c) P. Kleiweg 1998 - 2004
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2,
 * or (at your option) any later version.
 *
 */

#define my_VERSION "1.04"

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

#define BUFSIZE 2047

typedef struct
{
    char
	*s;
    float
	*f;
} VEC;

VEC
    *vec = NULL;

int
    pairs = 0,
    vec_max = 0,
    vec_n = 0,
    vec_size,
    input_line = 0;

char
    *outfile = NULL,
    *programname,
    buffer [BUFSIZE + 1],
    buf1 [BUFSIZE * 2 + 3],
    buf2 [BUFSIZE * 2 + 3],
    *no_mem_buffer,
    Out_of_memory [] = "Out of memory";

FILE
    *fp_out;

void
    get_programname (char const *argv0),
    syntax (void),
    errit (char const *format, ...),
    *s_malloc (size_t size),
    *s_realloc (void *block, size_t size),
    pearson_first (void);
char
    *s_strdup (const char *s);
char const
    *quote (char const *s, char *buf);
int
    my_getline (FILE *fp, int required, char const *filename);
float
    cityblock  (int i1, int i2),
    euclid     (int i1, int i2),
    pearson    (int i1, int i2),
    squarediff (int i1, int i2);
char
    *s_cityblock  = "City Block Distance",
    *s_euclid     = "Euclidean Difference",
    *s_pearson    = "1 - Pearson correlation coefficient",
    *s_squarediff = "Square Difference";

int main (int argc, char *argv [])
{
    int
        i,
        j;
    char
        *infile = NULL,
        *algorithm;
    float
        (*diff)(int, int);
    FILE
        *fp = NULL;
    time_t
        tp;

    no_mem_buffer = (char *)malloc (1024);

    get_programname (argv [0]);

    diff = euclid;
    algorithm = s_euclid;
    while (argc > 1 && argv [1][0] == '-') {
        if (! strcmp (argv [1], "-c")) {
            diff = cityblock;
            algorithm = s_cityblock;
        } else if (! strcmp (argv [1], "-e")) {
            diff = euclid;
            algorithm = s_euclid;
        } else if (! strcmp (argv [1], "-p")) {
            diff = pearson;
            algorithm = s_pearson;
        } else if (! strcmp (argv [1], "-s")) {
            diff = squarediff;
            algorithm = s_squarediff;
	} else if (! strcmp (argv [1], "-P"))
	    pairs = 1;
	else if (argv [1][1] == 'o') {
	    if (argv [1][2])
		outfile = argv [1] + 2;
	    else {
		argv++;
		argc--;
		if (argc == 1)
		    errit ("Missing argument for option -o");
		outfile = argv [1];
	    }
        } else
            syntax ();
        argv++;
        argc--;
    }

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

    if (outfile) {
	fp_out = fopen (outfile, "w");
	if (! fp_out)
	    errit ("Creating file \"%s\": %s", outfile, strerror (errno));
    } else
	fp_out = stdout;

    my_getline (fp, 1, infile);
    if (sscanf (buffer, "%i", &vec_size) != 1)
	errit (
	    "file \"%s\", line %i\nVectorsize expected",
	    infile,
	    input_line
	);
    while (my_getline (fp, 0, NULL)) {
        if (vec_n == vec_max) {
	    vec_max += 256;
	    vec = (VEC *) s_realloc (vec, vec_max * sizeof (VEC));
        }
        vec [vec_n].s = s_strdup (buffer);
        vec [vec_n].f = (float *) s_malloc (vec_size * sizeof (float));
	for (i = 0; i < vec_size; i++) {
	    my_getline (fp, 1, infile);
	    if (sscanf (buffer, "%f", &(vec [vec_n].f[i])) != 1)
		errit (
		    "file \"%s\", line %i\n"
		    "Missing value for vector \'%s\'",
		    infile,
		    input_line,
		    vec [vec_n].s
		);
	}
        vec_n++;
    }

    if (fp != stdin)
	fclose (fp);

    if (algorithm == s_pearson)
        pearson_first ();

    if (pairs) {
	for (i = 0; i < vec_n; i++)
	    for (j = 0; j < i; j++)
		fprintf (fp_out, "%f\t%s\t%s\n", diff (i, j), quote (vec [i].s, buf1), quote (vec [j].s, buf2));
	return 0;
    }

    time (&tp);
    fprintf (
	fp_out,
        "# Created by %s, (c) Peter Kleiweg 1998 - 2004\n"
        "# More info: http://www.let.rug.nl/~kleiweg/clustering/\n"
        "# Input file: %s\n"
        "# Distance measure used: %s\n"
        "# Date: %s\n",
        programname,
        infile,
        algorithm,
        asctime (localtime (&tp))
    ); 
    fprintf (fp_out, "# table size\n%i\n", vec_n);
    fprintf (fp_out, "# labels\n");
    for (i = 0; i < vec_n; i++)
        fprintf (fp_out, "%s\n", vec [i].s);
    for (i = 1; i < vec_n; i++) {
        fprintf (fp_out, "# diff [%i][1] ... diff [%i][%i]\n", i + 1, i + 1, i);
        for (j = 0; j < i; j++)
            fprintf (fp_out, "%f\n", diff (i, j));
    }

    if (outfile)
	fclose (fp_out);

    return 0;
}

float euclid (int i1, int i2)
{
    int
        i;
    float
        f;
    f = 0;
    for (i = 0; i < vec_size; i++)
        f += (vec [i1].f [i] - vec [i2].f [i]) * (vec [i1].f [i] - vec [i2].f [i]);
    return sqrt (f);
}

float cityblock (int i1, int i2)
{
    int
        i;
    float
        f;
    f = 0;
    for (i = 0; i < vec_size; i++)
        f += fabs (vec [i1].f [i] - vec [i2].f [i]);
    return f;
}

void pearson_first ()
{
    int
        i,
        j;
    float
        sum,
        m,
        sd;

    for (i = 0; i < vec_size; i++) {
        sum = 0.0;
        for (j = 0; j < vec_n; j++)
            sum += vec [j].f [i];
        m = sum / vec_n;
        sum = 0.0;
        for (j = 0; j < vec_n; j++)
            sum += (vec [j].f [i] - m) * (vec [j].f [i] - m);
        sd = sqrt (sum / (vec_n - 1));
        for (j = 0; j < vec_n; j++)
            vec [j].f [i] = (vec [j].f [i] - m) / sd;
    }
}

float pearson (int i1, int i2)
{
    int
        i;
    float
        f;
    f = 0.0;
    for (i = 0; i < vec_size; i++)
        f += vec [i1].f [i] * vec [i2].f [i];
    return 1 - (f / vec_size);
}

float squarediff (int i1, int i2)
{
    int
        i;
    float
        f;
    f = 0;
    for (i = 0; i < vec_size; i++)
        f += (vec [i1].f [i] - vec [i2].f [i]) * (vec [i1].f [i] - vec [i2].f [i]);
    return f;
}

char const *quote (char const *s, char *qbuffer)
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

int my_getline (FILE *fp, int required, char const *filename)
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
	while (i && isspace ((unsigned char) buffer [i - 1]))
	    buffer [--i] = '\0';
	i = 0;
	while (buffer [i] && isspace ((unsigned char) buffer [i]))
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
	"\n"
	"Vector difference, Version " my_VERSION "\n"
	"(c) P. Kleiweg 1998 - 2004\n"
	"\n"
	"Usage: %s [-c|-e|-p|-s] [-o filename] [-P] [vector file]\n"
	"\n"
        "\t-c : use %s\n"
        "\t-e : use %s (default)\n"
        "\t-p : use %s\n"
        "\t-s : use %s\n\n"
	"\t-o : output file\n"
        "\t-P : generate pair listing\n"
	"\n",
	programname,
        s_cityblock,
        s_euclid,
        s_pearson,
        s_squarediff
    );
    exit (0);
}
