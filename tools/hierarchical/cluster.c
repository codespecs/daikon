/*
 * file: cluster.c
 *
 * (c) P. Kleiweg 1998 - 2007
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2,
 * or (at your option) any later version.
 *
 */

#define my_VERSION "1.27"

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
#include <float.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifdef __WIN32__
#  include <sys/timeb.h>
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

void
    (*update_function)(int);

float
    *noise = NULL,
    **diff,
    **diff_in,
    **diff_cp;

int
    seed = 0,
    n_noise = 0,
    max_noise = 0,
    arg_c,
    size,
    input_line = 0,
    sorted = 1,
    cophenetic = 0,
    binary = 0,
    n_runs = 1,
    n_maxcl = 0,
    max_maxcl = 0,
    *maxcl = NULL,
    verbose = 0;

char
    *name_update = NULL,
    **arg_v,
    *outfile = NULL,
    **labels,
    *programname,
    buffer [BUFSIZE + 1],
    buffr2 [BUFSIZE + 1],
    buffr3 [BUFSIZE + 1],
    *no_mem_buffer,
    Out_of_memory [] = "Out of memory",
    a_sl [] = "Single Link (Nearest Neighbor)",
    a_cl [] = "Complete Link (Furthest Neighbor)",
    a_ga [] = "Group Average (UPGMA: Unweighted Pair Group Method using Arithmetic averages)",
    a_wa [] = "Weighted Average (WPGMA: Weighted Pair Group Method using Arithmetic averages)",
    a_uc [] = "Unweighted Centroid (Centroid, UPGMC: Unweighted Pair Group Method using Centroids)",
    a_wc [] = "Weighted Centroid (Median, WPGMC: Weighted Pair Group Method using Centroids)",
    a_wm [] = "Ward's Method (Minimum Variance)";

FILE
    *fp_out;

void
    update_cp (float d, int p1, int p2),
    update_sl (int i),
    update_cl (int i),
    update_ga (int i),
    update_wa (int i),
    update_uc (int i),
    update_wc (int i),
    update_wm (int i),
    process_args (void),
    get_programname (char const *argv0),
    syntax (void),
    errit (char const *format, ...),
    *s_malloc (size_t size),
    *s_realloc (void *block, size_t size),
    checkCancel(void);
char
    *get_arg (void),
    *s_strdup (const char *s);
char const
    *sortclus (int i);
int
    my_getline (FILE *fp, int required, char const *filename);

int main (int argc, char *argv [])
{
    int
        i,
        j,
        k,
	n,
        p1 = 0,
        p2 = 0,
	run,
	runnoise,
	nn_noise,
	nn_maxcl,
	n_total,
	counter;
    float
	sum,
	ssum,
	sd,
        d;
    char
        *infile = NULL;
    FILE
        *fp = NULL;
    time_t
        tp;

#ifdef __WIN32__
    struct timeb
	tb;
#endif
 
    no_mem_buffer = (char *) malloc (1024);

    get_programname (argv [0]);

    update_function = NULL;

    arg_c = argc;
    arg_v = argv;
    process_args ();

    if (! update_function)
        syntax ();
    if (n_runs > 1 && ((! cophenetic) || ! n_noise))
	errit ("-r only useful in combination with -b or -c, and -N");
    if (n_maxcl && ! cophenetic)
	errit ("-m only useful in combination with -b or -c");
    if (n_noise > 1 && ! cophenetic)
	errit ("Multiple noise levels only in combination with -b or -c");
    if (binary && ! n_maxcl)
	errit ("No -b without -m");

    switch (arg_c) {
	case 1:
	    if (isatty (fileno (stdin)))
		syntax ();
	    fp = stdin;
	    infile = "<stdin>";
	    break;
	case 2:
	    infile = arg_v [1];
	    fp = fopen (infile, "r");
	    if (! fp)
		errit ("Opening file \"%s\": %s", infile, strerror (errno));
	    break;
	default:
	    syntax ();
    }

    my_getline (fp, 1, infile);
    if (sscanf (buffer, "%i", &size) != 1)
	errit (
	    "file \"%s\", line %i\nTable size expected",
	    infile,
	    input_line
	);
    labels = (char **) s_malloc (size * sizeof (char *));
    for (i = 0; i < size; i++) {
        my_getline (fp, 1, infile);
        labels [i] = s_strdup (buffer);
    }

    diff_in = (float **) s_malloc (size * sizeof (float *));
    for (i = 1; i < size; i++)
	diff_in [i] = (float *) s_malloc (i * sizeof (float));
    for (i = 1; i < size; i++) {
        for (j = 0; j < i; j++) {
            my_getline (fp, 1, infile);
            if (sscanf (buffer, "%f", &d) != 1)
                errit ("file \"%s\", line %i\nValue expected", infile, input_line);
            diff_in [i][j] = d;
	}
    }

    if (fp != stdin)
	fclose (fp);

    for (i = 0; i < n_maxcl; i++)
	if (maxcl [i] > size)
	    errit ("Value for -m too large");

    if (cophenetic) {
	diff_cp = (float **) s_malloc (size * sizeof (float *));
	for (i = 1; i < size; i++) {
	    diff_cp [i] = (float *) s_malloc (i * sizeof (float));
	    for (j = 0; j < i; j++)
		diff_cp [i][j] = 0;
	}
    }

    diff = (float **) s_malloc ((2 * size - 1) * sizeof (float *));
    for (i = 0; i < 2 * size - 1; i++)
	diff [i] = (float *) s_malloc ((2 * size - 1) * sizeof (float));

    sd = 0;
    if (n_noise) {

#ifdef __WIN32__
	ftime (&tb);
	srand (tb.millitm ^ (tb.time << 8));
#else
	srand (time (NULL) ^ (getpid () << 8));
#endif

	if (seed)
	    srand (seed);

	n = 0;
	sum = ssum = 0.0;
	for (i = 0; i < size; i++)
	    for (j = 0; j < i; j++) {
		sum += diff_in [i][j];
		ssum += diff_in [i][j] * diff_in [i][j];
		n++;
	    }
	sd = sqrt ((ssum - sum * sum / (float) n) / (float) (n - 1));

    }

    cl = (CLUSTER *) s_malloc ((2 * size - 1) * sizeof (CLUSTER));

    nn_noise = n_noise ? n_noise : 1;
    nn_maxcl = n_maxcl ? n_maxcl : 1;

    n_total = n_runs * nn_noise * nn_maxcl;
    counter = n_runs * nn_noise;

    if (counter > 10)
	verbose = 1;

    for (run = 0; run < n_runs; run++) {

	for (runnoise = 0; runnoise < nn_noise; runnoise++) {

	    checkCancel();

	    for (i = 0; i < size; i++) {
		diff [i][i] = 0;
		for (j = 0; j < i; j++)
		    diff [i][j] = diff [j][i] = diff_in [i][j];
	    }

	    if (n_noise)
		for (i = 0; i < size; i++)
		    for (j = 0; j < i; j++)
			diff [i][j] = diff [j][i] = diff [i][j] + noise [runnoise] * sd * (((float) rand ()) / (float) RAND_MAX);

	    for (i = 0; i < size; i++) {
		cl [i].used = 0;
		cl [i].n_items = 1;
		cl [i].cl1 = cl [i].cl2 = -1;
		cl [i].f = 0.0;
		cl [i].cluster = i;
	    }

	    for (i = size; i < 2 * size - 1; i++) {
		cl [i].used = 0;
		d = FLT_MAX;
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

		if (cophenetic) {
		    if (n_maxcl) {
			k = 0;
			for (j = 0; j < n_maxcl; j++)
			    if (i > 2 * size - 1 - maxcl [j])
				k++;
			if (k)
			    update_cp (binary ? (k / (float) n_total) : (k * d / (float) n_total), p1, p2);
		    } else 
			update_cp (binary ? (1.0 / (float) n_total) : (d / (float) n_total), p1, p2);
		}

		update_function (i);

	    }

	    if (verbose) {
		fprintf (stderr, "  %i \r", counter--);
		fflush (stderr);
	    }
	}
    }

    if (verbose)
	fprintf (stderr, "    \r");

    if (outfile) {
	fp_out = fopen (outfile, "w");
	if (! fp_out)
	    errit ("Creating file \"%s\": %s", outfile, strerror (errno));
    } else
	fp_out = stdout;

    time (&tp);
    fprintf (
	fp_out,
	"# Created by %s, (c) Peter Kleiweg 1998 - 2007\n"
	"# More info: http://www.let.rug.nl/~kleiweg/L04/\n"
	"# More info: http://www.let.rug.nl/~kleiweg/clustering/\n"
	"# Input file: %s\n"
	"# Clustering algorithm: %s\n",
	programname,
	infile,
	name_update
    );
    if (cophenetic && ! binary) 
	fprintf (fp_out, "# Cophenetic differences\n");
    if (cophenetic && binary) 
	fprintf (fp_out, "# Binary differences\n");
    if (n_noise) {
	fputs ("# Noise:", fp_out);
	for (i = 0; i < n_noise; i++) {
	    if (i && (i % 8 == 0))
		fprintf (fp_out, "\n#    ");
	    fprintf (fp_out, " %g", noise [i]);
	}
	fputs ("\n", fp_out);
    }
    if (n_maxcl) {
	fputs ("# Clusters:", fp_out);
	for (i = 0; i < n_maxcl; i++) {
	    if (i && (i % 16 == 0))
		fprintf (fp_out, "\n#    ");
	    fprintf (fp_out, " %i", maxcl [i]);
	}
	fputs ("\n", fp_out);
    }
    if (n_runs > 1)
	fprintf (fp_out, "# Runs: %i\n", n_runs);
    fprintf (
	fp_out,
	"# Date: %s\n",
	asctime (localtime (&tp))
    );

    if (cophenetic) {
	fprintf (fp_out, "%i\n", size);
	for (i = 0; i < size; i++)
	    fprintf (fp_out, "%s\n", labels [i]);
	for (i = 1; i < size; i++)
	    for (j = 0; j < i; j++)
		fprintf (fp_out, "%g\n", diff_cp [i][j]);
    } else {

	if (sorted)
	    sortclus (2 * size - 2);
	for (i = size; i < 2 * size - 1; i++) {
	    fprintf (fp_out, "%i %g\n", i, cl [i].f);
	    if (cl [i].cl1 < size)
		fprintf (fp_out, "L %s\n", labels [cl [i].cl1]);
	    else
		fprintf (fp_out, "C %i\n", cl [i].cl1);
	    if (cl [i].cl2 < size)
		fprintf (fp_out, "L %s\n", labels [cl [i].cl2]);
	    else
		fprintf (fp_out, "C %i\n", cl [i].cl2);
	}

    }

    if (outfile)
	fclose (fp_out);

    return 0;
}

void checkCancel ()
{
    if (access ("_CANCEL_.L04", F_OK) == 0)
	errit ("CANCELLED");
}

void update_cp (float d, int p1, int p2)
{
    if (p1 >= size) {
	update_cp (d, cl [p1].cl1, p2);
	update_cp (d, cl [p1].cl2, p2);
    } else if (p2 >= size) {
	update_cp (d, p1, cl [p2].cl1);
	update_cp (d, p1, cl [p2].cl2);
    } else {
	if (p1 < p2)
	    diff_cp [p2][p1] += d;
	else
	    diff_cp [p1][p2] += d;
    }
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

/* Complete Link (Furthest Neighbor) */
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

/* Group Average (UPGMA) */
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

/* Weighted Average (WPGMA) */
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

/* Unweighted Centroid (Centroid, UPGMC) */
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

/* Weighted Centroid (Median, WPGMC) */
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

void process_args ()
{
    char
	*s;
    int
	i,
	i1,
	i2,
	ii;
    float
	f;
    while (arg_c > 1 && arg_v [1][0] == '-') {
        if ((! strcmp (arg_v [1], "-sl")) || ! strcmp (arg_v [1], "-n")) {
	    update_function = update_sl;
	    name_update =  a_sl;
        } else if (! strcmp (arg_v [1], "-cl")) {
	    update_function = update_cl;
	    name_update =  a_cl;
        } else if (! strcmp (arg_v [1], "-ga")) {
	    update_function = update_ga;
	    name_update =  a_ga;
        } else if (! strcmp (arg_v [1], "-wa")) {
	    update_function = update_wa;
	    name_update =  a_wa;
        } else if (! strcmp (arg_v [1], "-uc")) {
	    update_function = update_uc;
	    name_update =  a_uc;
        } else if (! strcmp (arg_v [1], "-wc")) {
	    update_function = update_wc;
	    name_update =  a_wc;
        } else if ((! strcmp (arg_v [1], "-wm")) || ! strcmp (arg_v [1], "-w")) {
	    update_function = update_wm;
	    name_update =  a_wm;
	} else if (arg_v [1][1] == 'm') {
	    s = get_arg ();
	    ii = 1;
	    buffer [0] = buffr2 [0] = buffr3 [0] = '\0';
	    i = sscanf (s, "%[0-9]-%[0-9]+%[0-9]", buffer, buffr2, buffr3);
	    if (i > 1) {
		i1 = atoi (buffer);
		i2 = atoi (buffr2);
		if (i1 < 2 || (i2 && i2 < i1))
		    errit ("Illegal range for -m: %s", s);
		if (!i2)
		    i2 = i1;
		if (i == 3)
		    ii = atoi (buffr3);
		if (ii < 1)
		    ii = 1;
	    } else {
		i1 = i2 = atoi (s);
		if (i1 < 2)
		    errit ("Illegal value for -m: %i", i1);		    
	    }
	    for (i = i1; i <= i2; i += ii) {
		if (n_maxcl == max_maxcl) {
		    max_maxcl += 64;
		    maxcl = (int *) s_realloc (maxcl, max_maxcl * sizeof (int));
		}
		maxcl [n_maxcl++] = i;
	    }
	} else if (arg_v [1][1] == 'N') {
	    f = atof (get_arg ());
	    if (f < 0.0)
		errit ("Illegal value for -N: %g", f);
	    if (n_noise == max_noise) {
		max_noise += 64;
		noise = (float *) s_realloc (noise, max_noise * sizeof (float));
	    }
	    noise [n_noise++] = f;
	} else if (arg_v [1][1] == 'r') {
	    n_runs = atoi (get_arg ());
	    if (n_runs < 2)
		errit ("Option -r : argument should be a number larger than 1");
	} else if (arg_v [1][1] == 's') {
	    seed = atoi (get_arg ());
	    if (seed < 1)
		errit ("Option -s : seed must be positive");
	} else if (arg_v [1][1] == 'o') {
	    outfile = get_arg ();
        } else if (! strcmp (arg_v [1], "-b")) {
	    cophenetic = 1;
	    binary = 1;
        } else if (! strcmp (arg_v [1], "-c")) {
	    cophenetic = 1;
	    binary = 0;
        } else if (! strcmp (arg_v [1], "-u")) {
	    sorted = 0;
	} else
	    errit ("Illegal option '%s'", arg_v [1]);

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


void syntax ()
{
    fprintf (
	stderr,
	"\nData Clustering, Version " my_VERSION "\n"
        "(c) P. Kleiweg 1998 - 2007\n"
        "\n"
	"Usage: %s -sl|-cl|-ga|-wa|-uc|-wc|-wm\n"
        "\t\t[-b] [-c] [-m int|int-int|int-int+int] [-N float]\n"
	"\t\t[-o filename] [-r int] [-s int] [-u] [difference table file]\n"
	"\n"
        "Clustering algorithm:\n"
        "\t-sl : %s, also: -n\n"
        "\t-cl : %s\n"
        "\t-ga : %s\n"
        "\t-wa : %s\n"
        "\t-uc : %s\n"
        "\t-wc : %s\n"
        "\t-wm : %s, also: -w\n\n"
	"Other options:\n"
	"\t-b  : binary difference output instead of clustering\n"
	"\t-c  : cophenetic difference output instead of clustering\n"
	"\t-m  : maximum number of clusters for binary or cophenetic output\n"
	"\t      defining a range is possible, e.g.: -m 2-8\n"
	"\t      or a range, e.g. like 2 5 8 11: -m 2-11+3\n"
	"\t-N  : noise\n"
	"\t-o  : output file\n"
	"\t-r  : number of runs (with -b or -c, and -N)\n"
	"\t-s  : seed for random number generator\n"
        "\t-u  : unsorted\n\n"
	"Options -m and -N can be used more than once\n\n",
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
