/**
 *  (C) 2002 MIT Laboratory for Computer Science.
 *
 *  Author: Alan Donovan <adonovan@lcs.mit.edu>
 *
 *  trace-untruncate-fast.c -- remove the last (possibly partial)
 *  record from dtrace files whose names are given on the command
 *  line.  More specifically, the programs removes the last paragraph
 *  of a file (everything after the last double-newline), and does not
 *  create backups.
 *
 **/

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>

#define CHUNK 1024
#define countof(x) (sizeof(x)/sizeof(x[0]))

static const char *progname = NULL;
static void
usage()
{
    fprintf(stderr, "usage: %s <file>\n", progname);
    exit(1);
}

static int untrunc(const char *filename);

int
main(int argc, char **argv)
{
    int	ii;
    int nerrs = 0;

    progname = argv[0];

    if (argc < 2)
	usage();

    for (ii=1; ii<argc; ++ii)
    {
	if (argv[ii][0] == '-')
	    usage();
	else
	    if (untrunc(argv[ii]))
		fprintf(stderr, "%s: Could not untruncate %s!\n",
			progname, argv[ii]);

    }

    if (nerrs)
	fprintf(stderr, "%s: Could not process %d files!\n", progname, nerrs);

    return (nerrs != 0);
}

static int
untrunc(const char *filename)
{
    FILE	       *fp;
    int			ret;

    fprintf(stderr, "Untruncating %s...\n", filename);

    if ((fp = fopen(filename, "r+")) == NULL)
    {
        fprintf(stderr, "%s: can't open file `%s': %s.\n",
		progname, filename, strerror(errno));
        return 1;
    }

    ret = fseek(fp, 0, SEEK_END);
    if (ret != 0) { perror("fseek"); return 1; }

    for (;;)
    {
	char buf[CHUNK];
	int count;

	ret = fseek(fp, -CHUNK*sizeof(char), SEEK_CUR);
	if (ret != 0) { perror("fseek"); return 1; }
	ret = fread(buf, CHUNK*sizeof(char), 1, fp); // at end of chunk
	if (ret == 0) { perror("fread"); return 1; }

	for (count = CHUNK-1; count > 0; --count)
	    if ((buf[count] == '\n') && (buf[count-1] == '\n'))
		break;

	if (count > 0)
	{
	    ret = fseek(fp, -(CHUNK-count)*sizeof(char), SEEK_CUR);
	    if (ret != 0) { perror("fseek"); return 1; }
	    break;
	}
    }

    fputs("\n// EOF\n", fp);

    ret = ftruncate(fileno(fp), ftell(fp));
    if (ret != 0) { perror("fseek"); return 1; }

    fclose(fp);

    return 0;
}

/*
 * Local Variables:
 * c-basic-offset:	4
 * compile-command:     "gcc -Wall -g trace-untruncate-fast.c -o trace-untruncate-fast"
 * End:
 */
