/**
 *  (C) 2002 MIT Laboratory for Computer Science.
 *
 *  Author: Alan Donovan <adonovan@lcs.mit.edu>
 *
 *  trace-untruncate-fast.c -- truncate file at last blank
 *  line ("\n\n"), and then append "// EOF\n"
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

int
main(int argc, char **argv)
{
    int			ii;
    const char	       *filename = NULL;
    FILE	       *fp;
    int			ret;

    progname = argv[0];

    for(ii=1; ii<argc; ++ii)
    {
	if(argv[ii][0] == '-') {
	    usage();
	}
	else 
	    filename = argv[ii];
    }

    if(filename == NULL)
	usage();
    
    if((fp = fopen(filename, "r+")) == NULL)
    {
        fprintf(stderr, "%s: can't open file `%s': %s.\n",
		progname, filename, strerror(errno));
        exit(1);
    }
    
    ret = fseek(fp, 0, SEEK_END); 
    if(ret != 0) { perror("fseek"); exit(1); }

    for(;;)
    {
	char buf[CHUNK];
	int count;

	ret = fseek(fp, -CHUNK*sizeof(char), SEEK_CUR);     
	if(ret != 0) { perror("fseek"); exit(1); }
	ret = fread(buf, CHUNK*sizeof(char), 1, fp); // at end of chunk
	if(ret == 0) { perror("fread"); exit(1); }

	for(count = CHUNK-1; count > 0; --count)
	    if((buf[count] == '\n') && (buf[count-1] == '\n'))
		break;
	
	if(count > 0)
	{
	    ret = fseek(fp, -(CHUNK-count)*sizeof(char), SEEK_CUR);
	    if(ret != 0) { perror("fseek"); exit(1); }
	    break;
	}
    }

    fputs("\n// EOF\n", fp);

    ret = ftruncate(fileno(fp), ftell(fp));
    if(ret != 0) { perror("fseek"); exit(1); }

    fclose(fp);

    return 0;
}

/*
 * Local Variables:
 * c-basic-offset:	4
 * compile-command:     gcc -Wall -g a.c -o truncate
 * End:
 */
