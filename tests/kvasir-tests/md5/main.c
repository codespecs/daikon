/*

  Calculate or Check MD5 Signature of File or Command Line Argument

			    by John Walker
		       http://www.fourmilab.ch/

		This program is in the public domain.

*/


#include <stdio.h>
#include <ctype.h>
#include <string.h>
#ifdef _WIN32
#include <fcntl.h>
#include <io.h>
#endif

#include "md5.h"

#define FALSE	0
#define TRUE	1

#define EOS     '\0'

#define sizeof_buffer 16384
#define sizeof_signature 16
#define sizeof_csig 16

/*  Main program  */



int main(int argc, char **argv)
{
    int i, j, cdata = FALSE, docheck = FALSE, f = 0;
    unsigned int bp;
    char *cp, *clabel, opt;
    FILE *in = stdin, *out = stdout;
    unsigned char buffer[sizeof_buffer],
      signature[sizeof_signature],
      csig[sizeof_csig];
    struct MD5Context md5c;

    MD5Init(&md5c);
    for (i = 1; i < argc; i++) {
	cp = argv[i];
        if (*cp == '-') {
	    opt = *(++cp);
	    if (islower(opt)) {
		opt = toupper(opt);
	    }

	    switch (opt) {

                case 'C':             /* -Csignature  --  Check signature, set return code */
		    docheck = TRUE;
		    if (strlen(cp + 1) != 32) {
			docheck = FALSE;
		    }
		    memset(csig, 0, 16);
		    clabel = cp + 1;
		    for (j = 0; j < 16; j++) {
			if (isxdigit(clabel[0]) && isxdigit(clabel[1]) &&
                            sscanf((cp + 1 + (j * 2)), "%02X", &bp) == 1) {
			    csig[j] = (unsigned char) bp;
			} else {
			    docheck = FALSE;
			    break;
			}
			clabel += 2;
		    }
		    if (!docheck) {
                        fprintf(stderr, "Error in signature specification.  Must be 32 hex digits.\n");
			return 2;
		    }
		    break;

                case 'D':             /* -Dtext  --  Compute signature of given text */
		    MD5Update(&md5c, (unsigned char *) (cp + 1), strlen(cp + 1));
		    cdata = TRUE;
		    f++;	      /* Mark no infile argument needed */
		    break;

                case '?':             /* -U, -? -H  --  Print how to call information. */
                case 'H':
                case 'U':
    fprintf(stderr,"\nMD5  --  Calculate MD5 signature of file.  Call");
    fprintf(stderr,
       "\n             with md5 [ options ] [input [output]]");
    fprintf(stderr,"\n");
    fprintf(stderr,"\n         Options:");
    fprintf(stderr,"\n              -csig   Check against sig, set exit status 0 = OK");
    fprintf(stderr,"\n              -dtext  Compute signature of text argument");
    fprintf(stderr,"\n              -u      Print this message");
    fprintf(stderr,"\n");
    fprintf(stderr,"\nby John Walker  --  http://www.fourmilab.ch/");
    fprintf(stderr,"\n");
		    return 0;
	    }
	} else {
	    switch (f) {
		case 0:
                    if (strcmp(cp, "-") != 0) {
                        if ((in = fopen(cp, "rb")) == NULL) {
                            fprintf(stderr, "Cannot open input file %s\n", cp);
			    return 2;
			}
		    }
		    f++;
		    break;

		case 1:
                    if (strcmp(cp, "-") != 0) {
                        if ((out = fopen(cp, "w")) == NULL) {
                            fprintf(stderr, "Cannot open output file %s\n", cp);
			    return 2;
			}
		    }
		    f++;
		    break;

		default:
                    fprintf(stderr, "Too many file names specified.\n");
		    return 2;
	    }
	}
    }

    if (!cdata) {
#ifdef _WIN32
	/** Warning!  On systems which distinguish text mode and
	    binary I/O (MS-DOS, Macintosh, etc.) the modes in the open
            statement for "in" should have forced the input file into
            binary mode.  But what if we're reading from standard
	    input?  Well, then we need to do a system-specific tweak
            to make sure it's in binary mode.  While we're at it,
            let's set the mode to binary regardless of however fopen
	    set it.

	    The following code, conditional on _WIN32, sets binary
	    mode using the method prescribed by Microsoft Visual C 5.0
            ("Monkey C"); this may require modification if you're
	    using a different compiler or release of Monkey C.	If
            you're porting this code to a different system which
            distinguishes text and binary files, you'll need to add
	    the equivalent call for that system. */

	setmode(fileno(in), O_BINARY);
#endif
	while ((i = fread(buffer, 1, sizeof_buffer, in)) > 0) {
	    MD5Update(&md5c, buffer, (unsigned) i);
	}
    }
    MD5Final(signature, &md5c);

    if (docheck) {
	docheck = 0;
	for (i = 0; i < sizeof_signature; i++) {
	    if (signature[i] != csig[i]) {
		docheck = 1;
		break;
	    }
	}
    } else {
	for (i = 0; i < sizeof_signature; i++) {
            fprintf(out, "%02X", signature[i]);
	}
        fprintf(out, "\n");
    }

    return docheck;
}
