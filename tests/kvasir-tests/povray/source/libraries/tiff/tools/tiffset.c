/******************************************************************************
 * $Id: tiffset.c,v 1.3 2002/01/16 17:50:05 warmerda Exp $
 *
 * Project:  libtiff tools
 * Purpose:  Mainline for setting metadata in existing TIFF files.
 * Author:   Frank Warmerdam, warmerda@home.com
 *
 ******************************************************************************
 * Copyright (c) 2000, Frank Warmerdam
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Sam Leffler and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Sam Leffler and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 * 
 * IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 ******************************************************************************
 *
 * $Log: tiffset.c,v $
 * Revision 1.3  2002/01/16 17:50:05  warmerda
 * Fix bug in error output.
 *
 * Revision 1.2  2001/09/26 17:42:18  warmerda
 * added TIFFRewriteDirectory
 *
 * Revision 1.1  2001/03/02 04:58:53  warmerda
 * New
 *
 */


#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "tiffiop.h"

static ttag_t field_name_to_id( const char * );

static char* usageMsg[] = {
    "usage: tiffset [-s name value] filename\n",
    NULL
};

static void
usage(void)
{
	int i;
	for (i = 0; usageMsg[i]; i++)
		fprintf(stderr, "%s", usageMsg[i]);
	exit(-1);
}

int
main(int argc, char* argv[])
{
    TIFF *tiff;
    int  arg_index;

    if (argc < 2)
        usage();

    tiff = TIFFOpen(argv[argc-1], "r+");
    if (tiff == NULL)
        return (-2);

    for( arg_index = 1; arg_index < argc-1; arg_index++ )
    {
        if( strcmp(argv[arg_index],"-s") == 0 && arg_index < argc-3 )
        {
            ttag_t  id;

            if( atoi(argv[arg_index+1]) > 0 )
                id = atoi(argv[arg_index+1]);
            else
                id = field_name_to_id(argv[arg_index+1]);

            if( id < 1 )
            {
                fprintf( stderr, "Field name %s not recognised.\n",
                         argv[arg_index+1] );
                exit( -3 );
            }

            if( TIFFSetField( tiff, id, argv[arg_index+2] ) != 1 )
            {
                fprintf( stderr, "Failed to set %s=%s\n", 
                         argv[arg_index+1], 
                         argv[arg_index+2] );
            }
            arg_index += 2;
        }
        else if( strcmp(argv[arg_index],"-sf") == 0 && arg_index < argc-3 )
        {
            ttag_t  id;
            FILE    *fp;
            char    *text;
            int     len;

            if( atoi(argv[arg_index+1]) > 0 )
                id = atoi(argv[arg_index+1]);
            else
                id = field_name_to_id(argv[arg_index+1]);

            if( id < 1 )
            {
                fprintf( stderr, "Field name %s not recognised.\n",
                         argv[arg_index+1] );
                exit( -3 );
            }

            fp = fopen( argv[arg_index+2], "rt" );
            if( fp == NULL )
            {
                perror( argv[arg_index+2] );
                continue;
            }

            text = (char *) malloc(66000);
            len = fread( text, 1, 65535, fp );
            text[len] = '\0';

            fclose( fp );

            if( TIFFSetField( tiff, id, text ) != 1 )
            {
                fprintf( stderr, "Failed to set %s=%s\n", 
                         argv[arg_index+1], 
                         argv[arg_index+2] );
            }
            free( text );
            arg_index += 2;
        }
        else
        {
            fprintf( stderr, "Unrecognised option: %s\n",
                     argv[arg_index] );
            usage();
        }
    }

#ifdef notdef
    tiff->tif_header.tiff_diroff = 0;
    tiff->tif_diroff = 0;
#endif

    TIFFRewriteDirectory(tiff);
    TIFFClose(tiff);
    return (0);
}

static ttag_t field_name_to_id( const char * name )

{
    if( strstr(name, "DESCRIPTION") != NULL 
        || strstr(name, "description") != NULL )
        return TIFFTAG_IMAGEDESCRIPTION;

    else if( strstr(name, "SOFTWARE") != NULL 
        || strstr(name, "software") != NULL )
        return TIFFTAG_SOFTWARE;

    else if( strstr(name, "COPYRIGHT") != NULL 
        || strstr(name, "copyright") != NULL )
        return TIFFTAG_COPYRIGHT;

    else
        return -1;
}
