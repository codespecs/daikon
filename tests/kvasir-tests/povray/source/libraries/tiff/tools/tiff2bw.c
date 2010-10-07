/* $Header: /cvsroot/osrs/libtiff/tools/tiff2bw.c,v 1.5 2003/03/12 14:05:06 dron Exp $ */

/*
 * Copyright (c) 1988-1997 Sam Leffler
 * Copyright (c) 1991-1997 Silicon Graphics, Inc.
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
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "tiffio.h"

#define	streq(a,b)	(strcmp((a),(b)) == 0)
#define	strneq(a,b,n)	(strncmp(a,b,n) == 0)

/* x% weighting -> fraction of full color */
#define	PCT(x)	(((x)*255+127)/100)
int	RED = PCT(30);		/* 30% */
int	GREEN = PCT(59);	/* 59% */
int	BLUE = PCT(11);		/* 11% */

static	void usage(void);
static	int processCompressOptions(char*);

static void
compresscontig(unsigned char* out, unsigned char* rgb, uint32 n)
{
	register int v, red = RED, green = GREEN, blue = BLUE;

	while (n-- > 0) {
		v = red*(*rgb++);
		v += green*(*rgb++);
		v += blue*(*rgb++);
		*out++ = v>>8;
	}
}

static void
compresssep(unsigned char* out,
    unsigned char* r, unsigned char* g, unsigned char* b, uint32 n)
{
	register uint32 red = RED, green = GREEN, blue = BLUE;

	while (n-- > 0)
		*out++ = (red*(*r++) + green*(*g++) + blue*(*b++)) >> 8;
}

static int
checkcmap(TIFF* tif, int n, uint16* r, uint16* g, uint16* b)
{
	while (n-- > 0)
		if (*r++ >= 256 || *g++ >= 256 || *b++ >= 256)
			return (16);
	TIFFWarning(TIFFFileName(tif), "Assuming 8-bit colormap");
	return (8);
}

static void
compresspalette(unsigned char* out, unsigned char* data, uint32 n, uint16* rmap, uint16* gmap, uint16* bmap)
{
	register int v, red = RED, green = GREEN, blue = BLUE;

	while (n-- > 0) {
		unsigned int ix = *data++;
		v = red*rmap[ix];
		v += green*gmap[ix];
		v += blue*bmap[ix];
		*out++ = v>>8;
	}
}

static	uint16 compression = (uint16) -1;
static	uint16 predictor = 0;
static	int jpegcolormode = JPEGCOLORMODE_RGB;
static	int quality = 75;		/* JPEG quality */

static	void cpTags(TIFF* in, TIFF* out);

int
main(int argc, char* argv[])
{
	uint32 rowsperstrip = (uint32) -1;
	TIFF *in, *out;
	uint32 w, h;
	uint16 samplesperpixel;
	uint16 bitspersample;
	uint16 config;
	uint16 photometric;
	uint16* red;
	uint16* green;
	uint16* blue;
	tsize_t rowsize;
	register uint32 row;
	register tsample_t s;
	unsigned char *inbuf, *outbuf;
	char thing[1024];
	int c;
	extern int optind;
	extern char *optarg;

	while ((c = getopt(argc, argv, "c:r:R:G:B:")) != -1)
		switch (c) {
		case 'c':		/* compression scheme */
			if (!processCompressOptions(optarg))
				usage();
			break;
		case 'r':		/* rows/strip */
			rowsperstrip = atoi(optarg);
			break;
		case 'R':
			RED = PCT(atoi(optarg));
			break;
		case 'G':
			GREEN = PCT(atoi(optarg));
			break;
		case 'B':
			BLUE = PCT(atoi(optarg));
			break;
		case '?':
			usage();
			/*NOTREACHED*/
		}
	if (argc - optind < 2)
		usage();
	in = TIFFOpen(argv[optind], "r");
	if (in == NULL)
		return (-1);
	photometric = 0;
	TIFFGetField(in, TIFFTAG_PHOTOMETRIC, &photometric);
	if (photometric != PHOTOMETRIC_RGB && photometric != PHOTOMETRIC_PALETTE ) {
		fprintf(stderr,
	    "%s: Bad photometric; can only handle RGB and Palette images.\n",
		    argv[optind]);
		return (-1);
	}
	TIFFGetField(in, TIFFTAG_SAMPLESPERPIXEL, &samplesperpixel);
	if (samplesperpixel != 1 && samplesperpixel != 3) {
		fprintf(stderr, "%s: Bad samples/pixel %u.\n",
		    argv[optind], samplesperpixel);
		return (-1);
	}
	TIFFGetField(in, TIFFTAG_BITSPERSAMPLE, &bitspersample);
	if (bitspersample != 8) {
		fprintf(stderr,
		    " %s: Sorry, only handle 8-bit samples.\n", argv[optind]);
		return (-1);
	}
	TIFFGetField(in, TIFFTAG_IMAGEWIDTH, &w);
	TIFFGetField(in, TIFFTAG_IMAGELENGTH, &h);
	TIFFGetField(in, TIFFTAG_PLANARCONFIG, &config);

	out = TIFFOpen(argv[optind+1], "w");
	if (out == NULL)
		return (-1);
	cpTags(in, out);
	TIFFSetField(out, TIFFTAG_BITSPERSAMPLE, 8);
	TIFFSetField(out, TIFFTAG_SAMPLESPERPIXEL, 1);
	TIFFSetField(out, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
	if (compression != (uint16) -1) {
		TIFFSetField(out, TIFFTAG_COMPRESSION, compression);
		switch (compression) {
		case COMPRESSION_JPEG:
			TIFFSetField(out, TIFFTAG_JPEGQUALITY, quality);
			TIFFSetField(out, TIFFTAG_JPEGCOLORMODE, jpegcolormode);
			break;
		case COMPRESSION_LZW:
		case COMPRESSION_DEFLATE:
			if (predictor != 0)
				TIFFSetField(out, TIFFTAG_PREDICTOR, predictor);
			break;
		}
	}
	TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
	sprintf(thing, "B&W version of %s", argv[optind]);
	TIFFSetField(out, TIFFTAG_IMAGEDESCRIPTION, thing);
	TIFFSetField(out, TIFFTAG_SOFTWARE, "tiff2bw");
	outbuf = (unsigned char *)_TIFFmalloc(TIFFScanlineSize(out));
	TIFFSetField(out, TIFFTAG_ROWSPERSTRIP,
	    TIFFDefaultStripSize(out, rowsperstrip));

#define	pack(a,b)	((a)<<8 | (b))
	switch (pack(photometric, config)) {
	case pack(PHOTOMETRIC_PALETTE, PLANARCONFIG_CONTIG):
	case pack(PHOTOMETRIC_PALETTE, PLANARCONFIG_SEPARATE):
		TIFFGetField(in, TIFFTAG_COLORMAP, &red, &green, &blue);
		/*
		 * Convert 16-bit colormap to 8-bit (unless it looks
		 * like an old-style 8-bit colormap).
		 */
		if (checkcmap(in, 1<<bitspersample, red, green, blue) == 16) {
			int i;
#define	CVT(x)		(((x) * 255L) / ((1L<<16)-1))
			for (i = (1<<bitspersample)-1; i >= 0; i--) {
				red[i] = CVT(red[i]);
				green[i] = CVT(green[i]);
				blue[i] = CVT(blue[i]);
			}
#undef CVT
		}
		inbuf = (unsigned char *)_TIFFmalloc(TIFFScanlineSize(in));
		for (row = 0; row < h; row++) {
			if (TIFFReadScanline(in, inbuf, row, 0) < 0)
				break;
			compresspalette(outbuf, inbuf, w, red, green, blue);
			if (TIFFWriteScanline(out, outbuf, row, 0) < 0)
				break;
		}
		break;
	case pack(PHOTOMETRIC_RGB, PLANARCONFIG_CONTIG):
		inbuf = (unsigned char *)_TIFFmalloc(TIFFScanlineSize(in));
		for (row = 0; row < h; row++) {
			if (TIFFReadScanline(in, inbuf, row, 0) < 0)
				break;
			compresscontig(outbuf, inbuf, w);
			if (TIFFWriteScanline(out, outbuf, row, 0) < 0)
				break;
		}
		break;
	case pack(PHOTOMETRIC_RGB, PLANARCONFIG_SEPARATE):
		rowsize = TIFFScanlineSize(in);
		inbuf = (unsigned char *)_TIFFmalloc(3*rowsize);
		for (row = 0; row < h; row++) {
			for (s = 0; s < 3; s++)
				if (TIFFReadScanline(in,
				    inbuf+s*rowsize, row, s) < 0)
					 return (-1);
			compresssep(outbuf,
			    inbuf, inbuf+rowsize, inbuf+2*rowsize, w);
			if (TIFFWriteScanline(out, outbuf, row, 0) < 0)
				break;
		}
		break;
	}
#undef pack
	TIFFClose(out);
	return (0);
}

static int
processCompressOptions(char* opt)
{
	if (streq(opt, "none"))
		compression = COMPRESSION_NONE;
	else if (streq(opt, "packbits"))
		compression = COMPRESSION_PACKBITS;
	else if (strneq(opt, "jpeg", 4)) {
		char* cp = strchr(opt, ':');
		if (cp && isdigit(cp[1]))
			quality = atoi(cp+1);
		if (cp && strchr(cp, 'r'))
			jpegcolormode = JPEGCOLORMODE_RAW;
		compression = COMPRESSION_JPEG;
	} else if (strneq(opt, "lzw", 3)) {
		char* cp = strchr(opt, ':');
		if (cp)
			predictor = atoi(cp+1);
		compression = COMPRESSION_LZW;
	} else if (strneq(opt, "zip", 3)) {
		char* cp = strchr(opt, ':');
		if (cp)
			predictor = atoi(cp+1);
		compression = COMPRESSION_DEFLATE;
	} else
		return (0);
	return (1);
}

#define	CopyField1(tag, v) \
    if (TIFFGetField(in, tag, &v)) TIFFSetField(out, tag, v)
#define	CopyField2(tag, v1, v2) \
    if (TIFFGetField(in, tag, &v1, &v2)) TIFFSetField(out, tag, v1, v2)
#define	CopyField3(tag, v1, v2, v3) \
    if (TIFFGetField(in, tag, &v1, &v2, &v3)) TIFFSetField(out, tag, v1, v2, v3)
#define	CopyField4(tag, v1, v2, v3, v4) \
    if (TIFFGetField(in, tag, &v1, &v2, &v3, &v4)) TIFFSetField(out, tag, v1, v2, v3, v4)

static void
cpTag(TIFF* in, TIFF* out, uint16 tag, uint16 count, TIFFDataType type)
{
    uint16 shortv, shortv2, *shortav;
    float floatv, *floatav;
    char *stringv;
    uint32 longv;

    switch (type) {
    case TIFF_SHORT:
	if (count == 1) {
	    CopyField1(tag, shortv);
	} else if (count == 2) {
	    CopyField2(tag, shortv, shortv2);
	} else if (count == (uint16) -1) {
	    CopyField2(tag, shortv, shortav);
	}
	break;
    case TIFF_LONG:
	CopyField1(tag, longv);
	break;
    case TIFF_RATIONAL:
	if (count == 1) {
	    CopyField1(tag, floatv);
	} else if (count == (uint16) -1) {
	    CopyField1(tag, floatav);
	}
	break;
    case TIFF_ASCII:
	CopyField1(tag, stringv);
	break;
    }
}
#undef CopyField4
#undef CopyField3
#undef CopyField2
#undef CopyField1

static struct cpTag {
    uint16	tag;
    uint16	count;
    TIFFDataType type;
} tags[] = {
    { TIFFTAG_IMAGEWIDTH,		1, TIFF_LONG },
    { TIFFTAG_IMAGELENGTH,		1, TIFF_LONG },
    { TIFFTAG_FILLORDER,		1, TIFF_SHORT },
    { TIFFTAG_DOCUMENTNAME,		1, TIFF_ASCII },
    { TIFFTAG_MAKE,			1, TIFF_ASCII },
    { TIFFTAG_MODEL,			1, TIFF_ASCII },
    { TIFFTAG_ORIENTATION,		1, TIFF_SHORT },
    { TIFFTAG_XRESOLUTION,		1, TIFF_RATIONAL },
    { TIFFTAG_YRESOLUTION,		1, TIFF_RATIONAL },
    { TIFFTAG_PAGENAME,			1, TIFF_ASCII },
    { TIFFTAG_XPOSITION,		1, TIFF_RATIONAL },
    { TIFFTAG_YPOSITION,		1, TIFF_RATIONAL },
    { TIFFTAG_RESOLUTIONUNIT,		1, TIFF_SHORT },
    { TIFFTAG_PAGENUMBER,		2, TIFF_SHORT },
    { TIFFTAG_ARTIST,			1, TIFF_ASCII },
    { TIFFTAG_HOSTCOMPUTER,		1, TIFF_ASCII },
};
#define	NTAGS	(sizeof (tags) / sizeof (tags[0]))

static void
cpTags(TIFF* in, TIFF* out)
{
    struct cpTag *p;
    for (p = tags; p < &tags[NTAGS]; p++)
	cpTag(in, out, p->tag, p->count, p->type);
}
#undef NTAGS

char* stuff[] = {
"usage: tiff2bw [options] input.tif output.tif",
"where options are:",
" -R %		use #% from red channel",
" -G %		use #% from green channel",
" -B %		use #% from blue channel",
"",
" -r #		make each strip have no more than # rows",
"",
" -c lzw[:opts]	compress output with Lempel-Ziv & Welch encoding",
"               (no longer supported by default due to Unisys patent enforcement)", 
" -c zip[:opts]	compress output with deflate encoding",
" -c packbits	compress output with packbits encoding",
" -c g3[:opts]	compress output with CCITT Group 3 encoding",
" -c g4		compress output with CCITT Group 4 encoding",
" -c none	use no compression algorithm on output",
"",
"LZW and deflate options:",
" #		set predictor value",
"For example, -c lzw:2 to get LZW-encoded data with horizontal differencing",
NULL
};

static void
usage(void)
{
	char buf[BUFSIZ];
	int i;

	setbuf(stderr, buf);
        fprintf(stderr, "%s\n\n", TIFFGetVersion());
	for (i = 0; stuff[i] != NULL; i++)
		fprintf(stderr, "%s\n", stuff[i]);
	exit(-1);
}
