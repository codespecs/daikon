/*
 * file: denxview.c
 *
 * (c) P. Kleiweg 1998 - 2002
 *
 * compile: gcc -s -Wall -o denxview denxview.c -lm -lX11
 *
 */

/* Comment out the next line if you're using X11 older than X11R4 */
#define X11R4UP

#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <values.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#define MAX(A, B) ((A) > (B) ? (A) : (B))

char
    *fonts [3] = {
       NULL,          /* user specified font */
       "variable",    /* first fall back font, or default font */
       "fixed",       /* fall back font */
    };

typedef enum { CLS, LBL } NODETYPE;
typedef enum { FALSE = 0, TRUE = 1} BOOL;

typedef struct _cluster {
    int
        index;
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
    int
        x,
        y;
} POINT;

#define BUFSIZE 1024

int
    spanwidth,
    spanxwidth,
    xs = 2,            /* ruimte naast tekst */
    ys = 0,            /* ruimte tussen tekst regels */
    fontheight,
    minwidth,
    maxwidth,
    w_width,
    w_height,
    x1,
    x2,
    ytop,
    current_y,
    top,
    inputline = 0,
    max = 0,
    used = 0,
    mindefined = 0,
    maxlabelwidth = 0;

float
    step,
    maxvalue = -MAXFLOAT,
    minvalue = MAXFLOAT;

char
    buffer [BUFSIZE + 1],
    *programname,
    *no_mem_buffer,
    Out_of_memory [] = "Out of Memory!";

FILE
    *fp;

Display
    *display;
GC
    gc;
Window
    window,
    rootwindow;
int
    screen;
unsigned long
    black,
    white;
XFontStruct
    *font_struct;
XEvent
    event;
XComposeStatus
    composestatus;
KeySym
    keysym;
char
    keybuffer [2];
#ifdef X11R4UP
Atom
    wm_delete_window;
#endif

void
    set_sizes (void),
    show_it (void),
    trim (void),
    *s_malloc (size_t size),
    *s_realloc (void *block, size_t size),
    get_programname (char const *argv0),
    errit (char const *format, ...),
    warn (char const *format, ...),
    syntax (void),
    start_graphics (void),
    ConnectToServer (void),
    OpenWindow (void),
#ifdef X11R4UP
    SetProtocols (void),
#endif
    SetSizeHints (void),
    SetWMHints (void),
    SetClassHints (void),
    CreateGC (void),
    setlinestyle (int line_style);

POINT
    draw_node (int n);

char
    *s_strdup (char const *s);

int
    stringwidth (char const *s),
    getx (float x);

BOOL
    getline (BOOL required);

int main (int argc, char *argv [])
{
    int
        i,
        j,
        k,
        n,
        busy,
	needredraw,
	resized;
    float
        f;

    no_mem_buffer = (char *) malloc (1024);

    get_programname (argv [0]);

    while (argc > 1) {
        if (! strcmp (argv [1], "-b")) {
            argc--;
            argv++;
            if (argc == 1)
                errit ("Missing argument for option -b");
            minvalue = atof (argv [1]);
            mindefined = 1;
        } else if (! strcmp (argv [1], "-f")) {
            argc--;
            argv++;
            if (argc == 1)
                errit ("Missing argument for option -f");
            fonts [0] = argv [1];
        } else if (! strcmp (argv [1], "-w"))
	    errit ("Option -w obsolete, use resize with mouse instead");
        else
             break;
        argc--;
        argv++;
    }

    switch (argc) {
        case 1:
            if (isatty (fileno (stdin)))
                syntax ();
            fp = stdin;
            break;
	case 2:
            fp = fopen (argv [1], "r");
            if (! fp)
                errit ("Opening file \"%s\": %s", argv [1], strerror (errno));
            break;
	default:
            syntax ();
    }

    while (getline (FALSE)) {
        if (used == max) {
            max += 256;
            cl = (CLUSTER *) s_realloc (cl, max * sizeof (CLUSTER));
        }
        if (sscanf (buffer, "%i %f%n", &(cl [used].index), &(cl [used].value), &i) < 2)
            errit ("Syntax error at line %i: \"%s\"", inputline, buffer);
        if (cl [used].value > maxvalue)
            maxvalue = cl [used].value;
        memmove (buffer, buffer + i, strlen (buffer + i) + 1);
        trim ();
        if (buffer [0] && buffer [0] != '#') {
            cl [used].text = s_strdup (buffer);
        } else
            cl [used].text = NULL;
        for (n = 0; n < 2; n++) {
            getline (TRUE);
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

    ConnectToServer ();

    for (i = fonts [0] ? 0 : 1; i < 3; i++) {
        font_struct = XLoadQueryFont (display, fonts [i]);
        if (font_struct == (XFontStruct *) NULL)
            warn ("Font %s not found", fonts [i]);
        else
            break;
    }
    if (font_struct == (XFontStruct *) NULL)
        errit ("No fonts found");

    for (i = 0; i < used; i++)
        for (j = 0; j < 2; j++)
	    if (cl [i].node [j] == LBL) {
                k = stringwidth (cl [i].n [j].label);
                if (k > maxlabelwidth)
                    maxlabelwidth = k;
            }

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

    if (! mindefined) {
        for (i = 0; i < used; i++)
            if (cl [i].value < minvalue)
                minvalue = cl [i].value;
        if (minvalue > 0)
            minvalue = 0;
    }

    step = pow (10, ceil (log10 (maxvalue - minvalue)) - 1);
    if ((maxvalue - minvalue) / step > 6.0)
        step *= 2.0;
    else if ((maxvalue - minvalue) / step < 3.0)
        step *= 0.5;

    /* bereken maten voor window */

    spanwidth = 50;
    spanxwidth = spanwidth + xs;
    for (i = 0; i < used; i++)
        if (
            cl [i].text
         && (f = stringwidth (cl [i].text)
                 + ((float) spanwidth) /
                   (maxvalue - minvalue) *
                   (cl [i].value - minvalue)
                 + xs) > spanxwidth
        )
            spanxwidth = (int) f;
    minwidth = maxlabelwidth + spanxwidth + 3 * xs;
    maxwidth = DisplayWidth (display, screen);
    if (minwidth > maxwidth)
	errit ("No room, labels to large for screen");
    w_width = (minwidth + maxwidth) / 2;

    fontheight = font_struct->ascent + font_struct->descent;
    w_height = (fontheight + ys) * (used + 3) + 4 * ys + 2;
    x1 = maxlabelwidth + 2 * xs;
    ytop = fontheight / 2 + 2 * ys;

    start_graphics ();

    set_sizes ();

    show_it ();

    needredraw = resized = 0;
    for (busy = 1; busy; ) {
	do {
	    XNextEvent (display, &event);
	    switch (event.type) {
		case Expose:
		    needredraw = 1;
		    break;
		case ConfigureNotify:
		    w_width = event.xconfigure.width;
		    resized = 1;
		    needredraw = 1;
		    break;
		case KeyPress:
		    i = XLookupString (
		            &(event.xkey),
			    keybuffer,
			    1,
			    &keysym,
			    &composestatus
			);
		    if (i && keybuffer [0] == 'q')
			busy = 0;
		    break;
#ifdef X11R4UP
		case ClientMessage:
		    if (event.xclient.data.l [0] == wm_delete_window)
			busy = 0;
		    break;
#endif
	    }
	} while (XPending (display));
	if (resized) {
	    set_sizes ();
	    resized = 0;
	}
	if (needredraw) {
	    show_it ();
	    needredraw = 0;
	}
    }

    XFreeFont (display, font_struct);
    XCloseDisplay (display);

    return 0;
}

void set_sizes ()
{
    int
	i;

    spanxwidth = spanwidth = w_width - maxlabelwidth - 3 * xs;
    for (i = 0; i < used; i++)
        if (
            cl [i].text
         && stringwidth (cl [i].text)
                 + ((float) spanwidth) /
                   (maxvalue - minvalue) *
                   (cl [i].value - minvalue)
                 + xs > spanxwidth
        )
            spanwidth = ((float) (spanxwidth - stringwidth (cl [i].text) - xs))
		/ (cl [i].value - minvalue)
		* (maxvalue - minvalue);

    x2 = x1 + spanwidth;
}

int getx (float x)
{
    return (int) ( ((float) x1) +
                   ((float) spanwidth) / 
                   (maxvalue - minvalue) * 
                   (x - minvalue) );
}

void show_it ()
{
    float
        x;
    int
        xx;
    XClearWindow (display, window);
    current_y = ytop;
    draw_node (top);
    current_y += (fontheight + ys) / 2;
    XDrawLine (display, window, gc, x1, current_y, x2, current_y);
    for (x = 0; x <= maxvalue; x += (step / 2.0))
        if (x >= minvalue) {
            xx = getx (x);
            XDrawLine (display, window, gc, xx, current_y, xx, current_y - 3);
	}
    for (x = -step / 2.0; x >= minvalue; x -= (step / 2.0))
        if (x <= maxvalue) {
            xx = getx (x);
            XDrawLine (display, window, gc, xx, current_y, xx, current_y - 3);
	}
    for (x = 0; x <= maxvalue; x += step)
        if (x >= minvalue) {
            xx = getx (x);
            sprintf (buffer, "%g", x);
            XDrawString (
                display, window, gc,
                xx - stringwidth (buffer) / 2,
                current_y + 2 + ys + font_struct->ascent,
                buffer,
                strlen (buffer)
            );
	}
    for (x = -step; x >= minvalue; x -= step)
        if (x <= maxvalue) {
            xx = getx (x);
            sprintf (buffer, "%g", x);
            XDrawString (
                display, window, gc,
                xx - stringwidth (buffer) / 2,
                current_y + 2 + ys + font_struct->ascent,
                buffer,
                strlen (buffer)
            );
	}
    if (minvalue < 0.0 && maxvalue > 0.0) {
        setlinestyle (LineOnOffDash);
        XDrawLine (display, window, gc, getx (0), current_y, getx (0), ytop - 2);
        setlinestyle (LineSolid);
    }
    XFlush (display);
} 

POINT draw_node (int n)
{
    int
        i;
    POINT
        p [2],
        pp;
    for (i = 0; i < 2; i++) {
        if (cl [n].node [i] == LBL) {
            XDrawString (
                display,
                window,
                gc,
                x1 - xs - stringwidth (cl [n].n [i].label),
                current_y + fontheight / 2 - font_struct->descent,
                cl [n].n [i].label,
                strlen (cl [n].n [i].label)
            );
            p [i].x = x1;
            p [i].y = current_y;
            current_y += fontheight + ys;
	} else {
            p [i] = draw_node (cl [n].n [i].cluster);
	}
    }
    pp.x = getx (cl [n].value);
    XDrawLine (display, window, gc, p [0].x, p [0].y, pp.x, p [0].y);
    XDrawLine (display, window, gc, p [1].x, p [1].y, pp.x, p [1].y);
    XDrawLine (display, window, gc, pp.x, p [0].y, pp.x, p [1].y);
    pp.y = (p [0].y + p [1].y) / 2;
    if (cl [n].text)
        XDrawString (
            display,
            window,
            gc,
            pp.x + xs,
            MAX (p [0].y, p [1].y),
            cl [n].text,
            strlen (cl [n].text)
        );
    return pp;
}

int stringwidth (char const *s)
{
    return XTextWidth (font_struct, s, strlen (s));
}

BOOL getline (BOOL required)
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
    char
        *p;
    p = strrchr (argv0, '/');
    if (p)
        programname = strdup (p + 1);
    else
        programname = strdup (argv0);
}

void ConnectToServer ()
{
    display = XOpenDisplay (NULL);
    if (display == (Display *) NULL) {
        fprintf (
            stderr,
            "\nCannot connect to X server [%s]\n\n",
            XDisplayName (NULL)
        );
        exit (1);
    }
    screen = DefaultScreen (display);
    rootwindow = RootWindow (display, screen);
    black = BlackPixel (display, screen);
    white = WhitePixel (display, screen);
}

void OpenWindow ()
{
    XSetWindowAttributes
        attributes;

    attributes.event_mask = ExposureMask | KeyPressMask | StructureNotifyMask;
    attributes.border_pixel = black;
    attributes.background_pixel = white;
    window = XCreateWindow (
        display,           /* display */
        rootwindow,        /* parent */
        100, 100,          /* location */
        w_width, w_height,     /* size */
        2,                 /* borderwidth */
        CopyFromParent,    /* depth */
        InputOutput,       /* class */
        CopyFromParent,    /* visual */
        CWEventMask | CWBackPixel | CWBorderPixel,   /* valuemask */
        &attributes         /* attributes */
    );
}

#ifdef X11R4UP
void SetProtocols ()
{
    wm_delete_window = XInternAtom (display, "WM_DELETE_WINDOW", False);
    XSetWMProtocols (display, window, &wm_delete_window, 1);
}
#endif

void SetSizeHints ()
{
    XSizeHints
        hints;

    hints.flags = USSize | PMinSize | PMaxSize;
    hints.width = w_width;
    hints.height = w_height;
    hints.min_width = minwidth;
    hints.min_height = w_height;
    hints.max_width = maxwidth;
    hints.max_height = w_height;
#ifdef X11R4UP
    hints.base_width = w_width;
    hints.base_height = w_height;
    hints.flags |= PBaseSize;
    XSetWMNormalHints (display, window, &hints);
#else  /* up to X11R3  */
    XSetNormalHints (display, window, &hints);
#endif /* X11R4UP  */
}

void SetWMHints ()
{
    XWMHints
        hints;

    hints.flags = InputHint | StateHint;
    hints.initial_state = NormalState;
    hints.input = True;
    XSetWMHints (display, window, &hints);
}

void SetClassHints ()
{
    XClassHint
        hints;
    char
        *s;

    s = s_strdup (programname);
    s [0] = toupper (s [0]);
    hints.res_name = programname;
    hints.res_class = s;
    XSetClassHint (display, window, &hints);
    free (s);
}

void CreateGC ()
{
    XGCValues
        values;

    gc = XCreateGC (
        display,
        window,
        0L,           /* valuemask */
        &values
    );
}

void start_graphics ()
{
    OpenWindow ();
#ifdef X11R4UP
    SetProtocols ();
#endif
    SetSizeHints ();
    SetWMHints ();
    SetClassHints ();
    XStoreName (display, window, programname);
    XMapWindow (display, window);
    CreateGC ();
    XSetBackground (display, gc, white);
    XSetFont (display, gc, font_struct->fid);
    XSetDashes (display, gc, 0, "\003\003", 2);
    setlinestyle (LineSolid);
}

void setlinestyle (int line_style)
{
    XSetLineAttributes (display, gc, 1, line_style, CapProjecting, JoinMiter);
}

void syntax ()
{
    printf (
        "\nDendrogram Viewer for Unix/X11\n(c) P. Kleiweg 1998 - 2002\n"
	"\nUsage: %s [-b float] [-f font] [cluster file]\n\n"
        "\t-b : base offset, may be negative\n"
        "\t     (default minimum of 0.0 and smallest value in cluster file)\n"
        "\t-f : X11 font name\n\n"
        "Type 'q' to quit\n\n",
	programname
    );
    puts (
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
        "C 3\n"
    );
    exit (0);
}
