/* ***************************************************************
    File name : stream.h
    PURPOSE   : This is the header file for stream.c . This inlcudes
                the type definitions which are to be exported to the
                other routines.
  * **************************************************************** */

typedef struct stream_type {
                               FILE *fp;  /* File pointer to stream */
                               int  stream_ind; /* buffer index */
                               char stream[80];  /* buffer for the file*/
                            } *character_stream;

typedef char CHARACTER;

character_stream open_character_stream();

CHARACTER get_char();

BOOLEAN is_end_of_character_stream();


