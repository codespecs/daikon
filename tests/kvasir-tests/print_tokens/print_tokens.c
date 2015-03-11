

# include <ctype.h>

# define START  5
# define TRUE  1
# define FALSE 0

typedef int BOOLEAN;
typedef char *string;

# include <stdio.h>
# include "tokens.h"

static token numeric_case();
static token error_or_eof_case();
static int check_delimiter();
static int keyword(int state);
static int special(int state);
static skip(character_stream stream_ptr);
static int constant(int state,char token_str[],int token_ind);
static int next_state();
static get_actual_token(char token_str[],int token_ind);

main(argc,argv)
int argc;
char *argv[];
{
      token token_ptr;
      token_stream stream_ptr;

      if(argc>2)
      {
          fprintf(stdout, "The format is print_tokens filename(optional)\n");
          exit(1);
      }
      stream_ptr=open_token_stream(argv[1]);

      while(!is_eof_token((token_ptr=get_token(stream_ptr))))
                print_token(token_ptr);
      print_token(token_ptr);
  exit(0);
}



/* *********************************************************************
       Function name : open_character_stream
       Input         : filename 
       Output        : charactre stream.
       Exceptions    : If file name doesn't exists it will
                       exit from the program.
       Description   : The function first allocates the memory for 
                       the structure and initilizes it. The constant
                       START gives the first character available in
                       the stream. It ckecks whether the filename is
                       empty string. If it is it assigns file pointer
                       to stdin else it opens the respective file as input.                   * ******************************************************************* */

character_stream open_character_stream(FILENAME)
string FILENAME;
{
      character_stream stream_ptr;

      stream_ptr=(character_stream)malloc(sizeof(struct stream_type));
      stream_ptr->stream_ind=START;
      stream_ptr->stream[START]='\0';
      if(FILENAME == NULL)
          stream_ptr->fp=stdin;
      else if((stream_ptr->fp=fopen(FILENAME,"r"))==NULL)
           {
               fprintf(stdout, "The file %s doesn't exists\n",FILENAME);
               exit(0);
           }
      return(stream_ptr);
}

/* *********************************************************************
   Function name : get_char
   Input         : charcter_stream.
   Output        : character.
   Exceptions    : None.
   Description   : This function takes character_stream type variable 
                   as input and returns one character. If the stream is
                   empty then it reads the next line from the file and
                   returns the character.       
 * ****************************************************************** */

CHARACTER get_char(stream_ptr)
character_stream stream_ptr;
{
      if(stream_ptr->stream[stream_ptr->stream_ind] == '\0')
      {
              if(fgets(stream_ptr->stream+START,80-START,stream_ptr->fp) == NULL)/* Fix bug: add -START - hf*/
                    stream_ptr->stream[START]=EOF;
              stream_ptr->stream_ind=START;
      }
      return(stream_ptr->stream[(stream_ptr->stream_ind)++]);
}

/* *******************************************************************
   Function name : is_end_of_character_stream.
   Input         : character_stream.
   Output        : Boolean value.
   Description   : This function checks whether it is end of character
                   stream or not. It returns BOOLEANvariable which is 
                   true or false. The function checks whether the last 
                   read character is end file character or not and
                   returns the value according to it.
 * ****************************************************************** */

BOOLEAN is_end_of_character_stream(stream_ptr)
character_stream stream_ptr;
{
      if(stream_ptr->stream[stream_ptr->stream_ind-1] == EOF)
            return(TRUE);
      else
            return(FALSE);
}

/* *********************************************************************
   Function name : unget_char
   Input         : character,character_stream.
   Output        : void.
   Description   : This function adds the character ch to the stream. 
                   This is accomplished by decrementing the stream_ind
                   and storing it in the stream. If it is not possible
                   to unget the character then it returns
 * ******************************************************************* */

unget_char(ch,stream_ptr)
CHARACTER ch;
character_stream stream_ptr;
{
      if(stream_ptr->stream_ind == 0)
          return;
      else
          stream_ptr->stream[--(stream_ptr->stream_ind)]=ch;
      return;
}


/* *******************************************************************
   Function name : open_token_stream
   Input         : filename
   Output        : token_stream
   Exceptions    : Exits if the file specified by filename not found.
   Description   : This function takes filename as input and opens the
                   token_stream which is nothing but the character stream.
                   This function allocates the memory for token_stream 
                   and calls open_character_stream to open the file as
                   input. This function returns the token_stream.
 * ****************************************************************** */

token_stream open_token_stream(FILENAME)
string FILENAME;
{
    token_stream token_ptr;
  
    token_ptr=(token_stream)malloc(sizeof(struct token_stream_type));
    token_ptr->ch_stream=open_character_stream(FILENAME);/* Get character
                                                             stream  */
    return(token_ptr);
}

/* ********************************************************************
   Function name : get_token
   Input         : token_stream
   Output        : token
   Exceptions    : none.
   Description   : This function returns the next token from the
                   token_stream.The type of token is integer and specifies 
                   only the type of the token. DFA is used for finding the
                   next token. cu_state is initialized to zero and charcter
                   are read until the the is the final state and it
                   returns the token type.
* ******************************************************************* */

token get_token(tstream_ptr)
token_stream tstream_ptr;
{
      char token_str[80]; /* This buffer stores the current token */
      int token_ind;      /* Index to the token_str  */
      token token_ptr;
      CHARACTER ch;
      int cu_state,next_st,token_found;
  
      token_ptr=(token)(malloc(sizeof(struct token_type)));
      ch=get_char(tstream_ptr->ch_stream);
      cu_state=token_ind=token_found=0;
      while(!token_found)
      {
	  if(token_ind < 80) /* ADDED ERROR CHECK - hf */
	  {
	      token_str[token_ind++]=ch;
	      next_st=next_state(cu_state,ch);
	  }
	  else
	  {
	      next_st = -1; /* - hf */
	  }
	  if (next_st == -1) { /* ERROR or EOF case */
	      return(error_or_eof_case(tstream_ptr, 
				       token_ptr,cu_state,token_str,token_ind,ch));
	  } else if (next_st == -2) {/* This is numeric case. */
	      return(numeric_case(tstream_ptr,token_ptr,ch,
				  token_str,token_ind));
	  } else if (next_st == -3) {/* This is the IDENTIFIER case */
	      token_ptr->token_id=IDENTIFIER;
	      unget_char(ch,tstream_ptr->ch_stream);
	      token_ind--;
	      get_actual_token(token_str,token_ind);
	      strcpy(token_ptr->token_string,token_str);
	      return(token_ptr);
	  } 
	    
	  switch(next_st) 
            { 
                 default : break;
                 case 6  : /* These are all KEYWORD cases. */
                 case 9  :
                 case 11 :
                 case 13 :
                 case 16 : ch=get_char(tstream_ptr->ch_stream);
                           if(check_delimiter(ch)==TRUE)
                           {
                                 token_ptr->token_id=keyword(next_st);
                                 unget_char(ch,tstream_ptr->ch_stream);
                                 token_ptr->token_string[0]='\0';
                                 return(token_ptr);
                           }
                           unget_char(ch,tstream_ptr->ch_stream);
                           break;
                 case 19 : /* These are all special SPECIAL character */
                 case 20 : /* cases */
                 case 21 :
                 case 22 :
                 case 23 :
                 case 24 :
                 case 25 :
                 case 32 : token_ptr->token_id=special(next_st);
                           token_ptr->token_string[0]='\0';
                           return(token_ptr);
                 case 27 : /* These are constant cases */
                 case 29 : token_ptr->token_id=constant(next_st,token_str,token_ind);
                           get_actual_token(token_str,token_ind);
                           strcpy(token_ptr->token_string,token_str);
                           return(token_ptr);
                 case 30 :  /* This is COMMENT case */
                           skip(tstream_ptr->ch_stream);
                           token_ind=next_st=0;
                           break;
            }
            cu_state=next_st;
            ch=get_char(tstream_ptr->ch_stream);
      }
}

/* ******************************************************************
   Function name : numeric_case
   Input         : tstream_ptr,token_ptr,ch,token_str,token_ind
   Output        : token_ptr;
   Exceptions    : none 
   Description   : It checks for the delimiter, if it is then it
                   forms numeric token else forms error token.
 * ****************************************************************** */

static token numeric_case(tstream_ptr,token_ptr,ch,token_str,token_ind)
token_stream tstream_ptr;
token token_ptr;
char ch,token_str[];
int token_ind;
{
        if(check_delimiter(ch)!=TRUE)
        {   /* Error case */
            token_ptr->token_id=ERROR;
            while(check_delimiter(ch)==FALSE)
	    {
		if(token_ind >= 80) break; /* Added protection - hf */
		token_str[token_ind++]=ch=get_char(tstream_ptr->ch_stream);
	    }
            unget_char(ch,tstream_ptr->ch_stream);
            token_ind--;
            get_actual_token(token_str,token_ind);
            strcpy(token_ptr->token_string,token_str);
            return(token_ptr);
        }
        token_ptr->token_id=NUMERIC; /* Numeric case */
        unget_char(ch,tstream_ptr->ch_stream);
        token_ind--;
        get_actual_token(token_str,token_ind);
        strcpy(token_ptr->token_string,token_str);
        return(token_ptr);
}

/* *****************************************************************
   Function name : error_or_eof_case 
   Input         : tstream_ptr,token_ptr,cu_state,token_str,token_ind,ch
   Output        : token_ptr 
   Exceptions    : none 
   Description   : This function checks whether it is EOF or not.
                   If it is it returns EOF token else returns ERROR 
                   token.
 * *****************************************************************/

static token error_or_eof_case(tstream_ptr,token_ptr,cu_state,token_str,token_ind,ch)
token_stream tstream_ptr;
token token_ptr;
int cu_state,token_ind;
char token_str[],ch;
{
      if(is_end_of_character_stream(tstream_ptr->ch_stream)) 
      {
            token_ptr->token_id = EOTSTREAM;
            token_ptr->token_string[0]='\0';
            return(token_ptr);
      }
      if(cu_state !=0)
      {
            unget_char(ch,tstream_ptr->ch_stream);
            token_ind--;
      }
      token_ptr->token_id=ERROR;
      get_actual_token(token_str,token_ind);
      strcpy(token_ptr->token_string,token_str);
      return(token_ptr);                
}

/* *********************************************************************
   Function name : check_delimiter
   Input         : character
   Output        : boolean
   Exceptions    : none.
   Description   : This function checks for the delimiter. If ch is not
                   alphabet and non numeric then it returns TRUE else 
                   it returns FALSE. 
 * ******************************************************************* */

static int check_delimiter(ch)
char ch;
{
      if(!isalpha(ch) && !isdigit(ch)) /* Check for digit and alpha */
          return(TRUE);
      return(FALSE);
}

/* ********************************************************************
   Function name : keyword
   Input         : state of the DFA
   Output        : Keyword.
   Exceptions    : If the state doesn't represent a keyword it exits.
   Description   : According to the final state specified by state the
                   respective token_id is returned.
 * ***************************************************************** */

static int keyword(state)
int state;
{
      switch(state)
      {   /* Return the respective macro for the Keyword. */
          case 6 : return(LAMBDA);
          case 9 : return(AND);
          case 11: return(OR);
          case 13: return(IF);
          case 16: return(XOR);
          default: fprintf(stdout, "error\n");break;
      }
      exit(0);
}

/* ********************************************************************
   Function name : special
   Input         : The state of the DFA.
   Output        : special symbol.
   Exceptions    : if the state doesn't belong to a special character
                   it exits.
   Description   : This function returns the token_id according to the
                   final state given by state.
 * ****************************************************************** */

static int special(state)
int state;
{
     switch(state)
     {   /* return the respective macro for the special character. */
         case 19: return(LPAREN);
         case 20: return(RPAREN);
         case 21: return(LSQUARE);
         case 22: return(RSQUARE);
         case 23: return(QUOTE);
         case 24: return(BQUOTE);
         case 25: return(COMMA);
         case 32: return(EQUALGREATER);
         default: fprintf(stdout, "error\n");break;
     }
     exit(0);
}

/* **********************************************************************
   Function name : skip
   Input         : character_stream
   Output        : void.
   Exceptions    : none.
   Description   : This function skips the comment part of the program.
                   It takes charcter_stream as input and reads character
                   until it finds new line character or
                   end_of_character_stream.                   
 * ******************************************************************* */

static skip(stream_ptr)
character_stream stream_ptr;
{
        char c;
  
        while((c=get_char(stream_ptr))!='\n' && 
               !is_end_of_character_stream(stream_ptr))
             ; /* Skip the characters until EOF or EOL found. */
	if(c==EOF) unget_char(c, stream_ptr); /* Put back to leave gracefully - hf */
        return;
}

/* *********************************************************************
   Function name : constant
   Input         : state of DFA, Token string, Token id.
   Output        : constant token.
   Exceptions    : none.
   Description   : This function returns the token_id for the constatnts
                   speccified by  the final state. 
 * ****************************************************************** */

static int constant(state,token_str,token_ind)
int state,token_ind;
char token_str[];
{
     switch(state)
     {   /* Return the respective CONSTANT macro. */
         case 27 : return(STRING_CONSTANT);
         case 29 : token_str[token_ind-2]=' '; return(CHARACTER_CONSTANT);
         default : break;
     }
}


/* *******************************************************************
   Function name : next_state
   Input         : current state, character
   Output        : next state of the DFA
   Exceptions    : none.
   Description   : This function returns the next state in the transition
                   diagram. The next state is determined by the current
                   state state and the inpu character ch.
 * ****************************************************************** */
              
static int next_state(state,ch)
int state;
char ch;
{
    if(state < 0)
      return(state);
    if(base[state]+ch >= 0)
    {
        if(check[base[state]+ch] == state) /* Check for the right state */
             return(next[base[state]+ch]);
        else
              return(next_state(default1[state],ch));
    }
    else
        return(next_state(default1[state],ch));
}

/* *********************************************************************
   Function name : is_eof_token
   Input         : token
   Output        : Boolean
   Exceptions    : none.
   Description   : This function checks whether the token t is eof_token 
                   or not. If the integer value stored in the t is
                   EOTSTREAM then it is eof_token.
 * ***************************************************************** */

BOOLEAN is_eof_token(t)
token t;
{
    if(t->token_id==EOTSTREAM)
        return(TRUE);
    return(FALSE);
}

/* ********************************************************************
   Function name : print_token
   Input         : token
   Output        : Boolean
   Exceptions    : none.
   Description   : This function  prints the token. The token_id gives 
                   the type of token not the token itself. So, in the
                   case of identifier,numeric,  string,character it is
                   required to print the actual token  from token_str. 
                   So, precaution must be taken when printing the token.
                   This function is able to print the current token only
                   and it is the limitation of the program.
 * ******************************************************************** */

BOOLEAN print_token(token_ptr)
token token_ptr;
{
     switch(token_ptr->token_id)
     {    /* Print the respective tokens. */
          case ERROR : fprintf(stdout, "error,\t\"");fprintf(stdout, "%s",token_ptr->token_string);
                       fprintf(stdout, "\".\n");return(TRUE);
          case EOTSTREAM : fprintf(stdout, "eof.\n");return(TRUE);
          case 6 : fprintf(stdout, "keyword,\t\"lambda\".\n");return(TRUE);
          case 9 : fprintf(stdout, "keyword,\t\"and\".\n");return(TRUE);
          case 11: fprintf(stdout, "keyword,\t\"or\".\n");return(TRUE);
          case 13: fprintf(stdout, "keyword,\t\"if\".\n");return(TRUE);
          case 16: fprintf(stdout, "keyword,\t\"xor\".\n");return(TRUE);
          case 17: fprintf(stdout, "identifier,\t\"");fprintf(stdout, "%s",token_ptr->token_string);
                   fprintf(stdout, "\".\n");return(TRUE);
          case 18: fprintf(stdout, "numeric,\t");fprintf(stdout, "%s",token_ptr->token_string);
                   fprintf(stdout, ".\n");return(TRUE);
          case 19: fprintf(stdout, "lparen.\n");return(TRUE);
          case 20: fprintf(stdout, "rparen.\n");return(TRUE);
          case 21: fprintf(stdout, "lsquare.\n");return(TRUE);
          case 22: fprintf(stdout, "rsquare.\n");return(TRUE);
          case 23: fprintf(stdout, "quote.\n");return(TRUE);
          case 24: fprintf(stdout, "bquote.\n");return(TRUE);
          case 25: fprintf(stdout, "comma.\n");return(TRUE);
          case 27: fprintf(stdout, "string,\t");fprintf(stdout, "%s",token_ptr->token_string);
                   fprintf(stdout, ".\n");return(TRUE);
          case 29: fprintf(stdout, "character,\t\"");fprintf(stdout, "%s",token_ptr->token_string);
                   fprintf(stdout, "\".\n");return(TRUE);
          case 32: fprintf(stdout, "keyword,\t\"=>\".\n");return(TRUE);
          default: break;
      }
      return(FALSE);
}

/* **********************************************************************
   Function name : get_actual_token
   Input         : token string and token id.
   Output        : void.
   Exceptions    : none.
   Description   : This function prints the actual token in the case of
                   identifier,numeric,string and character. It removes
                   the leading and trailing  spaces and prints the token.
 * ****************************************************************** */

static get_actual_token(token_str,token_ind)
int token_ind;
char token_str[];
{
          int ind,start;

          for(ind=token_ind;ind>0 && isspace(token_str[ind-1]);--ind); 
                        /* Delete the trailing white spaces & protect - hf */
           token_str[ind]='\0';token_ind=ind;
          for(ind=0;ind<token_ind;++ind)
                if(!isspace(token_str[ind]))
                      break;
          for(start=0;ind<=token_ind;++start,++ind) /* Delete the leading
                                                       white spaces. */
                token_str[start]=token_str[ind];
          return;
}
