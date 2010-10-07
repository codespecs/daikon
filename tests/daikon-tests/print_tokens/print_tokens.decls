VarComparability
implicit

DECLARE
std.main(int;char **;)int:::ENTER
argc
int
int
1
argv
char *[]
hashcode
2

DECLARE
std.main(int;char **;)int:::EXIT1
argc
int
int
1
argv
char *[]
hashcode
2
return
int
int
3

DECLARE
std.open_character_stream(string;)character_stream:::ENTER
FILENAME
char[]
hashcode
4
FILENAME[]
char[]
java.lang.String
5[6]

DECLARE
std.open_character_stream(string;)character_stream:::EXIT2
FILENAME
char[]
hashcode
4
FILENAME[]
char[]
java.lang.String
5[6]

DECLARE
std.get_char(character_stream;)CHARACTER:::ENTER

DECLARE
std.get_char(character_stream;)CHARACTER:::EXIT3
return
CHARACTER
int
3

DECLARE
std.is_end_of_character_stream(character_stream;)BOOLEAN:::ENTER

DECLARE
std.is_end_of_character_stream(character_stream;)BOOLEAN:::EXIT4
return
BOOLEAN
int
3

DECLARE
std.is_end_of_character_stream(character_stream;)BOOLEAN:::EXIT5
return
BOOLEAN
int
3

DECLARE
std.unget_char(CHARACTER;character_stream;)void:::ENTER
ch
CHARACTER
int
7

DECLARE
std.unget_char(CHARACTER;character_stream;)void:::EXIT6
ch
CHARACTER
int
7

DECLARE
std.unget_char(CHARACTER;character_stream;)void:::EXIT7
ch
CHARACTER
int
7

DECLARE
std.open_token_stream(string;)token_stream:::ENTER
FILENAME
char[]
hashcode
4
FILENAME[]
char[]
java.lang.String
5[6]

DECLARE
std.open_token_stream(string;)token_stream:::EXIT8
FILENAME
char[]
hashcode
4
FILENAME[]
char[]
java.lang.String
5[6]

DECLARE
std.get_token(token_stream;)token:::ENTER

DECLARE
std.get_token(token_stream;)token:::EXIT9

DECLARE
std.get_token(token_stream;)token:::EXIT10

DECLARE
std.get_token(token_stream;)token:::EXIT11

DECLARE
std.get_token(token_stream;)token:::EXIT12

DECLARE
std.get_token(token_stream;)token:::EXIT13

DECLARE
std.get_token(token_stream;)token:::EXIT14

DECLARE
std.get_token(token_stream;)token:::EXIT15

DECLARE
std.numeric_case(token_stream;token;char;char *;int;)token:::ENTER
ch
char
int
7
token_str
char[]
hashcode
8
token_str[]
char[]
java.lang.String
9[10]
token_ind
int
int
11

DECLARE
std.numeric_case(token_stream;token;char;char *;int;)token:::EXIT16
ch
char
int
7
token_str
char[]
hashcode
8
token_str[]
char[]
java.lang.String
9[10]
token_ind
int
int
11

DECLARE
std.numeric_case(token_stream;token;char;char *;int;)token:::EXIT17
ch
char
int
7
token_str
char[]
hashcode
8
token_str[]
char[]
java.lang.String
9[10]
token_ind
int
int
11

DECLARE
std.error_or_eof_case(token_stream;token;int;char *;int;char;)token:::ENTER
cu_state
int
int
12
token_str
char[]
hashcode
8
token_str[]
char[]
java.lang.String
9[10]
token_ind
int
int
11
ch
char
int
7

DECLARE
std.error_or_eof_case(token_stream;token;int;char *;int;char;)token:::EXIT18
cu_state
int
int
12
token_str
char[]
hashcode
8
token_str[]
char[]
java.lang.String
9[10]
token_ind
int
int
11
ch
char
int
7

DECLARE
std.error_or_eof_case(token_stream;token;int;char *;int;char;)token:::EXIT19
cu_state
int
int
12
token_str
char[]
hashcode
8
token_str[]
char[]
java.lang.String
9[10]
token_ind
int
int
11
ch
char
int
7

DECLARE
std.check_delimiter(char;)int:::ENTER
ch
char
int
7

DECLARE
std.check_delimiter(char;)int:::EXIT20
ch
char
int
7
return
int
int
3

DECLARE
std.check_delimiter(char;)int:::EXIT21
ch
char
int
7
return
int
int
3

DECLARE
std.keyword(int;)int:::ENTER
state
int
int
13

DECLARE
std.keyword(int;)int:::EXIT22
state
int
int
13
return
int
int
3

DECLARE
std.keyword(int;)int:::EXIT23
state
int
int
13
return
int
int
3

DECLARE
std.keyword(int;)int:::EXIT24
state
int
int
13
return
int
int
3

DECLARE
std.keyword(int;)int:::EXIT25
state
int
int
13
return
int
int
3

DECLARE
std.keyword(int;)int:::EXIT26
state
int
int
13
return
int
int
3

DECLARE
std.keyword(int;)int:::EXIT27
state
int
int
13
return
int
int
3

DECLARE
std.special(int;)int:::ENTER
state
int
int
13

DECLARE
std.special(int;)int:::EXIT28
state
int
int
13
return
int
int
3

DECLARE
std.special(int;)int:::EXIT29
state
int
int
13
return
int
int
3

DECLARE
std.special(int;)int:::EXIT30
state
int
int
13
return
int
int
3

DECLARE
std.special(int;)int:::EXIT31
state
int
int
13
return
int
int
3

DECLARE
std.special(int;)int:::EXIT32
state
int
int
13
return
int
int
3

DECLARE
std.special(int;)int:::EXIT33
state
int
int
13
return
int
int
3

DECLARE
std.special(int;)int:::EXIT34
state
int
int
13
return
int
int
3

DECLARE
std.special(int;)int:::EXIT35
state
int
int
13
return
int
int
3

DECLARE
std.special(int;)int:::EXIT36
state
int
int
13
return
int
int
3

DECLARE
std.skip(character_stream;)int:::ENTER

DECLARE
std.skip(character_stream;)int:::EXIT37
return
int
int
3

DECLARE
std.constant(int;char *;int;)int:::ENTER
state
int
int
13
token_str
char[]
hashcode
8
token_str[]
char[]
java.lang.String
9[10]
token_ind
int
int
11

DECLARE
std.constant(int;char *;int;)int:::EXIT38
state
int
int
13
token_str
char[]
hashcode
8
token_str[]
char[]
java.lang.String
9[10]
token_ind
int
int
11
return
int
int
3

DECLARE
std.constant(int;char *;int;)int:::EXIT39
state
int
int
13
token_str
char[]
hashcode
8
token_str[]
char[]
java.lang.String
9[10]
token_ind
int
int
11
return
int
int
3

DECLARE
std.constant(int;char *;int;)int:::EXIT40
state
int
int
13
token_str
char[]
hashcode
8
token_str[]
char[]
java.lang.String
9[10]
token_ind
int
int
11
return
int
int
3

DECLARE
std.next_state(int;char;)int:::ENTER
state
int
int
13
ch
char
int
7

DECLARE
std.next_state(int;char;)int:::EXIT41
state
int
int
13
ch
char
int
7
return
int
int
3

DECLARE
std.next_state(int;char;)int:::EXIT42
state
int
int
13
ch
char
int
7
return
int
int
3

DECLARE
std.next_state(int;char;)int:::EXIT43
state
int
int
13
ch
char
int
7
return
int
int
3

DECLARE
std.next_state(int;char;)int:::EXIT44
state
int
int
13
ch
char
int
7
return
int
int
3

DECLARE
std.is_eof_token(token;)BOOLEAN:::ENTER

DECLARE
std.is_eof_token(token;)BOOLEAN:::EXIT45
return
BOOLEAN
int
3

DECLARE
std.is_eof_token(token;)BOOLEAN:::EXIT46
return
BOOLEAN
int
3

DECLARE
std.print_token(token;)BOOLEAN:::ENTER

DECLARE
std.print_token(token;)BOOLEAN:::EXIT47
return
BOOLEAN
int
3

DECLARE
std.print_token(token;)BOOLEAN:::EXIT48
return
BOOLEAN
int
3

DECLARE
std.print_token(token;)BOOLEAN:::EXIT49
return
BOOLEAN
int
3

DECLARE
std.print_token(token;)BOOLEAN:::EXIT50
return
BOOLEAN
int
3

DECLARE
std.print_token(token;)BOOLEAN:::EXIT51
return
BOOLEAN
int
3

DECLARE
std.print_token(token;)BOOLEAN:::EXIT52
return
BOOLEAN
int
3

DECLARE
std.print_token(token;)BOOLEAN:::EXIT53
return
BOOLEAN
int
3

DECLARE
std.print_token(token;)BOOLEAN:::EXIT54
return
BOOLEAN
int
3

DECLARE
std.print_token(token;)BOOLEAN:::EXIT55
return
BOOLEAN
int
3

DECLARE
std.print_token(token;)BOOLEAN:::EXIT56
return
BOOLEAN
int
3

DECLARE
std.print_token(token;)BOOLEAN:::EXIT57
return
BOOLEAN
int
3

DECLARE
std.print_token(token;)BOOLEAN:::EXIT58
return
BOOLEAN
int
3

DECLARE
std.print_token(token;)BOOLEAN:::EXIT59
return
BOOLEAN
int
3

DECLARE
std.print_token(token;)BOOLEAN:::EXIT60
return
BOOLEAN
int
3

DECLARE
std.print_token(token;)BOOLEAN:::EXIT61
return
BOOLEAN
int
3

DECLARE
std.print_token(token;)BOOLEAN:::EXIT62
return
BOOLEAN
int
3

DECLARE
std.print_token(token;)BOOLEAN:::EXIT63
return
BOOLEAN
int
3

DECLARE
std.print_token(token;)BOOLEAN:::EXIT64
return
BOOLEAN
int
3

DECLARE
std.print_token(token;)BOOLEAN:::EXIT65
return
BOOLEAN
int
3

DECLARE
std.print_token(token;)BOOLEAN:::EXIT66
return
BOOLEAN
int
3

DECLARE
std.get_actual_token(char *;int;)int:::ENTER
token_str
char[]
hashcode
8
token_str[]
char[]
java.lang.String
9[10]
token_ind
int
int
11

DECLARE
std.get_actual_token(char *;int;)int:::EXIT67
token_str
char[]
hashcode
8
token_str[]
char[]
java.lang.String
9[10]
token_ind
int
int
11
return
int
int
3

# Implicit Type to Explicit Type
#   1 : argc
#   2 : argv
#   3 : lh_return_value
#   4 : FILENAME
#   5 : FILENAME_element
#   6 : FILENAME_index
#   7 : ch
#   8 : token_str
#   9 : token_str_element
#  10 : token_str_index
#  11 : token_ind
#  12 : cu_state
#  13 : state
