VarComparability
implicit

DECLARE
std.getline(char *;int;)boolean:::ENTER
s
char[]
hashcode
1
s[]
char[]
java.lang.String
2[3]
maxsize
int
int
4

DECLARE
std.getline(char *;int;)boolean:::EXIT1
s
char[]
hashcode
1
s[]
char[]
java.lang.String
2[3]
maxsize
int
int
4
return
boolean
int
5

DECLARE
std.addstr(char;char *;int *;int;)int:::ENTER
c
char
int
6
outset
char[]
hashcode
7
outset[]
char[]
java.lang.String
6[6]
j
int[]
hashcode
8
*j
int
int
6
maxset
int
int
6

DECLARE
std.addstr(char;char *;int *;int;)int:::EXIT2
c
char
int
6
outset
char[]
hashcode
7
outset[]
char[]
java.lang.String
6[6]
j
int[]
hashcode
8
*j
int
int
6
maxset
int
int
6
return
int
int
5

DECLARE
std.esc(char *;int *;)char:::ENTER
s
char[]
hashcode
1
s[]
char[]
java.lang.String
2[9]
i
int[]
hashcode
10
*i
int
int
9

DECLARE
std.esc(char *;int *;)char:::EXIT3
s
char[]
hashcode
1
s[]
char[]
java.lang.String
11[9]
i
int[]
hashcode
10
*i
int
int
9
return
char
int
11

DECLARE
std.dodash(char;char *;int *;char *;int *;int;)void:::ENTER
delim
char
int
12
src
char[]
hashcode
13
src[]
char[]
java.lang.String
12[14]
i
int[]
hashcode
10
*i
int
int
14
dest
char[]
hashcode
15
dest[]
char[]
java.lang.String
12[12]
j
int[]
hashcode
8
*j
int
int
12
maxset
int
int
12

DECLARE
std.dodash(char;char *;int *;char *;int *;int;)void:::EXIT4
delim
char
int
12
src
char[]
hashcode
13
src[]
char[]
java.lang.String
12[14]
i
int[]
hashcode
10
*i
int
int
14
dest
char[]
hashcode
15
dest[]
char[]
java.lang.String
12[12]
j
int[]
hashcode
8
*j
int
int
12
maxset
int
int
12

DECLARE
std.getccl(char *;int *;char *;int *;)boolean:::ENTER
arg
char[]
hashcode
16
arg[]
char[]
java.lang.String
17[18]
i
int[]
hashcode
10
*i
int
int
18
pat
char[]
hashcode
19
pat[]
char[]
java.lang.String
17[17]
j
int[]
hashcode
8
*j
int
int
17

DECLARE
std.getccl(char *;int *;char *;int *;)boolean:::EXIT5
arg
char[]
hashcode
16
arg[]
char[]
java.lang.String
17[18]
i
int[]
hashcode
10
*i
int
int
18
pat
char[]
hashcode
19
pat[]
char[]
java.lang.String
17[17]
j
int[]
hashcode
8
*j
int
int
17
return
boolean
int
5

DECLARE
std.stclose(char *;int *;int;)void:::ENTER
pat
char[]
hashcode
19
pat[]
char[]
java.lang.String
20[20]
j
int[]
hashcode
8
*j
int
int
20
lastj
int
int
20

DECLARE
std.stclose(char *;int *;int;)void:::EXIT6
pat
char[]
hashcode
19
pat[]
char[]
java.lang.String
20[20]
j
int[]
hashcode
8
*j
int
int
20
lastj
int
int
20

DECLARE
std.in_set_2(char;)boolean:::ENTER
c
char
int
21

DECLARE
std.in_set_2(char;)boolean:::EXIT7
c
char
int
21
return
boolean
int
5

DECLARE
std.in_pat_set(char;)boolean:::ENTER
c
char
int
21

DECLARE
std.in_pat_set(char;)boolean:::EXIT8
c
char
int
21
return
boolean
int
5

DECLARE
std.makepat(char *;int;char;char *;)int:::ENTER
arg
char[]
hashcode
16
arg[]
char[]
java.lang.String
22[23]
start
int
int
23
delim
char
int
22
pat
char[]
hashcode
19
pat[]
char[]
java.lang.String
22[22]

DECLARE
std.makepat(char *;int;char;char *;)int:::EXIT9
arg
char[]
hashcode
16
arg[]
char[]
java.lang.String
22[24]
start
int
int
24
delim
char
int
22
pat
char[]
hashcode
19
pat[]
char[]
java.lang.String
22[22]
return
int
int
24

DECLARE
std.getpat(char *;char *;)int:::ENTER
arg
char[]
hashcode
16
arg[]
char[]
java.lang.String
25[26]
pat
char[]
hashcode
19
pat[]
char[]
java.lang.String
25[25]

DECLARE
std.getpat(char *;char *;)int:::EXIT10
arg
char[]
hashcode
16
arg[]
char[]
java.lang.String
25[26]
pat
char[]
hashcode
19
pat[]
char[]
java.lang.String
25[25]
return
int
int
5

DECLARE
std.makesub(char *;int;character;char *;)int:::ENTER
arg
char[]
hashcode
16
arg[]
char[]
java.lang.String
27[28]
from
int
int
28
delim
character
int
27
sub
char[]
hashcode
29
sub[]
char[]
java.lang.String
27[30]

DECLARE
std.makesub(char *;int;character;char *;)int:::EXIT11
arg
char[]
hashcode
16
arg[]
char[]
java.lang.String
27[31]
from
int
int
31
delim
character
int
27
sub
char[]
hashcode
29
sub[]
char[]
java.lang.String
27[30]
return
int
int
31

DECLARE
std.getsub(char *;char *;)boolean:::ENTER
arg
char[]
hashcode
16
arg[]
char[]
java.lang.String
32[26]
sub
char[]
hashcode
29
sub[]
char[]
java.lang.String
32[30]

DECLARE
std.getsub(char *;char *;)boolean:::EXIT12
arg
char[]
hashcode
16
arg[]
char[]
java.lang.String
32[26]
sub
char[]
hashcode
29
sub[]
char[]
java.lang.String
32[30]
return
boolean
int
5

DECLARE
std.locate(character;char *;int;)boolean:::ENTER
c
character
int
33
pat
char[]
hashcode
19
pat[]
char[]
java.lang.String
33[33]
offset
int
int
33

DECLARE
std.locate(character;char *;int;)boolean:::EXIT13
c
character
int
33
pat
char[]
hashcode
19
pat[]
char[]
java.lang.String
33[33]
offset
int
int
33
return
boolean
int
5

DECLARE
std.omatch(char *;int *;char *;int;)boolean:::ENTER
lin
char[]
hashcode
34
lin[]
char[]
java.lang.String
35[36]
i
int[]
hashcode
10
*i
int
int
36
pat
char[]
hashcode
19
pat[]
char[]
java.lang.String
35[35]
j
int
int
35

DECLARE
std.omatch(char *;int *;char *;int;)boolean:::EXIT14
lin
char[]
hashcode
34
lin[]
char[]
java.lang.String
35[36]
i
int[]
hashcode
10
*i
int
int
36
pat
char[]
hashcode
19
pat[]
char[]
java.lang.String
35[35]
j
int
int
35
return
boolean
int
5

DECLARE
std.patsize(char *;int;)int:::ENTER
pat
char[]
hashcode
19
pat[]
char[]
java.lang.String
37[37]
n
int
int
37

DECLARE
std.patsize(char *;int;)int:::EXIT15
pat
char[]
hashcode
19
pat[]
char[]
java.lang.String
38[38]
n
int
int
38
return
int
int
38

DECLARE
std.amatch(char *;int;char *;int;)int:::ENTER
lin
char[]
hashcode
34
lin[]
char[]
java.lang.String
35[39]
offset
int
int
39
pat
char[]
hashcode
19
pat[]
char[]
java.lang.String
35[35]
j
int
int
35

DECLARE
std.amatch(char *;int;char *;int;)int:::EXIT16
lin
char[]
hashcode
34
lin[]
char[]
java.lang.String
35[40]
offset
int
int
40
pat
char[]
hashcode
19
pat[]
char[]
java.lang.String
35[35]
j
int
int
35
return
int
int
40

DECLARE
std.putsub(char *;int;int;char *;)void:::ENTER
lin
char[]
hashcode
34
lin[]
char[]
java.lang.String
41[42]
s1
int
int
42
s2
int
int
42
sub
char[]
hashcode
29
sub[]
char[]
java.lang.String
41[30]

DECLARE
std.putsub(char *;int;int;char *;)void:::EXIT17
lin
char[]
hashcode
34
lin[]
char[]
java.lang.String
41[42]
s1
int
int
42
s2
int
int
42
sub
char[]
hashcode
29
sub[]
char[]
java.lang.String
41[30]

DECLARE
std.subline(char *;char *;char *;)void:::ENTER
lin
char[]
hashcode
34
lin[]
char[]
java.lang.String
43[44]
pat
char[]
hashcode
19
pat[]
char[]
java.lang.String
43[43]
sub
char[]
hashcode
29
sub[]
char[]
java.lang.String
43[30]

DECLARE
std.subline(char *;char *;char *;)void:::EXIT18
lin
char[]
hashcode
34
lin[]
char[]
java.lang.String
43[44]
pat
char[]
hashcode
19
pat[]
char[]
java.lang.String
43[43]
sub
char[]
hashcode
29
sub[]
char[]
java.lang.String
43[30]

DECLARE
std.change(char *;char *;)void:::ENTER
pat
char[]
hashcode
19
pat[]
char[]
java.lang.String
45[45]
sub
char[]
hashcode
29
sub[]
char[]
java.lang.String
45[30]

DECLARE
std.change(char *;char *;)void:::EXIT19
pat
char[]
hashcode
19
pat[]
char[]
java.lang.String
45[45]
sub
char[]
hashcode
29
sub[]
char[]
java.lang.String
45[30]

DECLARE
std.main(int;char **;)int:::ENTER
argc
int
int
46
argv
char *[]
hashcode
47

DECLARE
std.main(int;char **;)int:::EXIT20
argc
int
int
46
argv
char *[]
hashcode
47
return
int
int
5

DECLARE
std.Caseerror(int;)void:::ENTER
n
int
int
48

DECLARE
std.Caseerror(int;)void:::EXIT21
n
int
int
48

# Implicit Type to Explicit Type
#   1 : s
#   2 : s_element
#   3 : s_index
#   4 : maxsize
#   5 : lh_return_value
#   6 : *j c maxset outset_element outset_index
#   7 : outset
#   8 : j
#   9 : *i s_index
#  10 : i
#  11 : lh_return_value s_element
#  12 : *j delim dest_element dest_index maxset src_element
#  13 : src
#  14 : *i src_index
#  15 : dest
#  16 : arg
#  17 : *j arg_element pat_element pat_index
#  18 : *i arg_index
#  19 : pat
#  20 : *j lastj pat_element pat_index
#  21 : c
#  22 : arg_element delim pat_element pat_index
#  23 : arg_index start
#  24 : arg_index lh_return_value start
#  25 : arg_element pat_element pat_index
#  26 : arg_index
#  27 : arg_element delim sub_element
#  28 : arg_index from
#  29 : sub
#  30 : sub_index
#  31 : arg_index from lh_return_value
#  32 : arg_element sub_element
#  33 : c offset pat_element pat_index
#  34 : lin
#  35 : j lin_element pat_element pat_index
#  36 : *i lin_index
#  37 : n pat_element pat_index
#  38 : lh_return_value n pat_element pat_index
#  39 : lin_index offset
#  40 : lh_return_value lin_index offset
#  41 : lin_element sub_element
#  42 : lin_index s1 s2
#  43 : lin_element pat_element pat_index sub_element
#  44 : lin_index
#  45 : pat_element pat_index sub_element
#  46 : argc
#  47 : argv
#  48 : n
