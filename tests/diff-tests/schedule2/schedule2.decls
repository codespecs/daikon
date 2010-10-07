VarComparability
implicit

DECLARE
std.enqueue(int;process *;)int:::ENTER
prio
int
int
1
::next_pid
int
int
2

DECLARE
std.enqueue(int;process *;)int:::EXIT1
prio
int
int
1
::next_pid
int
int
2
return
int
int
3

DECLARE
std.enqueue(int;process *;)int:::EXIT2
prio
int
int
1
::next_pid
int
int
2
return
int
int
3

DECLARE
std.main(int;char **;)int:::ENTER
argc
int
int
4
argv
char *[]
hashcode
5
::next_pid
int
int
2

DECLARE
std.main(int;char **;)int:::EXIT3
argc
int
int
4
argv
char *[]
hashcode
5
::next_pid
int
int
2
return
int
int
3

DECLARE
std.get_command(int *;int *;float *;)int:::ENTER
command
int[]
hashcode
6
*command
int
int
7
prio
int[]
hashcode
1
*prio
int
int
7
ratio
float[]
hashcode
8
*ratio
float
double
9
::next_pid
int
int
2

DECLARE
std.get_command(int *;int *;float *;)int:::EXIT4
command
int[]
hashcode
6
*command
int
int
7
prio
int[]
hashcode
1
*prio
int
int
7
ratio
float[]
hashcode
8
*ratio
float
double
9
::next_pid
int
int
2
return
int
int
3

DECLARE
std.get_command(int *;int *;float *;)int:::EXIT5
command
int[]
hashcode
6
*command
int
int
7
prio
int[]
hashcode
1
*prio
int
int
7
ratio
float[]
hashcode
8
*ratio
float
double
9
::next_pid
int
int
2
return
int
int
3

DECLARE
std.exit_here(int;)int:::ENTER
status
int
int
10
::next_pid
int
int
2

DECLARE
std.exit_here(int;)int:::EXIT6
status
int
int
10
::next_pid
int
int
2
return
int
int
3

DECLARE
std.new_job(int;)int:::ENTER
prio
int
int
1
::next_pid
int
int
2

DECLARE
std.new_job(int;)int:::EXIT7
prio
int
int
1
::next_pid
int
int
2
return
int
int
3

DECLARE
std.upgrade_prio(int;float;)int:::ENTER
prio
int
int
1
ratio
float
double
8
::next_pid
int
int
2

DECLARE
std.upgrade_prio(int;float;)int:::EXIT8
prio
int
int
1
ratio
float
double
8
::next_pid
int
int
2
return
int
int
3

DECLARE
std.upgrade_prio(int;float;)int:::EXIT9
prio
int
int
1
ratio
float
double
8
::next_pid
int
int
2
return
int
int
3

DECLARE
std.upgrade_prio(int;float;)int:::EXIT10
prio
int
int
1
ratio
float
double
8
::next_pid
int
int
2
return
int
int
3

DECLARE
std.block()int:::ENTER
::next_pid
int
int
2

DECLARE
std.block()int:::EXIT11
::next_pid
int
int
2
return
int
int
3

DECLARE
std.block()int:::EXIT12
::next_pid
int
int
2
return
int
int
3

DECLARE
std.unblock(float;)int:::ENTER
ratio
float
double
8
::next_pid
int
int
2

DECLARE
std.unblock(float;)int:::EXIT13
ratio
float
double
8
::next_pid
int
int
2
return
int
int
3

DECLARE
std.unblock(float;)int:::EXIT14
ratio
float
double
8
::next_pid
int
int
2
return
int
int
3

DECLARE
std.quantum_expire()int:::ENTER
::next_pid
int
int
2

DECLARE
std.quantum_expire()int:::EXIT15
::next_pid
int
int
2
return
int
int
3

DECLARE
std.quantum_expire()int:::EXIT16
::next_pid
int
int
2
return
int
int
3

DECLARE
std.finish()int:::ENTER
::next_pid
int
int
2

DECLARE
std.finish()int:::EXIT17
::next_pid
int
int
2
return
int
int
3

DECLARE
std.finish()int:::EXIT18
::next_pid
int
int
2
return
int
int
3

DECLARE
std.flush()int:::ENTER
::next_pid
int
int
2

DECLARE
std.flush()int:::EXIT19
::next_pid
int
int
2
return
int
int
3

DECLARE
std.get_current()process *:::ENTER
::next_pid
int
int
2

DECLARE
std.get_current()process *:::EXIT20
::next_pid
int
int
2

DECLARE
std.reschedule(int;)int:::ENTER
prio
int
int
1
::next_pid
int
int
2

DECLARE
std.reschedule(int;)int:::EXIT21
prio
int
int
1
::next_pid
int
int
2
return
int
int
3

DECLARE
std.schedule(int;int;float;)int:::ENTER
command
int
int
11
prio
int
int
11
ratio
float
double
8
::next_pid
int
int
2

DECLARE
std.schedule(int;int;float;)int:::EXIT22
command
int
int
11
prio
int
int
11
ratio
float
double
8
::next_pid
int
int
2
return
int
int
3

DECLARE
std.put_end(int;process *;)int:::ENTER
prio
int
int
1
::next_pid
int
int
2

DECLARE
std.put_end(int;process *;)int:::EXIT23
prio
int
int
1
::next_pid
int
int
2
return
int
int
3

DECLARE
std.put_end(int;process *;)int:::EXIT24
prio
int
int
1
::next_pid
int
int
2
return
int
int
3

DECLARE
std.get_process(int;float;process **;)int:::ENTER
prio
int
int
1
ratio
float
double
8
job
process *[]
hashcode
12
::next_pid
int
int
2

DECLARE
std.get_process(int;float;process **;)int:::EXIT25
prio
int
int
1
ratio
float
double
8
job
process *[]
hashcode
12
::next_pid
int
int
2
return
int
int
3

DECLARE
std.get_process(int;float;process **;)int:::EXIT26
prio
int
int
1
ratio
float
double
8
job
process *[]
hashcode
12
::next_pid
int
int
2
return
int
int
3

DECLARE
std.get_process(int;float;process **;)int:::EXIT27
prio
int
int
1
ratio
float
double
8
job
process *[]
hashcode
12
::next_pid
int
int
2
return
int
int
3

DECLARE
std.get_process(int;float;process **;)int:::EXIT28
prio
int
int
1
ratio
float
double
8
job
process *[]
hashcode
12
::next_pid
int
int
2
return
int
int
3

# Implicit Type to Explicit Type
#   1 : prio
#   2 : next_pid
#   3 : lh_return_value
#   4 : argc
#   5 : argv
#   6 : command
#   7 : *command *prio
#   8 : ratio
#   9 : *ratio
#  10 : status
#  11 : command prio
#  12 : job
