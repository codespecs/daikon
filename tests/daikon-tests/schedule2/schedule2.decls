decl-version 2.0

var-comparability implicit

ppt std.enqueue(int;process\_*;)int:::ENTER
  ppt-type enter
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable new_process
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 2
  variable new_process.priority
    var-kind field priority
    enclosing-var new_process
    dec-type int
    rep-type int
    comparability 3
  variable new_process.next
    var-kind field next
    enclosing-var new_process
    dec-type process[]
    rep-type hashcode
    comparability 4
  variable new_process.next.priority
    var-kind field priority
    enclosing-var new_process.next
    dec-type int
    rep-type int
    comparability 5
  variable new_process.next.next
    var-kind field next
    enclosing-var new_process.next
    dec-type process[]
    rep-type hashcode
    comparability 6
  variable new_process.next.next.priority
    var-kind field priority
    enclosing-var new_process.next.next
    dec-type int
    rep-type int
    comparability 7
  variable new_process.next.next.next
    var-kind field next
    enclosing-var new_process.next.next
    dec-type process[]
    rep-type hashcode
    comparability 8
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23

ppt std.enqueue(int;process\_*;)int:::EXIT1
  ppt-type subexit
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable new_process
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 2
  variable new_process.priority
    var-kind field priority
    enclosing-var new_process
    dec-type int
    rep-type int
    comparability 3
  variable new_process.next
    var-kind field next
    enclosing-var new_process
    dec-type process[]
    rep-type hashcode
    comparability 4
  variable new_process.next.priority
    var-kind field priority
    enclosing-var new_process.next
    dec-type int
    rep-type int
    comparability 5
  variable new_process.next.next
    var-kind field next
    enclosing-var new_process.next
    dec-type process[]
    rep-type hashcode
    comparability 6
  variable new_process.next.next.priority
    var-kind field priority
    enclosing-var new_process.next.next
    dec-type int
    rep-type int
    comparability 7
  variable new_process.next.next.next
    var-kind field next
    enclosing-var new_process.next.next
    dec-type process[]
    rep-type hashcode
    comparability 8
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.enqueue(int;process\_*;)int:::EXIT2
  ppt-type subexit
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable new_process
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 2
  variable new_process.priority
    var-kind field priority
    enclosing-var new_process
    dec-type int
    rep-type int
    comparability 3
  variable new_process.next
    var-kind field next
    enclosing-var new_process
    dec-type process[]
    rep-type hashcode
    comparability 4
  variable new_process.next.priority
    var-kind field priority
    enclosing-var new_process.next
    dec-type int
    rep-type int
    comparability 5
  variable new_process.next.next
    var-kind field next
    enclosing-var new_process.next
    dec-type process[]
    rep-type hashcode
    comparability 6
  variable new_process.next.next.priority
    var-kind field priority
    enclosing-var new_process.next.next
    dec-type int
    rep-type int
    comparability 7
  variable new_process.next.next.next
    var-kind field next
    enclosing-var new_process.next.next
    dec-type process[]
    rep-type hashcode
    comparability 8
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.main(int;char\_**;)int:::ENTER
  ppt-type enter
  variable argc
    var-kind variable
    dec-type int
    rep-type int
    comparability 25
  variable argv
    var-kind variable
    dec-type char\_*[]
    rep-type hashcode
    comparability 26
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23

ppt std.main(int;char\_**;)int:::EXIT3
  ppt-type subexit
  variable argc
    var-kind variable
    dec-type int
    rep-type int
    comparability 25
  variable argv
    var-kind variable
    dec-type char\_*[]
    rep-type hashcode
    comparability 26
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.get_command(int\_*;int\_*;float\_*;)int:::ENTER
  ppt-type enter
  variable command
    var-kind variable
    dec-type int[]
    rep-type hashcode
    comparability 27
  variable *command
    var-kind variable
    dec-type int
    rep-type int
    comparability 28
  variable prio
    var-kind variable
    dec-type int[]
    rep-type hashcode
    comparability 1
  variable *prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 28
  variable ratio
    var-kind variable
    dec-type float[]
    rep-type hashcode
    comparability 29
  variable *ratio
    var-kind variable
    dec-type float
    rep-type double
    comparability 30
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23

ppt std.get_command(int\_*;int\_*;float\_*;)int:::EXIT4
  ppt-type subexit
  variable command
    var-kind variable
    dec-type int[]
    rep-type hashcode
    comparability 27
  variable *command
    var-kind variable
    dec-type int
    rep-type int
    comparability 28
  variable prio
    var-kind variable
    dec-type int[]
    rep-type hashcode
    comparability 1
  variable *prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 28
  variable ratio
    var-kind variable
    dec-type float[]
    rep-type hashcode
    comparability 29
  variable *ratio
    var-kind variable
    dec-type float
    rep-type double
    comparability 30
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.get_command(int\_*;int\_*;float\_*;)int:::EXIT5
  ppt-type subexit
  variable command
    var-kind variable
    dec-type int[]
    rep-type hashcode
    comparability 27
  variable *command
    var-kind variable
    dec-type int
    rep-type int
    comparability 28
  variable prio
    var-kind variable
    dec-type int[]
    rep-type hashcode
    comparability 1
  variable *prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 28
  variable ratio
    var-kind variable
    dec-type float[]
    rep-type hashcode
    comparability 29
  variable *ratio
    var-kind variable
    dec-type float
    rep-type double
    comparability 30
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.exit_here(int;)int:::ENTER
  ppt-type enter
  variable status
    var-kind variable
    dec-type int
    rep-type int
    comparability 31
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23

ppt std.exit_here(int;)int:::EXIT6
  ppt-type subexit
  variable status
    var-kind variable
    dec-type int
    rep-type int
    comparability 31
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.new_job(int;)int:::ENTER
  ppt-type enter
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23

ppt std.new_job(int;)int:::EXIT7
  ppt-type subexit
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.upgrade_prio(int;float;)int:::ENTER
  ppt-type enter
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable ratio
    var-kind variable
    dec-type float
    rep-type double
    comparability 29
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23

ppt std.upgrade_prio(int;float;)int:::EXIT8
  ppt-type subexit
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable ratio
    var-kind variable
    dec-type float
    rep-type double
    comparability 29
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.upgrade_prio(int;float;)int:::EXIT9
  ppt-type subexit
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable ratio
    var-kind variable
    dec-type float
    rep-type double
    comparability 29
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.upgrade_prio(int;float;)int:::EXIT10
  ppt-type subexit
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable ratio
    var-kind variable
    dec-type float
    rep-type double
    comparability 29
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.block()int:::ENTER
  ppt-type enter
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23

ppt std.block()int:::EXIT11
  ppt-type subexit
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.block()int:::EXIT12
  ppt-type subexit
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.unblock(float;)int:::ENTER
  ppt-type enter
  variable ratio
    var-kind variable
    dec-type float
    rep-type double
    comparability 29
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23

ppt std.unblock(float;)int:::EXIT13
  ppt-type subexit
  variable ratio
    var-kind variable
    dec-type float
    rep-type double
    comparability 29
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.unblock(float;)int:::EXIT14
  ppt-type subexit
  variable ratio
    var-kind variable
    dec-type float
    rep-type double
    comparability 29
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.quantum_expire()int:::ENTER
  ppt-type enter
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23

ppt std.quantum_expire()int:::EXIT15
  ppt-type subexit
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.quantum_expire()int:::EXIT16
  ppt-type subexit
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.finish()int:::ENTER
  ppt-type enter
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23

ppt std.finish()int:::EXIT17
  ppt-type subexit
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.finish()int:::EXIT18
  ppt-type subexit
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.flush()int:::ENTER
  ppt-type enter
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23

ppt std.flush()int:::EXIT19
  ppt-type subexit
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.get_current()process\_*:::ENTER
  ppt-type enter
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23

ppt std.get_current()process\_*:::EXIT20
  ppt-type subexit
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 32
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type process[]
    rep-type hashcode
    comparability 32
  variable return.priority
    var-kind field priority
    enclosing-var return
    dec-type int
    rep-type int
    comparability 33
  variable return.next
    var-kind field next
    enclosing-var return
    dec-type process[]
    rep-type hashcode
    comparability 34
  variable return.next.priority
    var-kind field priority
    enclosing-var return.next
    dec-type int
    rep-type int
    comparability 35
  variable return.next.next
    var-kind field next
    enclosing-var return.next
    dec-type process[]
    rep-type hashcode
    comparability 36
  variable return.next.next.priority
    var-kind field priority
    enclosing-var return.next.next
    dec-type int
    rep-type int
    comparability 37
  variable return.next.next.next
    var-kind field next
    enclosing-var return.next.next
    dec-type process[]
    rep-type hashcode
    comparability 38

ppt std.reschedule(int;)int:::ENTER
  ppt-type enter
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23

ppt std.reschedule(int;)int:::EXIT21
  ppt-type subexit
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.schedule(int;int;float;)int:::ENTER
  ppt-type enter
  variable command
    var-kind variable
    dec-type int
    rep-type int
    comparability 39
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 39
  variable ratio
    var-kind variable
    dec-type float
    rep-type double
    comparability 29
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23

ppt std.schedule(int;int;float;)int:::EXIT22
  ppt-type subexit
  variable command
    var-kind variable
    dec-type int
    rep-type int
    comparability 39
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 39
  variable ratio
    var-kind variable
    dec-type float
    rep-type double
    comparability 29
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.put_end(int;process\_*;)int:::ENTER
  ppt-type enter
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable process_ptr
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 40
  variable process_ptr.priority
    var-kind field priority
    enclosing-var process_ptr
    dec-type int
    rep-type int
    comparability 41
  variable process_ptr.next
    var-kind field next
    enclosing-var process_ptr
    dec-type process[]
    rep-type hashcode
    comparability 42
  variable process_ptr.next.priority
    var-kind field priority
    enclosing-var process_ptr.next
    dec-type int
    rep-type int
    comparability 43
  variable process_ptr.next.next
    var-kind field next
    enclosing-var process_ptr.next
    dec-type process[]
    rep-type hashcode
    comparability 44
  variable process_ptr.next.next.priority
    var-kind field priority
    enclosing-var process_ptr.next.next
    dec-type int
    rep-type int
    comparability 45
  variable process_ptr.next.next.next
    var-kind field next
    enclosing-var process_ptr.next.next
    dec-type process[]
    rep-type hashcode
    comparability 46
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 40
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23

ppt std.put_end(int;process\_*;)int:::EXIT23
  ppt-type subexit
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable process_ptr
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 40
  variable process_ptr.priority
    var-kind field priority
    enclosing-var process_ptr
    dec-type int
    rep-type int
    comparability 41
  variable process_ptr.next
    var-kind field next
    enclosing-var process_ptr
    dec-type process[]
    rep-type hashcode
    comparability 42
  variable process_ptr.next.priority
    var-kind field priority
    enclosing-var process_ptr.next
    dec-type int
    rep-type int
    comparability 43
  variable process_ptr.next.next
    var-kind field next
    enclosing-var process_ptr.next
    dec-type process[]
    rep-type hashcode
    comparability 44
  variable process_ptr.next.next.priority
    var-kind field priority
    enclosing-var process_ptr.next.next
    dec-type int
    rep-type int
    comparability 45
  variable process_ptr.next.next.next
    var-kind field next
    enclosing-var process_ptr.next.next
    dec-type process[]
    rep-type hashcode
    comparability 46
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 40
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.put_end(int;process\_*;)int:::EXIT24
  ppt-type subexit
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable process_ptr
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 40
  variable process_ptr.priority
    var-kind field priority
    enclosing-var process_ptr
    dec-type int
    rep-type int
    comparability 41
  variable process_ptr.next
    var-kind field next
    enclosing-var process_ptr
    dec-type process[]
    rep-type hashcode
    comparability 42
  variable process_ptr.next.priority
    var-kind field priority
    enclosing-var process_ptr.next
    dec-type int
    rep-type int
    comparability 43
  variable process_ptr.next.next
    var-kind field next
    enclosing-var process_ptr.next
    dec-type process[]
    rep-type hashcode
    comparability 44
  variable process_ptr.next.next.priority
    var-kind field priority
    enclosing-var process_ptr.next.next
    dec-type int
    rep-type int
    comparability 45
  variable process_ptr.next.next.next
    var-kind field next
    enclosing-var process_ptr.next.next
    dec-type process[]
    rep-type hashcode
    comparability 46
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 40
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.get_process(int;float;process\_**;)int:::ENTER
  ppt-type enter
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable ratio
    var-kind variable
    dec-type float
    rep-type double
    comparability 29
  variable job
    var-kind variable
    dec-type process\_*[]
    rep-type hashcode
    comparability 47
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23

ppt std.get_process(int;float;process\_**;)int:::EXIT25
  ppt-type subexit
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable ratio
    var-kind variable
    dec-type float
    rep-type double
    comparability 29
  variable job
    var-kind variable
    dec-type process\_*[]
    rep-type hashcode
    comparability 47
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.get_process(int;float;process\_**;)int:::EXIT26
  ppt-type subexit
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable ratio
    var-kind variable
    dec-type float
    rep-type double
    comparability 29
  variable job
    var-kind variable
    dec-type process\_*[]
    rep-type hashcode
    comparability 47
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.get_process(int;float;process\_**;)int:::EXIT27
  ppt-type subexit
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable ratio
    var-kind variable
    dec-type float
    rep-type double
    comparability 29
  variable job
    var-kind variable
    dec-type process\_*[]
    rep-type hashcode
    comparability 47
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

ppt std.get_process(int;float;process\_**;)int:::EXIT28
  ppt-type subexit
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable ratio
    var-kind variable
    dec-type float
    rep-type double
    comparability 29
  variable job
    var-kind variable
    dec-type process\_*[]
    rep-type hashcode
    comparability 47
  variable ::current_job
    var-kind variable
    dec-type process[]
    rep-type hashcode
    comparability 9
  variable ::current_job.priority
    var-kind field priority
    enclosing-var ::current_job
    dec-type int
    rep-type int
    comparability 10
  variable ::current_job.next
    var-kind field next
    enclosing-var ::current_job
    dec-type process[]
    rep-type hashcode
    comparability 11
  variable ::current_job.next.priority
    var-kind field priority
    enclosing-var ::current_job.next
    dec-type int
    rep-type int
    comparability 12
  variable ::current_job.next.next
    var-kind field next
    enclosing-var ::current_job.next
    dec-type process[]
    rep-type hashcode
    comparability 13
  variable ::current_job.next.next.priority
    var-kind field priority
    enclosing-var ::current_job.next.next
    dec-type int
    rep-type int
    comparability 14
  variable ::current_job.next.next.next
    var-kind field next
    enclosing-var ::current_job.next.next
    dec-type process[]
    rep-type hashcode
    comparability 15
  variable ::next_pid
    var-kind variable
    dec-type int
    rep-type int
    comparability 16
  variable ::prio_queue
    var-kind variable
    dec-type queue[]
    rep-type hashcode
    comparability 17
  variable ::prio_queue.length
    var-kind field length
    enclosing-var ::prio_queue
    dec-type int
    rep-type int
    comparability 18
  variable ::prio_queue.head
    var-kind field head
    enclosing-var ::prio_queue
    dec-type process[]
    rep-type hashcode
    comparability 19
  variable ::prio_queue.head.priority
    var-kind field priority
    enclosing-var ::prio_queue.head
    dec-type int
    rep-type int
    comparability 20
  variable ::prio_queue.head.next
    var-kind field next
    enclosing-var ::prio_queue.head
    dec-type process[]
    rep-type hashcode
    comparability 21
  variable ::prio_queue.head.next.priority
    var-kind field priority
    enclosing-var ::prio_queue.head.next
    dec-type int
    rep-type int
    comparability 22
  variable ::prio_queue.head.next.next
    var-kind field next
    enclosing-var ::prio_queue.head.next
    dec-type process[]
    rep-type hashcode
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 24

# Implicit Type to Explicit Type
#   1 : prio
#   2 : current_job new_process
#   3 : new_process.priority
#   4 : new_process.next
#   5 : new_process.next.priority
#   6 : new_process.next.next
#   7 : new_process.next.next.priority
#   8 : new_process.next.next.next
#   9 : current_job
#  10 : current_job.priority
#  11 : current_job.next
#  12 : current_job.next.priority
#  13 : current_job.next.next
#  14 : current_job.next.next.priority
#  15 : current_job.next.next.next
#  16 : next_pid
#  17 : prio_queue
#  18 : prio_queue.length
#  19 : prio_queue.head
#  20 : prio_queue.head.priority
#  21 : prio_queue.head.next
#  22 : prio_queue.head.next.priority
#  23 : prio_queue.head.next.next
#  24 : lh_return_value
#  25 : argc
#  26 : argv
#  27 : command
#  28 : *command *prio
#  29 : ratio
#  30 : *ratio
#  31 : status
#  32 : current_job lh_return_value
#  33 : return.priority
#  34 : return.next
#  35 : return.next.priority
#  36 : return.next.next
#  37 : return.next.next.priority
#  38 : return.next.next.next
#  39 : command prio
#  40 : current_job process_ptr
#  41 : process_ptr.priority
#  42 : process_ptr.next
#  43 : process_ptr.next.priority
#  44 : process_ptr.next.next
#  45 : process_ptr.next.next.priority
#  46 : process_ptr.next.next.next
#  47 : job
