decl-version 2.0

var-comparability implicit

ppt std.new_ele(int;)Ele\_*:::ENTER
  ppt-type enter
  variable new_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.new_ele(int;)Ele\_*:::EXIT1
  ppt-type subexit
  variable new_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 24
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23
  variable return
    var-kind return
    dec-type Ele[]
    rep-type hashcode
    comparability 24
  variable return.next
    var-kind field next
    enclosing-var return
    dec-type _job[]
    rep-type hashcode
    comparability 25
  variable return.next.next
    var-kind field next
    enclosing-var return.next
    dec-type _job[]
    rep-type hashcode
    comparability 26
  variable return.next.prev
    var-kind field prev
    enclosing-var return.next
    dec-type _job[]
    rep-type hashcode
    comparability 27
  variable return.next.val
    var-kind field val
    enclosing-var return.next
    dec-type int
    rep-type int
    comparability 28
  variable return.prev
    var-kind field prev
    enclosing-var return
    dec-type _job[]
    rep-type hashcode
    comparability 29
  variable return.prev.next
    var-kind field next
    enclosing-var return.prev
    dec-type _job[]
    rep-type hashcode
    comparability 30
  variable return.prev.prev
    var-kind field prev
    enclosing-var return.prev
    dec-type _job[]
    rep-type hashcode
    comparability 31
  variable return.prev.val
    var-kind field val
    enclosing-var return.prev
    dec-type int
    rep-type int
    comparability 32
  variable return.val
    var-kind field val
    enclosing-var return
    dec-type int
    rep-type int
    comparability 33

ppt std.new_list()List\_*:::ENTER
  ppt-type enter
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.new_list()List\_*:::EXIT2
  ppt-type subexit
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 35
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23
  variable return
    var-kind return
    dec-type List[]
    rep-type hashcode
    comparability 35
  variable return.first
    var-kind field first
    enclosing-var return
    dec-type Ele[]
    rep-type hashcode
    comparability 36
  variable return.first.next
    var-kind field next
    enclosing-var return.first
    dec-type _job[]
    rep-type hashcode
    comparability 37
  variable return.first.prev
    var-kind field prev
    enclosing-var return.first
    dec-type _job[]
    rep-type hashcode
    comparability 38
  variable return.first.val
    var-kind field val
    enclosing-var return.first
    dec-type int
    rep-type int
    comparability 39
  variable return.last
    var-kind field last
    enclosing-var return
    dec-type Ele[]
    rep-type hashcode
    comparability 40
  variable return.last.next
    var-kind field next
    enclosing-var return.last
    dec-type _job[]
    rep-type hashcode
    comparability 41
  variable return.last.prev
    var-kind field prev
    enclosing-var return.last
    dec-type _job[]
    rep-type hashcode
    comparability 42
  variable return.last.val
    var-kind field val
    enclosing-var return.last
    dec-type int
    rep-type int
    comparability 43
  variable return.mem_count
    var-kind field mem_count
    enclosing-var return
    dec-type int
    rep-type int
    comparability 44

ppt std.append_ele(List\_*;Ele\_*;)List\_*:::ENTER
  ppt-type enter
  variable a_list
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 45
  variable a_list.first
    var-kind field first
    enclosing-var a_list
    dec-type Ele[]
    rep-type hashcode
    comparability 46
  variable a_list.first.next
    var-kind field next
    enclosing-var a_list.first
    dec-type _job[]
    rep-type hashcode
    comparability 47
  variable a_list.first.prev
    var-kind field prev
    enclosing-var a_list.first
    dec-type _job[]
    rep-type hashcode
    comparability 48
  variable a_list.first.val
    var-kind field val
    enclosing-var a_list.first
    dec-type int
    rep-type int
    comparability 49
  variable a_list.last
    var-kind field last
    enclosing-var a_list
    dec-type Ele[]
    rep-type hashcode
    comparability 50
  variable a_list.last.next
    var-kind field next
    enclosing-var a_list.last
    dec-type _job[]
    rep-type hashcode
    comparability 51
  variable a_list.last.prev
    var-kind field prev
    enclosing-var a_list.last
    dec-type _job[]
    rep-type hashcode
    comparability 52
  variable a_list.last.val
    var-kind field val
    enclosing-var a_list.last
    dec-type int
    rep-type int
    comparability 53
  variable a_list.mem_count
    var-kind field mem_count
    enclosing-var a_list
    dec-type int
    rep-type int
    comparability 54
  variable a_ele
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 55
  variable a_ele.next
    var-kind field next
    enclosing-var a_ele
    dec-type _job[]
    rep-type hashcode
    comparability 56
  variable a_ele.next.next
    var-kind field next
    enclosing-var a_ele.next
    dec-type _job[]
    rep-type hashcode
    comparability 57
  variable a_ele.next.prev
    var-kind field prev
    enclosing-var a_ele.next
    dec-type _job[]
    rep-type hashcode
    comparability 58
  variable a_ele.next.val
    var-kind field val
    enclosing-var a_ele.next
    dec-type int
    rep-type int
    comparability 59
  variable a_ele.prev
    var-kind field prev
    enclosing-var a_ele
    dec-type _job[]
    rep-type hashcode
    comparability 60
  variable a_ele.prev.next
    var-kind field next
    enclosing-var a_ele.prev
    dec-type _job[]
    rep-type hashcode
    comparability 61
  variable a_ele.prev.prev
    var-kind field prev
    enclosing-var a_ele.prev
    dec-type _job[]
    rep-type hashcode
    comparability 62
  variable a_ele.prev.val
    var-kind field val
    enclosing-var a_ele.prev
    dec-type int
    rep-type int
    comparability 63
  variable a_ele.val
    var-kind field val
    enclosing-var a_ele
    dec-type int
    rep-type int
    comparability 64
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 45
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.append_ele(List\_*;Ele\_*;)List\_*:::EXIT3
  ppt-type subexit
  variable a_list
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 65
  variable a_list.first
    var-kind field first
    enclosing-var a_list
    dec-type Ele[]
    rep-type hashcode
    comparability 46
  variable a_list.first.next
    var-kind field next
    enclosing-var a_list.first
    dec-type _job[]
    rep-type hashcode
    comparability 47
  variable a_list.first.prev
    var-kind field prev
    enclosing-var a_list.first
    dec-type _job[]
    rep-type hashcode
    comparability 48
  variable a_list.first.val
    var-kind field val
    enclosing-var a_list.first
    dec-type int
    rep-type int
    comparability 49
  variable a_list.last
    var-kind field last
    enclosing-var a_list
    dec-type Ele[]
    rep-type hashcode
    comparability 50
  variable a_list.last.next
    var-kind field next
    enclosing-var a_list.last
    dec-type _job[]
    rep-type hashcode
    comparability 51
  variable a_list.last.prev
    var-kind field prev
    enclosing-var a_list.last
    dec-type _job[]
    rep-type hashcode
    comparability 52
  variable a_list.last.val
    var-kind field val
    enclosing-var a_list.last
    dec-type int
    rep-type int
    comparability 53
  variable a_list.mem_count
    var-kind field mem_count
    enclosing-var a_list
    dec-type int
    rep-type int
    comparability 54
  variable a_ele
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 55
  variable a_ele.next
    var-kind field next
    enclosing-var a_ele
    dec-type _job[]
    rep-type hashcode
    comparability 56
  variable a_ele.next.next
    var-kind field next
    enclosing-var a_ele.next
    dec-type _job[]
    rep-type hashcode
    comparability 57
  variable a_ele.next.prev
    var-kind field prev
    enclosing-var a_ele.next
    dec-type _job[]
    rep-type hashcode
    comparability 58
  variable a_ele.next.val
    var-kind field val
    enclosing-var a_ele.next
    dec-type int
    rep-type int
    comparability 59
  variable a_ele.prev
    var-kind field prev
    enclosing-var a_ele
    dec-type _job[]
    rep-type hashcode
    comparability 60
  variable a_ele.prev.next
    var-kind field next
    enclosing-var a_ele.prev
    dec-type _job[]
    rep-type hashcode
    comparability 61
  variable a_ele.prev.prev
    var-kind field prev
    enclosing-var a_ele.prev
    dec-type _job[]
    rep-type hashcode
    comparability 62
  variable a_ele.prev.val
    var-kind field val
    enclosing-var a_ele.prev
    dec-type int
    rep-type int
    comparability 63
  variable a_ele.val
    var-kind field val
    enclosing-var a_ele
    dec-type int
    rep-type int
    comparability 64
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 65
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23
  variable return
    var-kind return
    dec-type List[]
    rep-type hashcode
    comparability 65
  variable return.first
    var-kind field first
    enclosing-var return
    dec-type Ele[]
    rep-type hashcode
    comparability 36
  variable return.first.next
    var-kind field next
    enclosing-var return.first
    dec-type _job[]
    rep-type hashcode
    comparability 37
  variable return.first.prev
    var-kind field prev
    enclosing-var return.first
    dec-type _job[]
    rep-type hashcode
    comparability 38
  variable return.first.val
    var-kind field val
    enclosing-var return.first
    dec-type int
    rep-type int
    comparability 39
  variable return.last
    var-kind field last
    enclosing-var return
    dec-type Ele[]
    rep-type hashcode
    comparability 40
  variable return.last.next
    var-kind field next
    enclosing-var return.last
    dec-type _job[]
    rep-type hashcode
    comparability 41
  variable return.last.prev
    var-kind field prev
    enclosing-var return.last
    dec-type _job[]
    rep-type hashcode
    comparability 42
  variable return.last.val
    var-kind field val
    enclosing-var return.last
    dec-type int
    rep-type int
    comparability 43
  variable return.mem_count
    var-kind field mem_count
    enclosing-var return
    dec-type int
    rep-type int
    comparability 44

ppt std.find_nth(List\_*;int;)Ele\_*:::ENTER
  ppt-type enter
  variable f_list
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 66
  variable f_list.first
    var-kind field first
    enclosing-var f_list
    dec-type Ele[]
    rep-type hashcode
    comparability 67
  variable f_list.first.next
    var-kind field next
    enclosing-var f_list.first
    dec-type _job[]
    rep-type hashcode
    comparability 68
  variable f_list.first.prev
    var-kind field prev
    enclosing-var f_list.first
    dec-type _job[]
    rep-type hashcode
    comparability 69
  variable f_list.first.val
    var-kind field val
    enclosing-var f_list.first
    dec-type int
    rep-type int
    comparability 70
  variable f_list.last
    var-kind field last
    enclosing-var f_list
    dec-type Ele[]
    rep-type hashcode
    comparability 71
  variable f_list.last.next
    var-kind field next
    enclosing-var f_list.last
    dec-type _job[]
    rep-type hashcode
    comparability 72
  variable f_list.last.prev
    var-kind field prev
    enclosing-var f_list.last
    dec-type _job[]
    rep-type hashcode
    comparability 73
  variable f_list.last.val
    var-kind field val
    enclosing-var f_list.last
    dec-type int
    rep-type int
    comparability 74
  variable f_list.mem_count
    var-kind field mem_count
    enclosing-var f_list
    dec-type int
    rep-type int
    comparability 75
  variable n
    var-kind variable
    dec-type int
    rep-type int
    comparability 76
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.find_nth(List\_*;int;)Ele\_*:::EXIT4
  ppt-type subexit
  variable f_list
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 66
  variable f_list.first
    var-kind field first
    enclosing-var f_list
    dec-type Ele[]
    rep-type hashcode
    comparability 67
  variable f_list.first.next
    var-kind field next
    enclosing-var f_list.first
    dec-type _job[]
    rep-type hashcode
    comparability 68
  variable f_list.first.prev
    var-kind field prev
    enclosing-var f_list.first
    dec-type _job[]
    rep-type hashcode
    comparability 69
  variable f_list.first.val
    var-kind field val
    enclosing-var f_list.first
    dec-type int
    rep-type int
    comparability 70
  variable f_list.last
    var-kind field last
    enclosing-var f_list
    dec-type Ele[]
    rep-type hashcode
    comparability 71
  variable f_list.last.next
    var-kind field next
    enclosing-var f_list.last
    dec-type _job[]
    rep-type hashcode
    comparability 72
  variable f_list.last.prev
    var-kind field prev
    enclosing-var f_list.last
    dec-type _job[]
    rep-type hashcode
    comparability 73
  variable f_list.last.val
    var-kind field val
    enclosing-var f_list.last
    dec-type int
    rep-type int
    comparability 74
  variable f_list.mem_count
    var-kind field mem_count
    enclosing-var f_list
    dec-type int
    rep-type int
    comparability 75
  variable n
    var-kind variable
    dec-type int
    rep-type int
    comparability 76
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23
  variable return
    var-kind return
    dec-type Ele[]
    rep-type hashcode
    comparability 24
  variable return.next
    var-kind field next
    enclosing-var return
    dec-type _job[]
    rep-type hashcode
    comparability 25
  variable return.next.next
    var-kind field next
    enclosing-var return.next
    dec-type _job[]
    rep-type hashcode
    comparability 26
  variable return.next.prev
    var-kind field prev
    enclosing-var return.next
    dec-type _job[]
    rep-type hashcode
    comparability 27
  variable return.next.val
    var-kind field val
    enclosing-var return.next
    dec-type int
    rep-type int
    comparability 28
  variable return.prev
    var-kind field prev
    enclosing-var return
    dec-type _job[]
    rep-type hashcode
    comparability 29
  variable return.prev.next
    var-kind field next
    enclosing-var return.prev
    dec-type _job[]
    rep-type hashcode
    comparability 30
  variable return.prev.prev
    var-kind field prev
    enclosing-var return.prev
    dec-type _job[]
    rep-type hashcode
    comparability 31
  variable return.prev.val
    var-kind field val
    enclosing-var return.prev
    dec-type int
    rep-type int
    comparability 32
  variable return.val
    var-kind field val
    enclosing-var return
    dec-type int
    rep-type int
    comparability 33

ppt std.find_nth(List\_*;int;)Ele\_*:::EXIT5
  ppt-type subexit
  variable f_list
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 66
  variable f_list.first
    var-kind field first
    enclosing-var f_list
    dec-type Ele[]
    rep-type hashcode
    comparability 67
  variable f_list.first.next
    var-kind field next
    enclosing-var f_list.first
    dec-type _job[]
    rep-type hashcode
    comparability 68
  variable f_list.first.prev
    var-kind field prev
    enclosing-var f_list.first
    dec-type _job[]
    rep-type hashcode
    comparability 69
  variable f_list.first.val
    var-kind field val
    enclosing-var f_list.first
    dec-type int
    rep-type int
    comparability 70
  variable f_list.last
    var-kind field last
    enclosing-var f_list
    dec-type Ele[]
    rep-type hashcode
    comparability 71
  variable f_list.last.next
    var-kind field next
    enclosing-var f_list.last
    dec-type _job[]
    rep-type hashcode
    comparability 72
  variable f_list.last.prev
    var-kind field prev
    enclosing-var f_list.last
    dec-type _job[]
    rep-type hashcode
    comparability 73
  variable f_list.last.val
    var-kind field val
    enclosing-var f_list.last
    dec-type int
    rep-type int
    comparability 74
  variable f_list.mem_count
    var-kind field mem_count
    enclosing-var f_list
    dec-type int
    rep-type int
    comparability 75
  variable n
    var-kind variable
    dec-type int
    rep-type int
    comparability 76
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23
  variable return
    var-kind return
    dec-type Ele[]
    rep-type hashcode
    comparability 24
  variable return.next
    var-kind field next
    enclosing-var return
    dec-type _job[]
    rep-type hashcode
    comparability 25
  variable return.next.next
    var-kind field next
    enclosing-var return.next
    dec-type _job[]
    rep-type hashcode
    comparability 26
  variable return.next.prev
    var-kind field prev
    enclosing-var return.next
    dec-type _job[]
    rep-type hashcode
    comparability 27
  variable return.next.val
    var-kind field val
    enclosing-var return.next
    dec-type int
    rep-type int
    comparability 28
  variable return.prev
    var-kind field prev
    enclosing-var return
    dec-type _job[]
    rep-type hashcode
    comparability 29
  variable return.prev.next
    var-kind field next
    enclosing-var return.prev
    dec-type _job[]
    rep-type hashcode
    comparability 30
  variable return.prev.prev
    var-kind field prev
    enclosing-var return.prev
    dec-type _job[]
    rep-type hashcode
    comparability 31
  variable return.prev.val
    var-kind field val
    enclosing-var return.prev
    dec-type int
    rep-type int
    comparability 32
  variable return.val
    var-kind field val
    enclosing-var return
    dec-type int
    rep-type int
    comparability 33

ppt std.del_ele(List\_*;Ele\_*;)List\_*:::ENTER
  ppt-type enter
  variable d_list
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 77
  variable d_list.first
    var-kind field first
    enclosing-var d_list
    dec-type Ele[]
    rep-type hashcode
    comparability 78
  variable d_list.first.next
    var-kind field next
    enclosing-var d_list.first
    dec-type _job[]
    rep-type hashcode
    comparability 79
  variable d_list.first.prev
    var-kind field prev
    enclosing-var d_list.first
    dec-type _job[]
    rep-type hashcode
    comparability 80
  variable d_list.first.val
    var-kind field val
    enclosing-var d_list.first
    dec-type int
    rep-type int
    comparability 81
  variable d_list.last
    var-kind field last
    enclosing-var d_list
    dec-type Ele[]
    rep-type hashcode
    comparability 82
  variable d_list.last.next
    var-kind field next
    enclosing-var d_list.last
    dec-type _job[]
    rep-type hashcode
    comparability 83
  variable d_list.last.prev
    var-kind field prev
    enclosing-var d_list.last
    dec-type _job[]
    rep-type hashcode
    comparability 84
  variable d_list.last.val
    var-kind field val
    enclosing-var d_list.last
    dec-type int
    rep-type int
    comparability 85
  variable d_list.mem_count
    var-kind field mem_count
    enclosing-var d_list
    dec-type int
    rep-type int
    comparability 86
  variable d_ele
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 87
  variable d_ele.next
    var-kind field next
    enclosing-var d_ele
    dec-type _job[]
    rep-type hashcode
    comparability 88
  variable d_ele.next.next
    var-kind field next
    enclosing-var d_ele.next
    dec-type _job[]
    rep-type hashcode
    comparability 89
  variable d_ele.next.prev
    var-kind field prev
    enclosing-var d_ele.next
    dec-type _job[]
    rep-type hashcode
    comparability 90
  variable d_ele.next.val
    var-kind field val
    enclosing-var d_ele.next
    dec-type int
    rep-type int
    comparability 91
  variable d_ele.prev
    var-kind field prev
    enclosing-var d_ele
    dec-type _job[]
    rep-type hashcode
    comparability 92
  variable d_ele.prev.next
    var-kind field next
    enclosing-var d_ele.prev
    dec-type _job[]
    rep-type hashcode
    comparability 93
  variable d_ele.prev.prev
    var-kind field prev
    enclosing-var d_ele.prev
    dec-type _job[]
    rep-type hashcode
    comparability 94
  variable d_ele.prev.val
    var-kind field val
    enclosing-var d_ele.prev
    dec-type int
    rep-type int
    comparability 95
  variable d_ele.val
    var-kind field val
    enclosing-var d_ele
    dec-type int
    rep-type int
    comparability 96
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.del_ele(List\_*;Ele\_*;)List\_*:::EXIT6
  ppt-type subexit
  variable d_list
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 97
  variable d_list.first
    var-kind field first
    enclosing-var d_list
    dec-type Ele[]
    rep-type hashcode
    comparability 78
  variable d_list.first.next
    var-kind field next
    enclosing-var d_list.first
    dec-type _job[]
    rep-type hashcode
    comparability 79
  variable d_list.first.prev
    var-kind field prev
    enclosing-var d_list.first
    dec-type _job[]
    rep-type hashcode
    comparability 80
  variable d_list.first.val
    var-kind field val
    enclosing-var d_list.first
    dec-type int
    rep-type int
    comparability 81
  variable d_list.last
    var-kind field last
    enclosing-var d_list
    dec-type Ele[]
    rep-type hashcode
    comparability 82
  variable d_list.last.next
    var-kind field next
    enclosing-var d_list.last
    dec-type _job[]
    rep-type hashcode
    comparability 83
  variable d_list.last.prev
    var-kind field prev
    enclosing-var d_list.last
    dec-type _job[]
    rep-type hashcode
    comparability 84
  variable d_list.last.val
    var-kind field val
    enclosing-var d_list.last
    dec-type int
    rep-type int
    comparability 85
  variable d_list.mem_count
    var-kind field mem_count
    enclosing-var d_list
    dec-type int
    rep-type int
    comparability 86
  variable d_ele
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 87
  variable d_ele.next
    var-kind field next
    enclosing-var d_ele
    dec-type _job[]
    rep-type hashcode
    comparability 88
  variable d_ele.next.next
    var-kind field next
    enclosing-var d_ele.next
    dec-type _job[]
    rep-type hashcode
    comparability 89
  variable d_ele.next.prev
    var-kind field prev
    enclosing-var d_ele.next
    dec-type _job[]
    rep-type hashcode
    comparability 90
  variable d_ele.next.val
    var-kind field val
    enclosing-var d_ele.next
    dec-type int
    rep-type int
    comparability 91
  variable d_ele.prev
    var-kind field prev
    enclosing-var d_ele
    dec-type _job[]
    rep-type hashcode
    comparability 92
  variable d_ele.prev.next
    var-kind field next
    enclosing-var d_ele.prev
    dec-type _job[]
    rep-type hashcode
    comparability 93
  variable d_ele.prev.prev
    var-kind field prev
    enclosing-var d_ele.prev
    dec-type _job[]
    rep-type hashcode
    comparability 94
  variable d_ele.prev.val
    var-kind field val
    enclosing-var d_ele.prev
    dec-type int
    rep-type int
    comparability 95
  variable d_ele.val
    var-kind field val
    enclosing-var d_ele
    dec-type int
    rep-type int
    comparability 96
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23
  variable return
    var-kind return
    dec-type List[]
    rep-type hashcode
    comparability 97
  variable return.first
    var-kind field first
    enclosing-var return
    dec-type Ele[]
    rep-type hashcode
    comparability 36
  variable return.first.next
    var-kind field next
    enclosing-var return.first
    dec-type _job[]
    rep-type hashcode
    comparability 37
  variable return.first.prev
    var-kind field prev
    enclosing-var return.first
    dec-type _job[]
    rep-type hashcode
    comparability 38
  variable return.first.val
    var-kind field val
    enclosing-var return.first
    dec-type int
    rep-type int
    comparability 39
  variable return.last
    var-kind field last
    enclosing-var return
    dec-type Ele[]
    rep-type hashcode
    comparability 40
  variable return.last.next
    var-kind field next
    enclosing-var return.last
    dec-type _job[]
    rep-type hashcode
    comparability 41
  variable return.last.prev
    var-kind field prev
    enclosing-var return.last
    dec-type _job[]
    rep-type hashcode
    comparability 42
  variable return.last.val
    var-kind field val
    enclosing-var return.last
    dec-type int
    rep-type int
    comparability 43
  variable return.mem_count
    var-kind field mem_count
    enclosing-var return
    dec-type int
    rep-type int
    comparability 44

ppt std.del_ele(List\_*;Ele\_*;)List\_*:::EXIT7
  ppt-type subexit
  variable d_list
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 97
  variable d_list.first
    var-kind field first
    enclosing-var d_list
    dec-type Ele[]
    rep-type hashcode
    comparability 78
  variable d_list.first.next
    var-kind field next
    enclosing-var d_list.first
    dec-type _job[]
    rep-type hashcode
    comparability 79
  variable d_list.first.prev
    var-kind field prev
    enclosing-var d_list.first
    dec-type _job[]
    rep-type hashcode
    comparability 80
  variable d_list.first.val
    var-kind field val
    enclosing-var d_list.first
    dec-type int
    rep-type int
    comparability 81
  variable d_list.last
    var-kind field last
    enclosing-var d_list
    dec-type Ele[]
    rep-type hashcode
    comparability 82
  variable d_list.last.next
    var-kind field next
    enclosing-var d_list.last
    dec-type _job[]
    rep-type hashcode
    comparability 83
  variable d_list.last.prev
    var-kind field prev
    enclosing-var d_list.last
    dec-type _job[]
    rep-type hashcode
    comparability 84
  variable d_list.last.val
    var-kind field val
    enclosing-var d_list.last
    dec-type int
    rep-type int
    comparability 85
  variable d_list.mem_count
    var-kind field mem_count
    enclosing-var d_list
    dec-type int
    rep-type int
    comparability 86
  variable d_ele
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 87
  variable d_ele.next
    var-kind field next
    enclosing-var d_ele
    dec-type _job[]
    rep-type hashcode
    comparability 88
  variable d_ele.next.next
    var-kind field next
    enclosing-var d_ele.next
    dec-type _job[]
    rep-type hashcode
    comparability 89
  variable d_ele.next.prev
    var-kind field prev
    enclosing-var d_ele.next
    dec-type _job[]
    rep-type hashcode
    comparability 90
  variable d_ele.next.val
    var-kind field val
    enclosing-var d_ele.next
    dec-type int
    rep-type int
    comparability 91
  variable d_ele.prev
    var-kind field prev
    enclosing-var d_ele
    dec-type _job[]
    rep-type hashcode
    comparability 92
  variable d_ele.prev.next
    var-kind field next
    enclosing-var d_ele.prev
    dec-type _job[]
    rep-type hashcode
    comparability 93
  variable d_ele.prev.prev
    var-kind field prev
    enclosing-var d_ele.prev
    dec-type _job[]
    rep-type hashcode
    comparability 94
  variable d_ele.prev.val
    var-kind field val
    enclosing-var d_ele.prev
    dec-type int
    rep-type int
    comparability 95
  variable d_ele.val
    var-kind field val
    enclosing-var d_ele
    dec-type int
    rep-type int
    comparability 96
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23
  variable return
    var-kind return
    dec-type List[]
    rep-type hashcode
    comparability 97
  variable return.first
    var-kind field first
    enclosing-var return
    dec-type Ele[]
    rep-type hashcode
    comparability 36
  variable return.first.next
    var-kind field next
    enclosing-var return.first
    dec-type _job[]
    rep-type hashcode
    comparability 37
  variable return.first.prev
    var-kind field prev
    enclosing-var return.first
    dec-type _job[]
    rep-type hashcode
    comparability 38
  variable return.first.val
    var-kind field val
    enclosing-var return.first
    dec-type int
    rep-type int
    comparability 39
  variable return.last
    var-kind field last
    enclosing-var return
    dec-type Ele[]
    rep-type hashcode
    comparability 40
  variable return.last.next
    var-kind field next
    enclosing-var return.last
    dec-type _job[]
    rep-type hashcode
    comparability 41
  variable return.last.prev
    var-kind field prev
    enclosing-var return.last
    dec-type _job[]
    rep-type hashcode
    comparability 42
  variable return.last.val
    var-kind field val
    enclosing-var return.last
    dec-type int
    rep-type int
    comparability 43
  variable return.mem_count
    var-kind field mem_count
    enclosing-var return
    dec-type int
    rep-type int
    comparability 44

ppt std.free_ele(Ele\_*;)void:::ENTER
  ppt-type enter
  variable ptr
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 98
  variable ptr.next
    var-kind field next
    enclosing-var ptr
    dec-type _job[]
    rep-type hashcode
    comparability 99
  variable ptr.next.next
    var-kind field next
    enclosing-var ptr.next
    dec-type _job[]
    rep-type hashcode
    comparability 100
  variable ptr.next.prev
    var-kind field prev
    enclosing-var ptr.next
    dec-type _job[]
    rep-type hashcode
    comparability 101
  variable ptr.next.val
    var-kind field val
    enclosing-var ptr.next
    dec-type int
    rep-type int
    comparability 102
  variable ptr.prev
    var-kind field prev
    enclosing-var ptr
    dec-type _job[]
    rep-type hashcode
    comparability 103
  variable ptr.prev.next
    var-kind field next
    enclosing-var ptr.prev
    dec-type _job[]
    rep-type hashcode
    comparability 104
  variable ptr.prev.prev
    var-kind field prev
    enclosing-var ptr.prev
    dec-type _job[]
    rep-type hashcode
    comparability 105
  variable ptr.prev.val
    var-kind field val
    enclosing-var ptr.prev
    dec-type int
    rep-type int
    comparability 106
  variable ptr.val
    var-kind field val
    enclosing-var ptr
    dec-type int
    rep-type int
    comparability 107
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.free_ele(Ele\_*;)void:::EXIT8
  ppt-type subexit
  variable ptr
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 98
  variable ptr.next
    var-kind field next
    enclosing-var ptr
    dec-type _job[]
    rep-type hashcode
    comparability 99
  variable ptr.next.next
    var-kind field next
    enclosing-var ptr.next
    dec-type _job[]
    rep-type hashcode
    comparability 100
  variable ptr.next.prev
    var-kind field prev
    enclosing-var ptr.next
    dec-type _job[]
    rep-type hashcode
    comparability 101
  variable ptr.next.val
    var-kind field val
    enclosing-var ptr.next
    dec-type int
    rep-type int
    comparability 102
  variable ptr.prev
    var-kind field prev
    enclosing-var ptr
    dec-type _job[]
    rep-type hashcode
    comparability 103
  variable ptr.prev.next
    var-kind field next
    enclosing-var ptr.prev
    dec-type _job[]
    rep-type hashcode
    comparability 104
  variable ptr.prev.prev
    var-kind field prev
    enclosing-var ptr.prev
    dec-type _job[]
    rep-type hashcode
    comparability 105
  variable ptr.prev.val
    var-kind field val
    enclosing-var ptr.prev
    dec-type int
    rep-type int
    comparability 106
  variable ptr.val
    var-kind field val
    enclosing-var ptr
    dec-type int
    rep-type int
    comparability 107
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.finish_process()void:::ENTER
  ppt-type enter
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.finish_process()void:::EXIT9
  ppt-type subexit
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.finish_all_processes()void:::ENTER
  ppt-type enter
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.finish_all_processes()void:::EXIT10
  ppt-type subexit
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.schedule()void:::ENTER
  ppt-type enter
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.schedule()void:::EXIT11
  ppt-type subexit
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.schedule()void:::EXIT12
  ppt-type subexit
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.upgrade_process_prio(int;float;)void:::ENTER
  ppt-type enter
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 108
  variable ratio
    var-kind variable
    dec-type float
    rep-type double
    comparability 109
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.upgrade_process_prio(int;float;)void:::EXIT13
  ppt-type subexit
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 108
  variable ratio
    var-kind variable
    dec-type float
    rep-type double
    comparability 109
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.upgrade_process_prio(int;float;)void:::EXIT14
  ppt-type subexit
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 108
  variable ratio
    var-kind variable
    dec-type float
    rep-type double
    comparability 109
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.unblock_process(float;)void:::ENTER
  ppt-type enter
  variable ratio
    var-kind variable
    dec-type float
    rep-type double
    comparability 109
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.unblock_process(float;)void:::EXIT15
  ppt-type subexit
  variable ratio
    var-kind variable
    dec-type float
    rep-type double
    comparability 109
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.quantum_expire()void:::ENTER
  ppt-type enter
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.quantum_expire()void:::EXIT16
  ppt-type subexit
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.block_process()void:::ENTER
  ppt-type enter
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.block_process()void:::EXIT17
  ppt-type subexit
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.new_process(int;)Ele\_*:::ENTER
  ppt-type enter
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 108
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.new_process(int;)Ele\_*:::EXIT18
  ppt-type subexit
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 108
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 24
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23
  variable return
    var-kind return
    dec-type Ele[]
    rep-type hashcode
    comparability 24
  variable return.next
    var-kind field next
    enclosing-var return
    dec-type _job[]
    rep-type hashcode
    comparability 25
  variable return.next.next
    var-kind field next
    enclosing-var return.next
    dec-type _job[]
    rep-type hashcode
    comparability 26
  variable return.next.prev
    var-kind field prev
    enclosing-var return.next
    dec-type _job[]
    rep-type hashcode
    comparability 27
  variable return.next.val
    var-kind field val
    enclosing-var return.next
    dec-type int
    rep-type int
    comparability 28
  variable return.prev
    var-kind field prev
    enclosing-var return
    dec-type _job[]
    rep-type hashcode
    comparability 29
  variable return.prev.next
    var-kind field next
    enclosing-var return.prev
    dec-type _job[]
    rep-type hashcode
    comparability 30
  variable return.prev.prev
    var-kind field prev
    enclosing-var return.prev
    dec-type _job[]
    rep-type hashcode
    comparability 31
  variable return.prev.val
    var-kind field val
    enclosing-var return.prev
    dec-type int
    rep-type int
    comparability 32
  variable return.val
    var-kind field val
    enclosing-var return
    dec-type int
    rep-type int
    comparability 33

ppt std.add_process(int;)void:::ENTER
  ppt-type enter
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 108
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.add_process(int;)void:::EXIT19
  ppt-type subexit
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 108
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.init_prio_queue(int;int;)void:::ENTER
  ppt-type enter
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 108
  variable num_proc
    var-kind variable
    dec-type int
    rep-type int
    comparability 110
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.init_prio_queue(int;int;)void:::EXIT20
  ppt-type subexit
  variable prio
    var-kind variable
    dec-type int
    rep-type int
    comparability 108
  variable num_proc
    var-kind variable
    dec-type int
    rep-type int
    comparability 110
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.initialize()void:::ENTER
  ppt-type enter
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.initialize()void:::EXIT21
  ppt-type subexit
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.main(int;char\_**;)int:::ENTER
  ppt-type enter
  variable argc
    var-kind variable
    dec-type int
    rep-type int
    comparability 111
  variable argv
    var-kind variable
    dec-type char\_*[]
    rep-type hashcode
    comparability 112
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23

ppt std.main(int;char\_**;)int:::EXIT22
  ppt-type subexit
  variable argc
    var-kind variable
    dec-type int
    rep-type int
    comparability 111
  variable argv
    var-kind variable
    dec-type char\_*[]
    rep-type hashcode
    comparability 112
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 113

ppt std.main(int;char\_**;)int:::EXIT23
  ppt-type subexit
  variable argc
    var-kind variable
    dec-type int
    rep-type int
    comparability 111
  variable argv
    var-kind variable
    dec-type char\_*[]
    rep-type hashcode
    comparability 112
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 113

ppt std.main(int;char\_**;)int:::EXIT24
  ppt-type subexit
  variable argc
    var-kind variable
    dec-type int
    rep-type int
    comparability 111
  variable argv
    var-kind variable
    dec-type char\_*[]
    rep-type hashcode
    comparability 112
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 113

ppt std.main(int;char\_**;)int:::EXIT25
  ppt-type subexit
  variable argc
    var-kind variable
    dec-type int
    rep-type int
    comparability 111
  variable argv
    var-kind variable
    dec-type char\_*[]
    rep-type hashcode
    comparability 112
  variable ::alloc_proc_num
    var-kind variable
    dec-type int
    rep-type int
    comparability 34
  variable ::num_processes
    var-kind variable
    dec-type int
    rep-type int
    comparability 2
  variable ::cur_proc
    var-kind variable
    dec-type Ele[]
    rep-type hashcode
    comparability 3
  variable ::cur_proc.next
    var-kind field next
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 4
  variable ::cur_proc.next.next
    var-kind field next
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 5
  variable ::cur_proc.next.prev
    var-kind field prev
    enclosing-var ::cur_proc.next
    dec-type _job[]
    rep-type hashcode
    comparability 6
  variable ::cur_proc.next.val
    var-kind field val
    enclosing-var ::cur_proc.next
    dec-type int
    rep-type int
    comparability 7
  variable ::cur_proc.prev
    var-kind field prev
    enclosing-var ::cur_proc
    dec-type _job[]
    rep-type hashcode
    comparability 8
  variable ::cur_proc.prev.next
    var-kind field next
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 9
  variable ::cur_proc.prev.prev
    var-kind field prev
    enclosing-var ::cur_proc.prev
    dec-type _job[]
    rep-type hashcode
    comparability 10
  variable ::cur_proc.prev.val
    var-kind field val
    enclosing-var ::cur_proc.prev
    dec-type int
    rep-type int
    comparability 11
  variable ::cur_proc.val
    var-kind field val
    enclosing-var ::cur_proc
    dec-type int
    rep-type int
    comparability 12
  variable ::prio_queue
    var-kind variable
    dec-type List\_*[]
    rep-type hashcode
    comparability 13
  variable ::block_queue
    var-kind variable
    dec-type List[]
    rep-type hashcode
    comparability 14
  variable ::block_queue.first
    var-kind field first
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 15
  variable ::block_queue.first.next
    var-kind field next
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 16
  variable ::block_queue.first.prev
    var-kind field prev
    enclosing-var ::block_queue.first
    dec-type _job[]
    rep-type hashcode
    comparability 17
  variable ::block_queue.first.val
    var-kind field val
    enclosing-var ::block_queue.first
    dec-type int
    rep-type int
    comparability 18
  variable ::block_queue.last
    var-kind field last
    enclosing-var ::block_queue
    dec-type Ele[]
    rep-type hashcode
    comparability 19
  variable ::block_queue.last.next
    var-kind field next
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 20
  variable ::block_queue.last.prev
    var-kind field prev
    enclosing-var ::block_queue.last
    dec-type _job[]
    rep-type hashcode
    comparability 21
  variable ::block_queue.last.val
    var-kind field val
    enclosing-var ::block_queue.last
    dec-type int
    rep-type int
    comparability 22
  variable ::block_queue.mem_count
    var-kind field mem_count
    enclosing-var ::block_queue
    dec-type int
    rep-type int
    comparability 23
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 113

# Implicit Type to Explicit Type
#   1 : alloc_proc_num new_num
#   2 : num_processes
#   3 : cur_proc
#   4 : cur_proc.next
#   5 : cur_proc.next.next
#   6 : cur_proc.next.prev
#   7 : cur_proc.next.val
#   8 : cur_proc.prev
#   9 : cur_proc.prev.next
#  10 : cur_proc.prev.prev
#  11 : cur_proc.prev.val
#  12 : cur_proc.val
#  13 : prio_queue
#  14 : block_queue
#  15 : block_queue.first
#  16 : block_queue.first.next
#  17 : block_queue.first.prev
#  18 : block_queue.first.val
#  19 : block_queue.last
#  20 : block_queue.last.next
#  21 : block_queue.last.prev
#  22 : block_queue.last.val
#  23 : block_queue.mem_count
#  24 : cur_proc lh_return_value
#  25 : return.next
#  26 : return.next.next
#  27 : return.next.prev
#  28 : return.next.val
#  29 : return.prev
#  30 : return.prev.next
#  31 : return.prev.prev
#  32 : return.prev.val
#  33 : return.val
#  34 : alloc_proc_num
#  35 : block_queue lh_return_value
#  36 : return.first
#  37 : return.first.next
#  38 : return.first.prev
#  39 : return.first.val
#  40 : return.last
#  41 : return.last.next
#  42 : return.last.prev
#  43 : return.last.val
#  44 : return.mem_count
#  45 : a_list block_queue
#  46 : a_list.first
#  47 : a_list.first.next
#  48 : a_list.first.prev
#  49 : a_list.first.val
#  50 : a_list.last
#  51 : a_list.last.next
#  52 : a_list.last.prev
#  53 : a_list.last.val
#  54 : a_list.mem_count
#  55 : a_ele cur_proc
#  56 : a_ele.next
#  57 : a_ele.next.next
#  58 : a_ele.next.prev
#  59 : a_ele.next.val
#  60 : a_ele.prev
#  61 : a_ele.prev.next
#  62 : a_ele.prev.prev
#  63 : a_ele.prev.val
#  64 : a_ele.val
#  65 : a_list block_queue lh_return_value
#  66 : block_queue f_list
#  67 : f_list.first
#  68 : f_list.first.next
#  69 : f_list.first.prev
#  70 : f_list.first.val
#  71 : f_list.last
#  72 : f_list.last.next
#  73 : f_list.last.prev
#  74 : f_list.last.val
#  75 : f_list.mem_count
#  76 : n
#  77 : block_queue d_list
#  78 : d_list.first
#  79 : d_list.first.next
#  80 : d_list.first.prev
#  81 : d_list.first.val
#  82 : d_list.last
#  83 : d_list.last.next
#  84 : d_list.last.prev
#  85 : d_list.last.val
#  86 : d_list.mem_count
#  87 : cur_proc d_ele
#  88 : d_ele.next
#  89 : d_ele.next.next
#  90 : d_ele.next.prev
#  91 : d_ele.next.val
#  92 : d_ele.prev
#  93 : d_ele.prev.next
#  94 : d_ele.prev.prev
#  95 : d_ele.prev.val
#  96 : d_ele.val
#  97 : block_queue d_list lh_return_value
#  98 : cur_proc ptr
#  99 : ptr.next
# 100 : ptr.next.next
# 101 : ptr.next.prev
# 102 : ptr.next.val
# 103 : ptr.prev
# 104 : ptr.prev.next
# 105 : ptr.prev.prev
# 106 : ptr.prev.val
# 107 : ptr.val
# 108 : prio
# 109 : ratio
# 110 : num_proc
# 111 : argc
# 112 : argv
# 113 : lh_return_value
