This directory contains several example C programs, to illustrate the
use of Daikon and the C front end.

STEP-BY-STEP INSTRUCTIONS

First, install the Daikon invariant detector.  This is well-documented
in the daikon manual.  Download and install the most recent
distrobution.

In addition to the Daikon engine, you also need a front end.  Dfec is
the front end for C.  Obtain dfec.zip and set it up as follows.

(We will refer to the directory DFEC is unzipped in as $DFECDIR.
This path, and all other paths discussed in these instructions, should
be in terms of the cywgin file namespace;
e.g. /cygdrive/c/mydir/file.txt not c:\mydir\file.txt).

Compile the daikon runtime library.  At a cygwin prompt:

$ cd $DFECDIR; gcc -c daikon_runtime.cc

This generates the file daikon_runtime.o in $DFECDIR.

Set the DTRACEAPPEND environment variable.  The bash syntax is shown
here.

$ export DTRACEAPPEND=1

Next, determine paths and options for your system by running g++.
At a cygwin prompt:

$ touch empty.cc
$ g++ -v -E empty.cc
Reading specs from /usr/lib/gcc-lib/i686-pc-cygwin/2.95.3-4/specs
gcc version 2.95.3-4 (cygwin special)
 /usr/lib/gcc-lib/i686-pc-cygwin/2.95.3-4/cpp0.exe -lang-c++ -v -D__GNUC__=2 -D__GNUG__=2 -D__GNUC_MINOR__=95 -D__cplusplus -D_X86_=1 -D_X86_=1 -Asystem(winnt)-D__EXCEPTIONS -Acpu(i386) -Amachine(i386) -Di386 -D__i386 -D__i386__ -Di686 -Dpentiumpro -D__i686 -D__i686__ -D__pentiumpro -D__pentiumpro__ -D__stdcall=__attribute__((__stdcall__)) -D__cdecl=__attribute__((__cdecl__)) -D_stdcall=__attribute__((__stdcall__)) -D_cdecl=__attribute__((__cdecl__)) -D__declspec(x)=__attribute__((x)) -D__CYGWIN32__ -D__CYGWIN__ -Dunix -D__unix__ -D__unix -isystem /usr/local/include -idirafter /usr/include -idirafter /usr/include/w32api -isystem /usr/include/g++-3 -isystem /usr/include/g++ empty.cc
GNU CPP version 2.95.3-4 (cygwin special) (80386, BSD syntax)
#include "..." search starts here:
#include <...> search starts here:
 /usr/include/g++-3
 /usr/lib/gcc-lib/i686-pc-cygwin/2.95.3-4/include
 /usr/include
 /usr/include/w32api
End of search list.
 The following default directories have been omitted from the search path:
End of omitted list.
# 1 "empty.cc"

$ rm empty.cc

Set INC_PATH to be everything following an -isystem in the cpp0.exe
invocation above, INC_AFTER to be everything following an -idirafter,
and INC_SEARCH to be the search path after the "#include <...> search
starts here:", yielding this:

$ export INC_PATH="-I/usr/local/include -I/usr/include/g++-3 -I/usr/include/g++"
$ export INC_AFTER="-I/usr/include -I/usr/include/w32api"
$ export INC_SEARCH="-I/usr/include/g++-3 -I/usr/lib/gcc-lib/i686-pc-cygwin/2.95.3-4/include -I/usr/include -I/usr/include/w32api"

There are a lot of defines that need to get sent to dfec to get it to
process the system headers correctly.

$ export DFEC_OPTS="-w \"-D__SIZE_TYPE__=unsigned int\" -D__attribute__\(x\)=\"\" \"-D__extension__=\" -D__null=0 -D__GNUG__=1 -D_WIN32"

XXX I can't get the quoting right on the above line.  Should be the
XXX same in bash on linux, so maybe someone there can debug it?  The
XXX quotes are the thing.

Now set DFEC to be the correct dfec, plus include paths (note the
order!); recall that all paths are cygwin paths.

$ export DFEC="$DFECDIR/dfec.exe ${INC_PATH} ${INC_SEARCH} ${INC_AFTER} ${DFEC_OPTS}"

Then, to invoke dfec from the cygwin bash shell, you can call ${DFEC}.

$ ${DFEC} program.c ... (more later)

Now dfec is set up.  Let's move on to some examples

******************** EXAMPLES ********************

Unpack the examples into a directory (which we will call $EXAMPLES).
Also recall that the c front end (dfec) is installed in $DFECDIR.

$ tar zxvf c-examples.tar.gz

To detect invariants for a program, you need to perform three basic
tasks: instrument the target program (steps 1-3), run the instrumented
program to create a data trace file (step 4), and run Daikon over the
data trace file to produce invariants (steps 5-6).  The following
instructions are for the print_tokens example.  The other examples can
be run in exactly the same manner.

1. Change to the directory containing the print_tokens program.

$ cd $EXAMPLES/print_tokens

2. Instrument the program using dfec, the C front end.

First copy $DFECDIR/daikon_runtime.h into the directory that your
source file resides in.

$ cp $DFECDIR/daikon_runtime.h .

Then run the front-end:

$ ${DFEC} print_tokens.c stream.h tokens.h

We instrument the source file and all the user-created header files it
depends on.  This command creates two directories, daikon-instrumented
and daikon-output.  It creates an instrumented and preprocessed
version of print_tokens.c at daikon-instrumented/print_tokens.cc.  It
creates a declaration file at daikon-output/print_tokens.decls.

Now, under Windows, you have to then fix the instrumented file with a
sed script to put in a gcc-specific __attribute__ for the _ctype_
variable, which is used in our test suite.  Run it like this:

$ cd daikon-instrumented
$ sed -f $DFECDIR/fix.sed print_tokens.cc > print_tokens_fixed.cc
$ cd ..

This fixes the above problem.

3. Compile and link the instrumented program.

$ g++ -w -o print_tokens.exe daikon-instrumented/print_tokens_fixed.cc \
     $DFECDIR/daikon_runtime.o

This creates the executable print_tokens.exe in the current directory.

4. Run the print_tokens test suite.

$ sh tests.sh

This creates a data trace file at daikon-output/print_tokens.dtrace.

==== BEFORE HERE IS "OK" ACCORDING TO JEREMY

5. Run Daikon on the trace file.

$ java -Xmx256m daikon.Daikon -o print_tokens.inv \
     daikon-output/print_tokens.decls daikon-output/print_tokens.dtrace

The invariants are printed to standard out, and a binary representation
of the invariants is written to print_tokens.inv.

6. Examine the invariants. There are three ways to do this.

   - Examine the output from running Daikon.  (You may find it convenient
     to capture the output in a file; add "> print_tokens.txt" to
     the end of the command that runs Daikon.)

   - Use the Daikon Tree GUI to browse the invariants.  The Tree GUI
     contains a tree which hierarchically organizes program points
     according to their class and method.  Using the GUI, you can look
     at invariants for only the methods and program points you care
     about.

     java daikon.gui.treeGUI.InvariantsGUI print_tokens.inv

   - Use the Daikon Context GUI to browse the invariants.  As you move
     the cursor in an editor window, the Context GUI displays the
     invariants applicable to the current location (class or
     method).  For details on running the Context GUI, see the Daikon
     manual.  The Context GUI can also be run from the command line:

     java daikon.gui.contextGUI.ContextGUI print_tokens.inv


SUMMARY

The above steps can be divided into three stages:

Instrumentation (Steps 1-3)
   cd $EXAMPLES/print_tokens
   cp $DFECDIR/daikon_runtime.h .
   ${DFEC} print_tokens.c stream.h tokens.h
   cd daikon-instrumented
   sed -f $DFECDIR/fix.sed print_tokens.cc > print_tokens_fixed.cc
   cd ..
   g++ -w -o print_tokens.exe daikon-instrumented/print_tokens_fixed.cc \
     $DFECDIR/daikon_runtime.o

Trace File Generation (Step 4)
   sh tests.sh

Invariant Detection (Steps 5-6)
   java -Xmx256m daikon.Daikon -o print_tokens.inv \
     daikon-output/print_tokens.decls daikon-output/print_tokens.dtrace
   java daikon.gui.treeGUI.InvariantsGUI StackAr.inv


UNDERSTANDING THE INVARIANTS

For help understanding the invariants, see the Daikon manual.


RIJNDAEL EXAMPLE

The rijndael program requires some special instructions, due to the
size of its data trace file.  The data trace file is over 1GB in size
when first created, so make sure you have adequate disk space.

First, it is recommended that you set the TRACE_KAT_MCT variable when
you run the C front end, so you will see some output when running the
test suite.

2.  $DFECDIR/dfec -w -I$DFECDIR -DTRACE_KAT_MCT rijndael.c \
      rijndael-alg-ref.h rijndael-api-ref.h

Second, you will need to manually reduce the size of the dtrace file.
Run these commands after step 4 above.

4b. head -1000000 daikon-output/rijndael.dtrace > \
      daikon-output/rijndael.truncated.dtrace
    ../trace-untruncate daikon-output/rijndael.truncated.dtrace

The first command selects the first million lines of the file, and the
second command cleans up the end of the file, in case we truncate in
the middle of a data structure.

Finally, run Daikon using the truncated trace file.

5.  java -Xmx256m daikon.Daikon -o rijndael.inv \
      daikon-output/rijndael.decls daikon-output/rijndael.truncated.dtrace
