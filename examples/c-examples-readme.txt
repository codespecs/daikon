This directory contains several example C programs, to illustrate the
use of Daikon and the C front end.

STEP-BY-STEP INSTRUCTIONS

These instructions assume the following:

- The examples are located in the directory $EXAMPLES.  This is the
  directory where the c-examples.tar.gz was unpacked.  This, and all
  other paths discussed in these instrucctions, should be in terms of
  the cywgin file namespace.
  (e.g. /cygdrive/c/mydir/file.txt not c:\mydir\file.txt).

- Daikon is installed
** Where do I find out how to do this?

As part of an installation step, you should make sure that the C front
end, dfec, is installed.  This means that the dfec.zip file has been
unzipped there, and the following setup steps have been taken to match
it to your system.

** What does installed mean?  Where do I find out how to install this?
** Unzip dfec.zip
** Then also do the environmental setup instructions?

Then, discover the include path by running g++..

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

I set INC_PATH to be everything following an -isystem in the cpp0.exe invocation above, INC_AFTER to be everything following an -idirafter, and INC_SEARCH to be the search path after the "#include <...> search starts here:", yielding this:

** Put double quotes

export INC_PATH=-I/usr/local/include -I/usr/include/g++-3 -I/usr/include/g++
export INC_AFTER=-I/usr/include -I/usr/include/w32api
export INC_SEARCH=-I/usr/include/g++-3 -I/usr/lib/gcc-lib/i686-pc-cygwin/2.95.3-4/include -I/usr/include -I/usr/include/w32api

Then, I set DFEC to be the correct dfec, plus include paths (note the order!):

** Is this path in cygwin?  Yes.
** Put double quotes.

export DFEC=path/to/dfec ${INC_PATH} ${INC_SEARCH} ${INC_AFTER}

Then, to invoke dfec from the cygwin bash shell, you can call ${DFEC}.
There are a lot of defines that need to get sent to dfec to get it to
process the system headers correctly.  Unfortunately, I haven't been
able to set them into an environment variable yet because of the space
in "unsigned int".

$ ${DFEC} -w -D__SIZE_TYPE__="unsigned int" -D__attribute__\(x\)="" -D__extension__="" -D__null=0 -D__GNUG__=1 -D_WIN32 programname.c a.h another.h


To instrument and compile a program, first copy daikon_runtime.h into the directory that your source file resides in.


When programname.c is instrumented (note that you have to specify the
.h files so that dfec knows to correctly instrument functions whose
prototypes appear in those headers), it creates
daikon-instrumented/programname.cc.  For Windows, you have to then fix
this with a sed script to put in a gcc-specific __attribute__ for the
_ctype_ variable, which is used in our test suite.  Run it like this:

$ sed -f path/to/fix.sed daikon-instrumented/programname.cc > daikon-instrumented/programname_fixed.cc

This should make it kosher.  Then, compile:

$ g++ -w -o programname daikon-instrumented/programname_fixed.cc path/to/daikon_runtime.o

You should then be able to run programname and get trace data.

The c front end (dfec) is installed in $DFECDIR.  

You must perform two steps to complete the installation of the C front
end.

0a. Compile the daikon runtime library.

    cd $DFECDIR; gcc -c daikon_runtime.cc
** Where am I typing this?  DOS?  Cygwin?

    This generates the file daikon_runtime.o in $DFECDIR.

0b. Set the DTRACEAPPEND environment variable.  The bash syntax is
    shown here.

    export DTRACEAPPEND=1


To detect invariants for a program, you need to perform three basic
tasks: instrument the target program (steps 1-3), run the instrumented
program to create a data trace file (step 4), and run Daikon over the
data trace file to produce invariants (steps 5-6).  The following
instructions are for the print_tokens example.  The other examples can
be run in exactly the same manner.

1. Change to the directory containing the print_tokens program.

   cd $EXAMPLES/print_tokens

2. Instrument the program using dfec, the C front end.

   $DFECDIR/dfec -w -I$DFECDIR print_tokens.c stream.h tokens.h

** "The procedure entry point setrlimit could not be located in the dynamic link library cygwin1.dll"
** ^^ indicates you need to upgrade cygwin

   We instrument the source file and all the user-created header files
   it depends on.  This command creates two directories, daikon-instrumented
   and daikon-output.  It creates an instrumented and preprocessed version
   of print_tokens.c at daikon-instrumented/print_tokens.cc.  It creates a
   declaration file at daikon-output/print_tokens.decls.

3. Compile and link the instrumented program.

   g++ -w -o print_tokens.exe daikon-instrumented/print_tokens.cc \
     $DFECDIR/daikon_runtime.o

   This creates the executable print_tokens.exe in the current directory.

4. Run the print_tokens test suite.

   sh tests.sh

   This creates a data trace file at daikon-output/print_tokens.dtrace.

5. Run Daikon on the trace file.

   java -Xmx256m daikon.Daikon -o print_tokens.inv \
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
   $DFECDIR/dfec -w -I$DFECDIR print_tokens.c stream.h tokens.h
   g++ -w -o print_tokens.exe daikon-instrumented/print_tokens.cc \
     $DFECDIR/daikon_runtime.o

Trace File Generation (Step 4)
   sh tests.sh

Invariant Detection (Steps 5-6)
   java -Xmx256m daikon.Daikon -o print_tokens.inv \
     daikon-output/print_tokens.decls daikon-output/print_tokens.dtrace
   java daikon.gui.treeGUI.InvariantsGUI StackAr.inv


UNDERSTANDING THE INVARIANTS

For help understanding the invariants, see the Daikon manual.
