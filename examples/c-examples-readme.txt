This directory contains several example C programs, to illustrate the
use of Daikon and the C front end.

STEP-BY-STEP INSTRUCTIONS

These instructions assume the following:

- The examples are located in the directory $EXAMPLES
** Say this is where the tar.gz was unpacked.
** What path format or namespace is this?  Cygwin or windows?

- The c front end (dfec) is installed in $DFECDIR
** What does installed mean?  Where do I find out how to install this?
** Unzip dfec.zip
** Then also do the environmental setup instructions?

- Daikon is installed
** Where do I find out how to do this?

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
