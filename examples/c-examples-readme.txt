This directory contains several example C programs, to illustrate the
use of Daikon and the C front end.

STEP-BY-STEP INSTRUCTIONS

******************** INSTALLATION ********************

(0) For Windows users only:  To use Daikon on Windows, you must install the
Cywin utilities:  http://sources.redhat.com/cygwin/

(1) First, install the Daikon invariant detector.  This is
documented in the Daikon manual.  http://pag.lcs.mit.edu/daikon/download/

(2) In addition to the Daikon engine, you also need a front end.  The front
end for C is dfec.  Obtain dfec from http://pag.lcs.mit.edu/daikon/download/.
The Daikon manual describes how to install it.

(3) Set the DTRACEAPPEND environment variable.  The bash syntax is shown
here ("$" is the shell prompt).

$ export DTRACEAPPEND=1

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

$ dfec print_tokens.c stream.h tokens.h

We instrument the source file and all the user-created header files it
depends on.  This command creates two directories, daikon-instrumented
and daikon-output.  It creates an instrumented and preprocessed
version of print_tokens.c at daikon-instrumented/print_tokens.cc.  It
creates a declaration file at daikon-output/print_tokens.decls.

##### Windows only
  Now, under Windows, you have to then fix the instrumented file with a
  sed script to put in a gcc-specific __attribute__ for the _ctype_
  variable, which is used in our test suite.  Run it like this:

  $ alias fix="sed -f $DFECDIR/fix.sed"
  $ (cd daikon-instrumented; fix print_tokens.cc > print_tokens_fixed.cc)

  This fixes the above problem.
##### end of Windows only

3. Compile and link the instrumented program.

$ g++ -w -o print_tokens.exe daikon-instrumented/print_tokens_fixed.cc \
     $DFECDIR/daikon_runtime.o

This creates the executable print_tokens.exe in the current directory.

4. Run the print_tokens test suite.

$ sh tests.sh

This creates a data trace file at daikon-output/print_tokens.dtrace.

5. Run Daikon on the trace file.

$ java -Xmx256m daikon.Daikon -o print_tokens.inv \
     daikon-output/print_tokens.decls daikon-output/print_tokens.dtrace

The invariants are printed to standard out, and a binary representation
of the invariants is written to print_tokens.inv.

6. Examine the invariants.  As described in the Daikon manual, there are
several ways to do this:
   - Examine the output from running Daikon.
   - Use the PrintInvariants program to display the invariants.
   - Use the Daikon Tree GUI to browse the invariants.
   - Use the Daikon Context GUI to browse the invariants.
For help understanding the invariants, see the Daikon manual.


SUMMARY

The above steps can be divided into three stages:

Instrumentation (Steps 1-3)
   cd $EXAMPLES/print_tokens
   cp $DFECDIR/daikon_runtime.h .
   dfec print_tokens.c stream.h tokens.h
     ##### Windows only   
     (cd daikon-instrumented; fix print_tokens.cc > print_tokens_fixed.cc)
     ##### end of Windows only   
   g++ -w -o print_tokens.exe daikon-instrumented/print_tokens_fixed.cc \
     $DFECDIR/daikon_runtime.o

Trace File Generation (Step 4)
   sh tests.sh

Invariant Detection (Steps 5-6)
   java -Xmx256m daikon.Daikon -o print_tokens.inv \
     daikon-output/print_tokens.decls daikon-output/print_tokens.dtrace
   java daikon.gui.treeGUI.InvariantsGUI StackAr.inv


RIJNDAEL EXAMPLE

The rijndael program requires some special instructions, due to the
size of its data trace file.  The data trace file is over 1GB in size
when first created, so make sure you have adequate disk space.

First, it is recommended that you set the TRACE_KAT_MCT variable (this is
used by the Rijndael program) when you run the C front end, so you will see
some output when running the test suite.

2.  dfec -DTRACE_KAT_MCT rijndael.c \
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
