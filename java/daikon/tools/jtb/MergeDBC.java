/* This file is temporary. 
 * The functionality in here will be incorporated in MergeESC.java.
 * However, my current attempt at an integrated Merge class is horrible spaghetti.
 * So for the moment I'm leaving MergeESC.java and MergeESCVisitor alone, and 
 * keeping the mess here. -RRN
 */  

package daikon.tools.jtb;

import daikon.*;
import daikon.inv.Invariant.OutputFormat;
import utilMDE.*;
import gnu.getopt.*;
import java.util.logging.Logger;
import java.io.*;

/**
 * Merge Daikon-generated invariants into Java source code as DBC annotations.
 * <p>
 *
 * The first argument is a Daikon .inv file -- a serialized file of
 * Invariant objects.  All subsequent arguments are .java files that are
 * rewritten into -dbcannotated versions; alternately, use the -r flag to
 * process every .java file under the current directory.  (All original
 * .java files are left unmodified.)
 * <p>
 **/
class MergeDBC {

  // Invariants are inserted as follows:
  //  * invariants at method entry become "requires"
  //  * invariants at method exit become "ensures".
  //    ensures/exsures annotations should not mention orig(primitive arguments).
  //  * invariants at method exceptional exit become "exsures"
  //  * object invariants become object invariants, inserted at the beginning
  //    of the java class
  //  * "modifies" clauses are inserted for changed variables.
  //    Use "a[*]", not "a[]" or "a[i]" or "a[i..]" or "a[...].field", for arrays;
  //    consider adding a \forall to specify which indices are *not* changed.
  //    Modifies clauses should not contain "size()", ".class", or "~".
  //    Modifies clauses should not contain final fields.
  //  * use "also_requires", "also_ensures", "also_modifies" if this method
  //    overrides another method/interface.

  // spec_public:
  // for each non-public field, add "/*@ spec_public */" to its declaration

  // owner annotations:
  // for each private field that is set via a call to new in the constructor
  // (in the short term, just do this for all array fields):
  //  * add object invariant "invariant field.owner == this"
  //  * whenever the field is set, "set field.owner = this"

  // Explanatory comments:
  // Handle "The invariant on the following line means:".

  // Optional behavior:
  //  * With -i flag, invariants not supported by DBC are inserted with "!"
  //    instead of "@"; by default these "inexpressible" invariants are
  //    simply omitted.
  //  * With -s flag, use // comments; by default, use /* comments.

  public final static String lineSep = System.getProperty("line.separator");

  public static final Logger debug = Logger.getLogger("daikon.tools.jtb.MergeDBC");
 
  private static String usage =
    UtilMDE.join(new String[] {
      "Usage:  java daikon.tools.MergeDBC FILE.inv FILE.java ...",
      "  -h   Display this usage message",
      "  -i   Insert invariants not supported by DBC with \"!\" instead of \"@\";",
      "       by default these \"inexpressible\" invariants are simply omitted",
      "  -r   Use all .java files under the current directory as arguments",
      "  -s   Use // comments rather than /* comments",
    },
                 lineSep);

  public static void main(String[] args) throws Exception {
    boolean slashslash = false;
    boolean insert_inexpressible = false;

    Daikon.output_style = OutputFormat.DBCJAVA;   
    daikon.LogHelper.setupLogs (daikon.LogHelper.INFO);
    LongOpt[] longopts = new LongOpt[] {
      new LongOpt(Daikon.debugAll_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.debug_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
    };
    Getopt g = new Getopt("daikon.tools.jtb.MergeDBC", args, "hs", longopts);
    int c;
    while ((c = g.getopt()) != -1) {
      switch(c) {
      case 0:
        // got a long option
        String option_name = longopts[g.getLongind()].getName();
        if (Daikon.debugAll_SWITCH.equals(option_name)) {
          Global.debugAll = true;
        } else if (Daikon.debug_SWITCH.equals(option_name)) {
          LogHelper.setLevel (g.getOptarg(), LogHelper.FINE);                
        } else {
          throw new RuntimeException("Unknown long option received: " +
                                     option_name);
        }
        break;
      case 'h':
        System.out.println(usage);
        System.exit(1);
        break;
      case 'i':
        insert_inexpressible = true;
        break;
      // case 'r':
      //   // Should do this witout calling out to the system.  (There must be
      //   // an easy way to do this in Java.)
      //   Process p = System.exec("find . -type f -name '*.java' -print");
      //   p.waitFor();
      //   StringBufferInputStream sbis
      //   break;
      case 's':
        slashslash = true;
        break;
      case '?':
        break; // getopt() already printed an error
      default:
        System.out.println("getopt() returned " + c);
        break;
      }
    }
    // The index of the first non-option argument -- the name of the file
    int argindex = g.getOptind();
    if (argindex >= args.length) {
      System.out.println("Error: No .inv file or .java file arguments supplied.");
      System.out.println(usage);
      System.exit(1);
    }
    String invfile = args[argindex];
    argindex++;
    if (argindex >= args.length) {
      System.out.println("Error: No .java file arguments supplied.");
      System.out.println(usage);
      System.exit(1);
    }
    PptMap ppts = FileIO.read_serialized_pptmap(new File(invfile),
                                                true // use saved config
                                                );

    Daikon.suppress_implied_controlled_invariants = true;
    Daikon.suppress_implied_postcondition_over_prestate_invariants = true;
    Daikon.suppress_redundant_invariants_with_simplify = true;

    for ( ; argindex < args.length; argindex++) {
      String javafile = args[argindex];
      if (! javafile.endsWith(".java")) {
        System.out.println("File does not end in .java: " + javafile);
        System.exit(1);
      }
      Reader input = new FileReader(javafile);
      File outputFile;
      outputFile = new File(javafile + "-dbcannotated");

      // outputFile.getParentFile().mkdirs();
      Writer output = new FileWriter(outputFile);

      debug.fine ("Processing file " + javafile);

      // Annotate the file
      Ast.applyVisitorInsertComments(input, output,
                   new MergeDBCVisitor(ppts, slashslash, insert_inexpressible));
    }
  }


}
