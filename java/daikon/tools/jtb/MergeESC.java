package daikon.tools.jtb;

import java.io.*;
import gnu.getopt.*;
import org.apache.log4j.Category;
import daikon.*;
import utilMDE.*;
import jtb.syntaxtree.*;
import jtb.JavaParser;
import jtb.ParseException;
import jtb.visitor.*;

// To do:  permit multiple .inv files as arguments.

/**
 * Merge Daikon-generated invariants into Java source code as ESC annotations.
 * <p>
 *
 * The first argument is a Daikon .inv file -- a serialized file of
 * Invariant objects.  All subsequent arguments are .java files that are
 * rewritten into -escannotated versions.
 * <p>
 **/
class MergeESC {

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
  //    overrides another method.  Examples include
  //    equals(java.lang.Object), toString().

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
  //  * Invariants not supported by ESC are inserted with "!" instead of "@";
  //    by default these "inexpressible" invariants are simply omitted.
  //  * Whether to use // or /* comments.

  public final static String lineSep = System.getProperty("line.separator");

  public static final Category debug = Category.getInstance(MergeESC.class.getName());

  private static String usage =
    UtilMDE.join(new String[] {
      "Usage:  java daikon.tools.MergeESC FILE.inv FILE.java ...",
      "  -h   Display this usage message",
      "  -i   Insert invariants not supported by ESC with \"!\" instead of \"@\";",
      "       by default these \"inexpressible\" invariants are simply omitted",
      "  -r   Use all .java files under the current directory as arguments",
      "  -s   Use // comments rather than /* comments",
    },
                 lineSep);

  public static void main(String[] args) throws Exception {
    boolean slashslash = true;  // temporarily default to true
    boolean insert_inexpressible = false;

    daikon.Logger.setupLogs (daikon.Logger.INFO);
    LongOpt[] longopts = new LongOpt[] {
      new LongOpt(Daikon.debugAll_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.debug_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
    };
    Getopt g = new Getopt("daikon.PrintInvariants", args, "hs", longopts);
    int c;
    while ((c = g.getopt()) != -1) {
      switch(c) {
      case 0:
        // got a long option
        String option_name = longopts[g.getLongind()].getName();
	if (Daikon.debugAll_SWITCH.equals(option_name)) {
	  Global.debugAll = true;
	} else if (Daikon.debug_SWITCH.equals(option_name)) {
	  Logger.setPriority (g.getOptarg(), Logger.DEBUG);
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
      //   // an easy way to do this in Java.
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

    Daikon.output_style = Daikon.OUTPUT_STYLE_ESC;
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
      File outputFile = new File(javafile + "-escannotated");
      // outputFile.getParentFile().mkdirs();
      Writer output = new FileWriter(outputFile);

      debug.debug("Processing file " + javafile);

      // Annotate the file
      applyVisitor(input, output,
                   new MergeESCVisitor(ppts, slashslash, insert_inexpressible));
    }
  }

  // Reads an AST from the input stream, applies the visitor to the
  // AST, and writes the resulting AST to the output stream
  public static void applyVisitor(Reader input, Writer output,
                                  MergeESCVisitor visitor) {
    JavaParser parser = new JavaParser(input);
    Node root = null;
    try {
      root = parser.CompilationUnit();
    }
    catch (ParseException e) {
      e.printStackTrace();
      System.exit(1);
    }
    root.accept(visitor);
    root.accept(new InsertCommentFormatter(visitor.addedComments));
    root.accept(new TreeDumper(output));
  }

}

