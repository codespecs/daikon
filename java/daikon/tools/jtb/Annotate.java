package daikon.tools.jtb;

import daikon.*;
import daikon.inv.filter.JMLCompilerWorkaroundFilter;
import daikon.inv.Invariant.OutputFormat;
import utilMDE.*;
import gnu.getopt.*;
import java.util.logging.Logger;
import java.io.*;

/**
 * Annotate Daikon-generated invariants into Java source code as ESC (or DBC, or JML) annotations.
 * <p>
 *
 * The first argument is a Daikon .inv file -- a serialized file of
 * Invariant objects.  All subsequent arguments are .java files that are
 * rewritten into -escannotated versions; alternately, use the -r flag to
 * process every .java file under the current directory.  (All original
 * .java files are left unmodified.)
 * <p>
 **/
class Annotate {

  // CP: This documentation needs to be updated.
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
  //  * With -i flag, invariants not supported by ESC/DBC/JML are inserted with "!"
  //    instead of "@"; by default these "inexpressible" invariants are
  //    simply omitted.
  //  * With -s flag, use // comments; by default, use /* comments.

  public final static String lineSep = System.getProperty("line.separator");

  public static final Logger debug = Logger.getLogger("daikon.tools.jtb.Annotate");

  public static final String useJML_SWITCH = "jml_output";
  public static final String useESC_SWITCH = "esc_output";
  public static final String useDBC_SWITCH = "dbc_output";
  public static final String useNOCLASSIFY_SWITCH = "noclassify";
  public static final String useNEGATE_IS_SWITCH = "negateIS";
  public static final String useNEGATE_OS_SWITCH = "negateOS";
  public static final String useNEGATE_IO_SWITCH = "negateIO";
  public static final String useOMIT_IS_SWITCH = "omitIS";
  public static final String useOMIT_OS_SWITCH = "omitOS";
  public static final String useOMIT_IO_SWITCH = "omitIO";

  private static String usage =
    UtilMDE.join(new String[] {
      "Usage:  java daikon.tools.Annotate FILE.inv FILE.java ...",
      "  -h   Display this usage message",
      "  -i   Insert invariants not supported by ESC/JML/DBC with \"!\" instead of \"@\";",
      "       by default these \"inexpressible\" invariants are simply omitted",
      "  -r   Use all .java files under the current directory as arguments",
      "  -s   Use // comments rather than /* comments",
      "",
      "  Annotation language options",
      "  (if one is not given, the default is ESC)",
      "  --jml_output Insert JML annotations",
      "  --esc_output Insert ESC annotations",
      "  --dbc_output Insert DBC annotations",
      "               This output format has the following further options:",
      "",
      "               --negateIS   negate input space conditions ( @pre )",
      "               --negateOS   negate output space conditions",
      "                            (@post containing no $pre expressions)",
      "               --negateIO   negate input-output space conditions",
      "                            (@post containing some $pre expression(s))",
      "",
      "               --omitIS     do not print input space conditions",
      "               --omitOS     do not print output space conditions",
      "               --omitIO     do not print input-output space conditions",
      "",
      "               --noclassify do not print annotations classified into",
      "                            input, output, i/o, and inexpressible",
      "                            (useful for comparing with other formats)",
    },
                 lineSep);

  public static void main(String[] args) throws Exception {
    boolean slashslash = false;
    boolean insert_inexpressible = true; // change to false when command-line
                                         // options are corrected!
    boolean setEsc = true; /* the default */
    boolean dbc = false;
    boolean noclassify = false;
    boolean negateIS = false;
    boolean negateOS = false;
    boolean negateIO = false;
    boolean omitIS = false;
    boolean omitOS = false;
    boolean omitIO = false;


    Daikon.output_style = OutputFormat.ESCJAVA;
    daikon.LogHelper.setupLogs (daikon.LogHelper.INFO);
    LongOpt[] longopts = new LongOpt[] {
      new LongOpt(Daikon.debugAll_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.debug_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(useJML_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(useESC_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(useDBC_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(useNEGATE_IS_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(useNEGATE_OS_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(useNEGATE_IO_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(useOMIT_IS_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(useOMIT_OS_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(useOMIT_IO_SWITCH, LongOpt.NO_ARGUMENT, null, 0)
    };
    Getopt g = new Getopt("daikon.tools.jtb.Annotate", args, "hs", longopts);
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
        } else if (useESC_SWITCH.equals(option_name)) {
          Daikon.output_style = OutputFormat.ESCJAVA; //cp: redundant
          setEsc = true; //cp: redundant
	  dbc = false;
        } else if (useJML_SWITCH.equals(option_name)) {
          Daikon.output_style = OutputFormat.JML;
          setEsc = false;
	  dbc = false;
          JMLCompilerWorkaroundFilter.createNextFilterOn = true;
        } else if (useDBC_SWITCH.equals(option_name)) {
          Daikon.output_style = OutputFormat.DBCJAVA;
          setEsc = false;
	  dbc = true;
        } else if (useNOCLASSIFY_SWITCH.equals(option_name)) {
          noclassify = true;
        } else if (useNEGATE_IS_SWITCH.equals(option_name)) {
          negateIS = true;
        } else if (useNEGATE_OS_SWITCH.equals(option_name)) {
          negateOS = true;
        } else if (useNEGATE_IO_SWITCH.equals(option_name)) {
          negateIO = true;
        } else if (useOMIT_IS_SWITCH.equals(option_name)) {
          omitIS = true;
        } else if (useOMIT_OS_SWITCH.equals(option_name)) {
          omitOS = true;
        } else if (useOMIT_IO_SWITCH.equals(option_name)) {
          omitIO = true;
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
      File outputFile = null;
      if (Daikon.output_style == OutputFormat.ESCJAVA)
        outputFile = new File(javafile + "-escannotated");
      else if (Daikon.output_style == OutputFormat.DBCJAVA) {
        outputFile = new File(javafile + "-dbcannotated");
      }
      Writer output = new FileWriter(outputFile);

      debug.fine ("Processing file " + javafile);

      // Annotate the file
      Ast.applyVisitorInsertComments(input,
                                     output,
                                     new AnnotateVisitor(ppts, slashslash, insert_inexpressible,
                                                         setEsc, dbc,
                                                         noclassify,
                                                         negateIS, negateOS, negateIO,
                                                         omitIS, omitOS, omitIO));
    }
  }


}
