package daikon.tools.jtb;

import java.io.*;
import gnu.getopt.*;
import java.util.logging.Logger;
import java.util.logging.Level;
import daikon.*;
import jtb.syntaxtree.*;
import jtb.JavaParser;
import jtb.ParseException;
import jtb.visitor.*;
import utilMDE.*;

/**
 * Create a splitter info file from Java source.
 * <p>
 *
 * The argument is a list of .java files.  The original .java files are
 * left unmodified.  A .spinfo file is written for every .java file.
 */

public class CreateSpinfo {

// The expressions in the Java source are extracted as follows:
// For each method:
//  * extracts all expressions in conditional statements
//    ie. if, for, which, etc.
//  * if the method body is a one-line return statement, it
//    extracts it for later substitution into expressions which
//    call this function. These statements are referred to as
//    replace statements
// For each field declaration
//  * if the field is a boolean, it stores the expression
//    "<fieldname> == true" as a splitting condition.
//
//  The method printSpinfoFile prints out these expressions and
//  replace statements in splitter info file format.

  public final static String lineSep = System.getProperty("line.separator");

  public static final Logger debug = Logger.getLogger("daikon.tools.jtb.CreateSpinfo");

  private static String usage =
    UtilMDE.join(new String[] {
      "Usage:  java daikon.tools.CreateSpinfo FILE.java ...",
      "  -o outputfile   Put all output in specified file",
      "  -h              Display this usage message",
    },
                 lineSep);


  public static void main (String[] args) throws IOException {

    // If not set, put output in files named after the input (source) files.
    String outputfilename = null;

    daikon.LogHelper.setupLogs (daikon.LogHelper.INFO);
    LongOpt[] longopts = new LongOpt[] {
      new LongOpt(Daikon.debugAll_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.debug_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
    };

    Getopt g = new Getopt("daikon.tools.jtb.CreateSpinfo", args, "ho:", longopts);
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
      case 'o':
        outputfilename = g.getOptarg();
        break;
      case 'h':
        System.out.println(usage);
        System.exit(1);
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
      System.out.println("Error: No .java file arguments supplied.");
      System.out.println(usage);
      System.exit(1);
    }

    if (outputfilename != null) {
      Writer output = new FileWriter(outputfilename);
      for ( ; argindex < args.length; argindex++) {
        String javaFileName = args[argindex];
        writeSplitters(javaFileName, output);
      }
      output.flush();
      output.close();
    } else {
      for ( ; argindex < args.length; argindex++) {
        String javaFileName = args[argindex];

        String spinfoFileName = spinfoFileName(javaFileName);
        // System.out.println("Splitter Info file => " + spinfoFileName);
        Writer output = new FileWriter(spinfoFileName);

        writeSplitters(javaFileName, output);
        output.flush();
        output.close();
      }
    }
  }

  private static String spinfoFileName(String javaFileName) {
    if (javaFileName.endsWith(".java")) {
      return javaFileName.substring(0, javaFileName.length()-5) + ".spinfo";
    }

    // The file does not end with ".java".  Proceed, but issue a warning.
    System.err.println ("Warning: CreateSpinfo input file " + javaFileName + "does not end in .java.");

    // change the file extension to .spinfo
    int dotPos = javaFileName.indexOf (".");

    if (dotPos == -1) {
      return javaFileName + ".spinfo";
    } else {
      return javaFileName.substring (0, dotPos) + ".spinfo";
    }
  }

  /** Write splitters for the Java file to the Writer. **/
  private static void writeSplitters(String javaFileName, Writer output) throws IOException {
    Reader input = new FileReader(javaFileName);
    JavaParser parser = new JavaParser(input);

    Node root = null;
    try {
      root = parser.CompilationUnit();
    } catch (ParseException e) {
      e.printStackTrace();
      System.exit(1);
    }
    debug.fine ("CreateSpinfo: processing file " + javaFileName);
    ConditionExtractor extractor = new ConditionExtractor();
    root.accept(extractor);
    // transform conditions
    // ... convert to String
    extractor.printSpinfoFile(output);
  }

}
