package daikon.tools.jtb;

import java.io.*;
import gnu.getopt.*;
import org.apache.log4j.Category;
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

class CreateSpinfo {

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

  public static final Category debug = Category.getInstance("daikon.tools.jtb.CreateSpinfo");

  private static String usage =
    UtilMDE.join(new String[] {
      "Usage:  java daikon.tools.CreateSpinfo FILE.java ...",
      "  -h   Display this usage message",
    },
		 lineSep);


  public static void main (String[] args) throws Exception {

    daikon.Logger.setupLogs (daikon.Logger.INFO);
    LongOpt[] longopts = new LongOpt[] {
      new LongOpt(Daikon.debugAll_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.debug_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
    };

    Getopt g = new Getopt("daikon.tools.jtb.CreateSpinfo", args, "hs", longopts);
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

    for ( ; argindex < args.length; argindex++) {

      String javafile = args[argindex];
      Reader input = new FileReader(javafile);

      if (javafile.endsWith(".java")) {
	javafile = javafile.substring(0, javafile.length()-5) + ".spinfo";
      }


      // If the file does not appear to be a .java file, then proceed but
      // provide a warning.
      else {

        System.out.println ("Warning, CreateSpinfo is only supported for Java source code! \nYou are getting this message because the input file does not end in .java.");

        // change the file extension to .spinfo
        if (javafile.indexOf (".") != -1) {
          javafile = javafile.substring (0, javafile.lastIndexOf (".")) + ".spinfo";
        }

        else {
          javafile = javafile + ".spinfo";
        }
      }

      System.out.println("Splitter Info file => " + javafile);
      File outputFile = new File(javafile);
      Writer output = new FileWriter(outputFile);

      JavaParser parser = new JavaParser(input);
      Node root = null;

      try {
	root = parser.CompilationUnit();
      } catch (ParseException e) {
	e.printStackTrace();
	System.exit(1);
      }
      debug.debug("CreateSpinfo: processing file " + javafile);
      ConditionExtractor extractor = new ConditionExtractor();
      root.accept(extractor);
      extractor.printSpinfoFile(output);
      output.flush();
      output.close();
    }
  }
}
