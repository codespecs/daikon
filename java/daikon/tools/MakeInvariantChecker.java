package daikon.tools;

import daikon.*;
import daikon.inv.*;
import daikon.inv.Invariant.OutputFormat;
import daikon.inv.filter.*;

import utilMDE.*;
import gnu.getopt.*;

import java.io.*;
import java.util.*;
import java.util.logging.*;

/**
 *   MakeInvariantChecker creates a java program that checks if the
 *   invariants of a program or data hold on newly seen program point
 *   samples. <br> MakeInvariantChecker uses Daikon's output (.inv) files
 *   to generate the invariant checking program. This program (after
 *   being compiled) can be used to check if new data traces seen
 *   obey previously derived invariants.
 *   <ul>
 *   Example usage:
 *   <li>    java daikon.tools.MakeInvariantChecker myProgram.inv TestMyProgram.java</li>
 *   <li>    javac TestMyProgram.java </li>
 *   <li>    java TestMyProgram new_trace_file.dtrace </li>
 *   </ul>
 */

public final class MakeInvariantChecker {

  // FIELD VARIABLES

  public static final Logger debug
    = Logger.getLogger ("daikon.tools.MakeInvariantChecker");

  /**
   * Maps invalid variable names to valid variable names.
   * Invalid java identifier names such as "this.theArray[1...length]"
   * or "202.167.125.21 Requests per second " are modified by
   * replacing every invalid character with its ascii code. This HashMap
   * contains the actual variable name (e.g. "this.theArray[1...length]") as
   * the key, and the modified variable name (e.g. "var_this46theArray451464646length45")
   * as the value.
   */
  protected static HashMap varNameMap;


  /**
   * Set of procedure names that are used in the output java file.
   * Used for checking whether a procedure name exists when
   * a procedure needs to be broken down to into two for
   * space considerations.
   * See private method createOutputFile.
   */
  protected static Set procedureNameSet;


  /**
   * Some invariants might not be available in java_output_format.
   * Keep a list of them for debugging purposes.
   */
  protected static List unImplementedInvariants;


  // SWITCHES

  /** Specifies the behavior for "assertions". */
  public static String assertion_behavior_SWITCH = "assertion_behavior";

  /** Defines which filters to use when eliminating invariants. */
  public static String filter_behavior_SWITCH = "filter_behavior";



  /** List of command line options. */
  protected static String flagList =
    UtilMDE.join (new String []
      {"    " + assertion_behavior_SWITCH +
      " -- defines the method to be called to check invariants.",
      filter_behavior_SWITCH +
      " -- determines what filters will be used in eliminating invariants.",
      "  --" + Daikon.config_option_SWITCH + " config_var=val",
      "      Sets the specified configuration variable.  ",
      "  --" + Daikon.debugAll_SWITCH,
      "      Turns on all debug flags (voluminous output)",
      "  --" + Daikon.debug_SWITCH + " logger",
      "      Turns on the specified debug logger",
      "  --" + Daikon.track_SWITCH + " class<var1,var2,var3>@ppt",
      "      Print debug info on the specified invariant class, vars, and ppt",
      },
      Daikon.lineSep + "    ");

  /** Usage string. */
  protected static String usage =
    UtilMDE.join(new String[] {
      "Daikon invariant checker ",
      "Usage:",
      "    java daikon.tools.MakeInvariantChecker [flags...] files...",
      "",
      "     Each file is a .inv file or a java file; the file type",
      "     is determined by the file name (containing \".inv\" or \".java\").",
      "     List of flags : ",
      flagList},
                 Daikon.lineSep);



  /**
   *  Generates invariant checking programs if command line arguments
   *  are valid (see procedure readOptions for details).
   *
   *  @param args - command line arguments.
   */
  public static void main(String[] args) {

    // Read command line options
    Set[] files = readOptions(args);
    Set inv_files = files[0];
    Set java_files = files[1];

    // Input validation
    if ((inv_files.size() == 0) || (java_files.size()==0)) {
      System.out.println("No .inv or .java file was specified");
      System.exit(1);
    }
    else if (inv_files.size() != java_files.size())
      System.out.println("No. of .inv files must be equal to no. of java files");
    try {
      Iterator outputFiles = java_files.iterator();
      Iterator inputFiles = inv_files.iterator();
      while (outputFiles.hasNext()) {
        File outputFile = (File) outputFiles.next();
        File inputFile = (File) inputFiles.next();
        HashMap invariants = processInvariants(inputFile);
        createOutputFile(invariants, outputFile, inputFile);
        System.out.println("Created "  + outputFile.toString() + "...");
      }
      System.out.println("Finished creating files. Exiting...");

      // Prints a list of unimplemented invariants.
      // Useful for debugging purposes.
      if (unImplementedInvariants != null) {
        System.out.println(UtilMDE.join(new String [] {
          "",
          "Following invariants are not included since they are not implemented",
          "in the java output format. To include these invariants in the resulting",
          "Java file, please go to the shown file and do the necessary changes."}, Daikon.lineSep));

        Iterator unImpIter = unImplementedInvariants.iterator();
        while(unImpIter.hasNext()) {
          String inv = (String) unImpIter.next();
          System.out.println(inv);
        }
      }
    }
    catch (IOException e) {
      System.out.println(e.toString());
      e.printStackTrace();
    }
    catch (ClassNotFoundException e) {
      System.out.println(e.toString());
      e.printStackTrace();
    }
  }


  /**
   * Parses command line options.  Returns an array containing two
   * Sets. First set contains the names of invariant files. Second set
   * contains the names of java files (files to be generated).
   * Java files should end with suffix ".java", whereas
   * invariant files generated by daikon should contain ".inv".
   * The order of the file names does not matter. If more than one pair
   * of .java & .inv are supplied, the procedure will match
   * each .inv file to a .java file in the order they are received.
   * <br>
   * If no command line argument is supplied, exits
   * with error message.
   * <br>
   * If the number of ".java" and ".inv" files are not equal,
   * exits with error message.
   *
   *
   * @throws RuntimeException() - if an unknown long option is received
   * @throws Error() - if ".inv" files cannot be read, or ".java"
   * files cannot be created,
   * <br>
   * Also sets assertion_behavior_SWITCH and filter_behavior_SWITCH
   * if associated arguments are supplied at command line.
   *     <ul>
   *           <li> --filter_behavior  [none, all]
   *           <li> --assertion_behavior [none]
   *     </ul>
   */
  protected static Set[] readOptions(String[] args) {
    if (args.length == 0) {
      System.out.println("MakeInvariantChecker error: "
                          + "no files supplied on command line.");
      System.out.println(usage);
      System.exit(1);
    }

    Set inv_files = new HashSet();
    Set java_files = new HashSet();

    LongOpt[] longopts = new LongOpt[] {
      new LongOpt(Daikon.help_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(assertion_behavior_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(filter_behavior_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(Daikon.config_option_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(Daikon.debugAll_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.debug_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(Daikon.track_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
    };
    Getopt g = new Getopt("daikon.tools.MakeInvariantChecker", args, "ho:", longopts);
    int c;

    while ((c = g.getopt()) != -1) {
      switch(c) {
      case 0:
        // got a long option
        String option_name = longopts[g.getLongind()].getName();
        if (Daikon.help_SWITCH.equals(option_name)) {
          System.out.println(usage);
          System.exit(1);
        }
        else if (assertion_behavior_SWITCH.equals(option_name)) {
          try {
            System.out.println("Assertion behavior not implemented.");
          } catch (Exception e) {
            throw new Error(e.toString());
          }
          break;
        } else if (filter_behavior_SWITCH.equals(option_name)) {
          try {
            System.out.println("Filtering behavior not implemented.");
          } catch (Exception e) {
            throw new Error(e.toString());
          }
          break;
        } else if (Daikon.config_option_SWITCH.equals(option_name)) {
          String item = g.getOptarg();
          daikon.config.Configuration.getInstance().apply(item);
          break;
        } else if (Daikon.debugAll_SWITCH.equals(option_name)) {
          Global.debugAll = true;
        } else if (Daikon.debug_SWITCH.equals(option_name)) {
          LogHelper.setLevel(g.getOptarg(), LogHelper.FINE);
        } else if (Daikon.track_SWITCH.equals (option_name)) {
          LogHelper.setLevel("daikon.Debug", LogHelper.FINE);
          String error = Debug.add_track (g.getOptarg());
          if (error != null) {
            System.out.println ("Error parsing track argument '"
                                + g.getOptarg() + "' - " + error);
            System.exit(1);
          }
        } else {
          throw new RuntimeException ("Unknown long option received: "
                                      + option_name);
        }
        break;

      case 'h':
        System.out.println(usage);
        System.exit(1);
        break;
        //
      case '?':
        break; // getopt() already printed an error
        //
      default:
        System.out.print("getopt() returned " + c + Global.lineSep);
        break;
      }
    }

    for (int i=g.getOptind(); i<args.length; i++) {
      File file = new File(args[i]);

      // These aren't "endsWith()" because there might be a suffix on the end
      // (eg, a date).
      // However, this does not apply to java files. They need to
      // have a ".java" extension. use "endsWith()"

      String filename = file.toString();
      if ((! file.exists()) && (filename.indexOf(".inv") != -1)) {
        throw new Error("File " + file + " not found.");
      }

      if (filename.indexOf(".inv") != -1) {
        inv_files.add(file);
      } else if (filename.endsWith(".java")) {
        String className = replaceAllString(filename, ".java", "");
        if (! UtilMDE.canCreateAndWrite(file)) {
          throw new Error("Cannot write to file " + filename);
        }
        else if (!isValidJavaIdentifier(className)) {
          if (!hasValidJavaIdentifierStart(className))
            throw new Error("Java filename provided does not have a valid start. " + className);
          else if (!hasValidJavaIdentifierBody(className))
            throw new Error("Java filename provided does not have a valid body. " + className);
        }
        else
          java_files.add(file);
      }
      else {
        throw new Error("Unrecognized argument: " + file);
      }
    }
    return new Set[] {
      inv_files,
      java_files
    };
  }


  /**
   *  Reads in an invariant file (.inv) and extracts invariants,
   *  variables and variable types from the PptMap associated with the
   *  invariant file. Returns a HashMap with PptTopLevel objects as
   *  keys and with an array containing two elements [a List containing
   *  VarInfo objects and a List containing Invariant objects for that
   *  program point] as values.
   *
   *  @param invariantFile Daikon output to be processed
   *  @throws IOException if invariantFile cannot be read.
   *  @throws ClassNotFoundException if
   */
  private static HashMap processInvariants(File invariantFile) throws IOException, ClassNotFoundException {
    // See varNameMap declaration above for info.
    Daikon.output_style = OutputFormat.JAVA;

    if (varNameMap == null)
      varNameMap = new HashMap();

    HashMap invariantInfo = new HashMap();
    PptMap pptMap = (PptMap) FileIO.read_serialized_pptmap(invariantFile, false);

    Iterator pptIterator = pptMap.pptIterator();

    while (pptIterator.hasNext()) {
      PptTopLevel atoplevel = (PptTopLevel) pptIterator.next();
      List [] currentLists = (List []) invariantInfo.get(atoplevel);
      if (currentLists == null) {
        currentLists = new List [] {
          new ArrayList(),
          new ArrayList(), };
        invariantInfo.put(atoplevel, currentLists);
      }

      // Get the variable information (declared type and name)
      // to generate variable declaration statements.

      VarInfo[] varInfos = atoplevel.var_infos;

      for (int i = 0; i < varInfos.length; i ++) {
      VarInfo aVarInfo = (VarInfo)varInfos[i];
      VarInfoName aVarInfoName = aVarInfo.name;

      varNameMap.put(aVarInfoName.java_name(aVarInfo),
                     makeValidJavaIdentifier(aVarInfoName.java_name(aVarInfo)));
      currentLists[0].add(aVarInfo);
      }

      // Get the invariants for this PptTopLevel.
      Iterator invariants = atoplevel.invariants_iterator();
      InvariantFilters fi = new InvariantFilters();
      fi.setPptMap(pptMap);

      while(invariants.hasNext()) {
        Invariant inv = (Invariant) invariants.next();
        if (fi.shouldKeep (inv) == null)
          continue;
        if (!inv.justified())
          continue;
        debug.fine ("processing invariant " + inv.format());
        String stringInvariant = inv.format_using(Daikon.output_style);
        if (isImplementedYet (stringInvariant))
          ((List) currentLists[1]).add (inv);
      }
    }
    return invariantInfo;
  }



  /**
   * @param String type, varName, val
   * Generates statements of the form 'type varName = (type)
   * type.parsetype(val)'
   */
  private static String formatVariableInitialization(String type, String varName, String val) {
    // Primitive types such as double, int, boolean need to be parsed
    // from the string value.
    if (type.equals("int"))
      return  "                         " + type + " " +
        varName + " = (" + type + ") Integer.parseInt(" +
        val + ".toString());" + Daikon.lineSep;
    else if (type.equals("boolean"))
      return  "                         " + type + " " +
         varName + " = (" + type + ") Boolean.valueOf(" +
        val + ".toString());" + Daikon.lineSep;
    else if (type.equals("double"))
      return  "                         " + type + " " + varName +
        " = (" + type + ") Double.parseDouble("+
        val + ".toString());" + Daikon.lineSep;
    else if (type.equals("hashcode"))
      return  "                         int " +
        varName + " = (int) Integer.parseInt(" +
        val + ".toString());" + Daikon.lineSep;
    else if (type.equals("hashcode[]"))
      return  "                         int[] " +
        varName + " = (int[])"+ val+";" + Daikon.lineSep;
    else
      return  "                         " + type + " " + varName +
        " = (" + type + ") " + val + ";" + Daikon.lineSep;
 }

  private static String formatClassDeclaration (String fileName) {
    return "public final class " + fileName.substring(0, fileName.lastIndexOf(".java")) + "{" ;
  }





  /**
   * Checks whether the input string is a valid java identifier.
   * Returns true if all the characters in the string obeys:
   * <ol>
   * <li> character.isValidJavaIdentifierPart() == true &&   </li>
   * <li> the first character obeys character.isValidJavaIdentifierStart() == true </li>
   * </ol>
   * Returns false otherwise.
   */
  private static boolean isValidJavaIdentifier(String javaIdentifier) {
    if ((javaIdentifier == null) ||
        (javaIdentifier.length() == 0) ||
        (!(Character.isJavaIdentifierStart(javaIdentifier.charAt(0)))))
      return false;
    else {
      for (int i = 0; i < javaIdentifier.length() ; i++) {
        if (!Character.isJavaIdentifierPart(javaIdentifier.charAt(i)))
          return false;
      }
    }
    return true;
  }


  /**
   * Returns true if the first character of the input string obeys:
   * character.isValidJavaIdentifierPart() == true Returns false
   * otherwise.
   */
  private static boolean hasValidJavaIdentifierStart(String javaIdentifier) {
    if ((javaIdentifier == null) ||
        (javaIdentifier.length() == 0))
      return false;
    else
      return Character.isJavaIdentifierStart(javaIdentifier.charAt(0));
  }

  /**
   *  Returns true if all the characters of the input string
   *  are valid java identifier parts.
   *
   */
  private static boolean hasValidJavaIdentifierBody(String javaIdentifier) {
    if ((javaIdentifier == null) ||
        (javaIdentifier.length() == 0))
      return false;
    else {
      for (int i = 0; i < javaIdentifier.length() ; i++) {
        if (!Character.isJavaIdentifierPart(javaIdentifier.charAt(i)))
          return false;
      }
    }
    return true;
  }


  /**
   * Makes a valid java identifier name out of an invalid name.  If
   * input argument is a valid java identifier, returns the input
   * argument.
   *
   * If input argument is not valid, appends the string "v_" to the
   * input argument and replaces every invalid character with its
   * ascii code representation.
   *
   * @throws RuntimeException if input argument is null or 0 length
   */
  public static String makeValidJavaIdentifier(String javaIdentifier) {
    boolean validBody = hasValidJavaIdentifierBody(javaIdentifier);
    boolean validStart = hasValidJavaIdentifierStart(javaIdentifier);
    boolean keyword = isJavaKeyWord(javaIdentifier);
    if (validBody && validStart && !keyword) {
      return javaIdentifier;
    }
    else if (validBody && validStart && keyword)
      return "v_" + javaIdentifier;
    else if (validBody && !validStart) {
      String modid = "v_" + javaIdentifier;
      if (isValidJavaIdentifier(modid))
        return modid;
      else
        return makeValidJavaIdentifierHelper(modid);
    }
    else
      return makeValidJavaIdentifierHelper("v_"+javaIdentifier);

  }


  /**
   * Returns true if s is a JavaKeyWord
   * Returns false otherwise.
   */
  public static boolean isJavaKeyWord(String s) {
    String [] keywords = new String []
      {"abstract", "double", "int", "strictfp",
       "boolean", "else", "interface" ,"super",
       "break", "extends", "long", "switch",
       "byte", "final", "native","synchronized",
       "case", "finally", "new", "this", "catch",
       "float", "package", "throw", "char", "for",
       "private","throws", "class", "goto","protected",
       "transient", "const", "if", "public", "try",
       "continue", "implements", "return", "void",
       "default", "import", "short", "volatile", "do",
       "instance", "of", "static", "while"};
    for (int i = 0; i < keywords.length; i ++) {
      if (keywords[i].equals(s))
        return true;
    }
    return false;
  }


  // throws RuntimeException if javaIdentifier null, or zero length.
  // returns a String that is identical to input argument except that
  // all invalid Java Identifier characters are replaced with
  // their bytecodes.
  private static String makeValidJavaIdentifierHelper(String javaIdentifier) {
    if (javaIdentifier == null)
      throw new RuntimeException(); // Change this into a specific exception.
    else if (javaIdentifier.length() == 0)
      throw new RuntimeException(); // Change this into a specific exception.
    else {
      String retString = "";
      for (int i = 0; i < javaIdentifier.length() ; i++) {
        if (!Character.isJavaIdentifierPart(javaIdentifier.charAt(i))) {

          // Character does not have a getByte method.
          // So convert to string to get the bytes.
          String repl = getStringFromByteArray (getBytesFromChar (javaIdentifier.charAt(i)));
          retString = retString + repl;
        }
        else
          retString = retString + new String(new char [] {javaIdentifier.charAt(i)});
      }
      return retString;
    }}


  /**
   * Returns the ascii representation of char c in byte[] format.
   * @param c - character
   */
  private static byte [] getBytesFromChar (char c) {
    String replacement = new String(new char [] {c});
    return replacement.getBytes();
  }

  /**
   * Returns a string representation of byte array byte [] b.
   * @throws illegalArgumentException if b is
   * null or has zero length.
   * @param b - byte array to be converted.
   */
  private static String getStringFromByteArray (byte [] b) {
    if (b == null)
      throw new IllegalArgumentException();
    else if (b.length == 0)
      throw new IllegalArgumentException();
    else {
      String ret ="";
      for (int k = 0; k < b.length ; k ++ ) {
        ret = ret + Byte.toString(b[k]);
      }
      return ret;
    }
  }


  /**
   * Replaces all occurences of target in actual
   * with replacement.
   * @param actual -- original string
   * @param target -- the part to be replaced.
   * @param replacement -- the part to replace.
   * If specified target is not contained within actual,
   * returns actual.
   * @throws IllegalArgumentException if actual or replacement
   * or target is null.
   */
  private static String replaceAllString(String actual, String target, String replacement) {
    if (actual == null || target == null || replacement == null)
      throw new IllegalArgumentException();
    if (actual.indexOf(target) == -1 || target.equals(replacement))
      return actual;
    else
      if (actual.indexOf(target) == actual.indexOf(replacement)+ replacement.indexOf(target))
        return actual;
      else
        return actual.substring(0, actual.indexOf(target))+
          replacement+ replaceAllString(actual.substring(actual.indexOf(target)+target.length()), target, replacement);
  }



  // Returns true if the string invariant contains the word
  // "warning" and adds the string into list of unImplementedInvariants.
  // For immediate purposes only.  Should be removed after all
  // invariants are implemented in java output format.

  private static boolean isImplementedYet(String invariant) {
    if (invariant.indexOf("warning") != -1) {
      if (unImplementedInvariants == null) {
        unImplementedInvariants = new ArrayList();
        unImplementedInvariants.add(invariant);
      }
      else
        unImplementedInvariants.add(invariant);
      return false;
    }
    return true;
  }


  /**
   *  Creates a java program under the name javaFile that checks the
   *  invariants of a program.
   *
   *  @param invariantInformation
   *  @param javaFile -- output file
   *  @param invariantFile -- input invariant file
   *
   *  invariantInformation is a HashMap with PptTopLevel objects as
   *  keys and with an array containing two elements [a List containing
   *  VarInfo objects and a List containing Invariant objects for that
   *  program point] as values.
   *  Using invariantInformation, this module creates file javaFile
   *  which contains the source for the program that checks for
   *  the invariants of invariantFile.
   *
   */
  private static void createOutputFile (HashMap invariantInformation, File javaFile, File invariantFile)
    throws IOException {

    // Indentation is carried out in a "not good-looking" way here.
    // I am not aware of any Java class does this in a better way.
    // I created an emacs macro which indents a java file using
    // "java-mode" in emacs. It is called  ~akcabac/research/invariants/java/.indentJava
    // and can be run "emacs -batch -l .indentJava filename.java".


    procedureNameSet = new HashSet();
    String header = UtilMDE.join (new String [] {
      "// This file is auto-generated by MakeInvariantChecker.java.",
      "",
      "/**",
      " * " + javaFile.toString() + " checks whether the invariants present in",
      " * " + invariantFile.toString() + " hold for every program point present a new",
      " * new dtrace file.",
      " */"}, Daikon.lineSep);

    String importStatements = UtilMDE.join( new String [] {
      "",
      "//Import Statements ",
      "import daikon.tools.*;",
      "import daikon.*;",
      "import daikon.inv.*;",
      "import daikon.config.Configuration;",
      "import gnu.getopt.*;",
      "import utilMDE.*;",
      "import java.io.*;",
      "import java.util.*;",
      "import java.util.zip.GZIPInputStream;",
      "import java.util.zip.GZIPOutputStream;",
      "// Import Statements -- end",
      ""}, Daikon.lineSep);

    String dateGenerated = (new Date()).toString();

    String usage = UtilMDE.join (new String[] {
      "",
      "     /** shows how to use this class */",
      "     public static String usage = UtilMDE.join(new String[] {\"java"+ javaFile +"myProgram.dtrace [options]\",",
      "                                                             \"options : -h prints usage information\",",
      "                                                             \"          -o OutputFileName writes invariant violations\",",
      "                                                             \"             in file OutputFileName\"},",
      "                                               Daikon.lineSep);"}, Daikon.lineSep);

    String bannerRoutine =  UtilMDE.join (new String [] {
      "",
      "     /*",
      "      *  ShowBanner() prints the date of generation and",
      "      *  name of the generating program to standard out.",
      "      *",
      "      */",
      "     private static void showBanner() {" ,
      "          System.out.println(\"This program is auto-generated by MakeInvariantChecker.\");",
      "          System.out.println(\"Last updated =" + dateGenerated +"\");",
      "     }",
      ""}, Daikon.lineSep);

    String assertProc = UtilMDE.join(new String[]{
      "     public static void assertT(boolean invariant, String s) {",
      "          if (!invariant)",
      "               if (x != null)",
      "                    x.println(s);",
      "               else",
      "                    System.out.println(s);",
      "    }"},
                                     Daikon.lineSep);



    String readOptions = UtilMDE.join(new String [] {
      "     private static PrintWriter x;",
      "     /**",
      "      *  Reads command line options",
      "      *",
      "      *",
      "      */",
      "     private static Set readOptions (String[] args) throws IOException {",
      "          if (args.length == 0) {",
      "               System.out.println(\"No dtrace file specified.\");",
      "               System.out.println(usage);",
      "               System.exit(1);",
      "          }",
      "          Getopt g = new Getopt(\"test\", args, \"ho:\");",
      "          int c;",
      "          HashSet outputFiles = new HashSet();",
      "          HashSet inputFiles = new HashSet();",
      "          while ((c = g.getopt()) != -1) {",
      "               switch(c) {",
      "               case 'o':",
      "                    String outputFilename = g.getOptarg();",
      "                    File outputFile = new File(outputFilename);",
      "                    if (! UtilMDE.canCreateAndWrite(outputFile)) {",
      "                         throw new Error(\"Cannot write to file \" + outputFilename);",
      "                    }",
      "                    x = new PrintWriter(new FileWriter(outputFile));",
      "                    break;",
      "               case 'h':",
      "                    System.out.println(usage);",
      "                    System.exit(1);",
      "                    break;",
      "                    //",
      "               case '?':",
      "                    break; // getopt() already printed an error",
      "                    //",
      "               default:",
      "                    System.out.print(\"getopt() returned \" + c + Global.lineSep);",
      "                    break;",
      "               }",
      "          }",
      "          for (int i=g.getOptind(); i<args.length; i++) {",
      "               File file = new File(args[i]);",
      "               String filename = file.toString();",
      "               if ((! file.exists()) && (filename.indexOf(\".dtrace\") != -1))",
      "                    throw new Error(\"File \" + file + \" not found.\");",
      "               if (filename.indexOf(\".dtrace\") != -1)",
      "                    inputFiles.add(file);",
      "               else",
      "                    throw new Error(\"Unrecognized argument: \" + file);",
      "          }",
      "          return inputFiles;",
      "     }"
    }, Daikon.lineSep);

    String mainModule = UtilMDE.join (new String [] {
      "     public static void main(String[] args) throws IOException {",
      "          showBanner();",
      "          Set dtrace_files = readOptions(args);",
      "          String invFileName =\"" + invariantFile + "\";",
      "          try {",
      "               PptMap pptMap = (PptMap) FileIO.read_serialized_pptmap(new File(invFileName),false);",
      "               AssertionChecker ac =  new AssertionChecker();",
      "               FileIO.readDataTraceFile(dtrace_files,pptMap,ac);",
      "               if (x != null)",
      "                    x.close();",
      "               }",
      "          catch (IOException e) {",
      "               e.printStackTrace();",
      "               System.exit(1);",
      "          }}",
      ""}, Daikon.lineSep);

     String assertionProcedureDispatch =
      "          private static void checkAssertions(PptTopLevel atoplevel, ValueTuple vt) {" + Daikon.lineSep +
      "            List info = new ArrayList();" + Daikon.lineSep +
      "            info.add(vt.vals);" + Daikon.lineSep;

    String assertionChecker = UtilMDE.join (new String [] {
      "     public static final class AssertionChecker implements daikon.FileIO.DtraceProcessor{\n",
      "          public void visit(PptTopLevel atoplevel, ValueTuple vt) {",
      "               checkAssertions(atoplevel, vt);",
      "          }",
      ""}, Daikon.lineSep);

    CountingPrintWriter javaSourceWriter = new CountingPrintWriter (new FileWriter(javaFile));



    javaSourceWriter.print(UtilMDE.join (new String [] {
      header,
      importStatements,
      formatClassDeclaration(javaFile.toString()),
      usage,
      bannerRoutine,
      assertProc,
      readOptions,
      mainModule,
      assertionChecker,
      assertionProcedureDispatch},
                                         Daikon.lineSep));

    Iterator pptPoints = invariantInformation.keySet().iterator();
    while (pptPoints.hasNext()) {
      PptTopLevel atoplevel = (PptTopLevel) pptPoints.next();
      String pptPointName = makeValidJavaIdentifier(atoplevel.name());
      javaSourceWriter.print(UtilMDE.join (new String [] {
        "               if (daikon.tools.InvariantChecker.makeValidJavaIdentifier",
        "                   (atoplevel.name()).equals(\"" +
        pptPointName  + "\")) ",
        "                    assertPpt"+ pptPointName +"(info);", ""}, Daikon.lineSep));
    }
    javaSourceWriter.print("          }\n\n");


    Iterator pptPoints2 = invariantInformation.keySet().iterator();

    while (pptPoints2.hasNext()) {
      String initialization = Daikon.lineSep;
      PptTopLevel atoplevel = (PptTopLevel) pptPoints2.next();
      String pptPointName = makeValidJavaIdentifier(atoplevel.name());
      String procedureName = "          private static void assertPpt" + pptPointName;

      javaSourceWriter.print(UtilMDE.join(new String [] {
        procedureName + "(List valueList) {",
        "               if (valueList != null) {",
        "                    Iterator objList = valueList.iterator();",
        "                    while (objList.hasNext()) {",
        "                         Object[] objval = (Object[]) objList.next();",
        ""}, Daikon.lineSep));

      List[] pptInfo = (List []) invariantInformation.get(atoplevel);
      Assert.assertTrue(pptInfo != null);
      Assert.assertTrue(pptInfo.length == 2);
      List varInfos = (List) pptInfo[0];
      List invariantList = (List) pptInfo[1];

      Iterator variableIterator = varInfos.iterator();

      int count = 0;
      int procedureCount = 0;

      while (variableIterator.hasNext()) {
        VarInfo aVarInfo = (VarInfo) variableIterator.next();
        VarInfoName aVarInfoName = aVarInfo.name;
        ProglangType representationType = aVarInfo.file_rep_type;

        String newVar = makeValidJavaIdentifier(aVarInfoName.java_name(aVarInfo));
        String newType = representationType.toString();
        javaSourceWriter.print(formatVariableInitialization(newType, newVar,"objval["+count+"]"));
        count = count + 1;
      }
      javaSourceWriter.resetAll();
      Assert.assertTrue(invariantList != null);

      List assertions = formatAssertions(invariantList,varInfos);
      Iterator assertIt = assertions.iterator();
      while (assertIt.hasNext()) {
        String anAssertion = (String) assertIt.next();

        if (javaSourceWriter.getNumberOfPrintedBytes() + javaSourceWriter.countBytes(anAssertion) >= 65535) {
          if(procedureNameSet.contains(pptPointName + procedureCount))
            throw new Error("pptPointName duplicated. MakeInvariantChecker has a bug.");
          else {
            javaSourceWriter.resetAll();
            javaSourceWriter.print("assertPpt"+pptPointName + procedureCount + "(valueList);");
            javaSourceWriter.print("                    }}}          ");
            String otherProcedureName = "          private static void assertPpt" + pptPointName+procedureCount;

            javaSourceWriter.print(UtilMDE.join(new String [] {
              otherProcedureName + "(List valueList) {",
              "               if (valueList != null) {",
              "                    Iterator objList = valueList.iterator();",
              "                    while (objList.hasNext()) {",
              "                         Object[] objval = (Object[]) objList.next();",
              ""}, Daikon.lineSep));

            List[] otherPptInfo = (List []) invariantInformation.get(atoplevel);
            List otherVarInfos = (List) otherPptInfo[0];
            Iterator variableIt = otherVarInfos.iterator();
            int other_count = 0;
            while (variableIt.hasNext()) {
              VarInfo aVarInfo = (VarInfo) variableIt.next();
              VarInfoName aVarInfoName = aVarInfo.name;
              ProglangType representationType = aVarInfo.file_rep_type;

              String newVar = makeValidJavaIdentifier(aVarInfoName.java_name(aVarInfo));
              String newType = representationType.toString();
              javaSourceWriter.print(formatVariableInitialization(newType, newVar,"objval["+other_count+"]"));
              other_count = other_count + 1;
            }
            procedureCount = procedureCount + 1;
            javaSourceWriter.print(anAssertion);
          }}
        else
          javaSourceWriter.print(anAssertion);
      }
      javaSourceWriter.print("                    }}}");
      javaSourceWriter.resetAll();
    }
    javaSourceWriter.print("          }}");
    javaSourceWriter.close();
  }


  // Returns a list of java assertion statements (strings), constructed
  // from a list of invariants and varInfos.
  private static List formatAssertions(List invariants, List varInfos) {
    List assertions = new ArrayList();
    Iterator invIt = invariants.iterator();
    while (invIt.hasNext()) {
      Invariant anInvariant = (Invariant) invIt.next();
      String anInvariantStr = anInvariant.format_using(Daikon.output_style);
      String variables = "";
      Iterator varInfoIt = varInfos.iterator();
      while (varInfoIt.hasNext()) {
        VarInfo aVarInfo = (VarInfo) varInfoIt.next();
        if (anInvariant.usesVar(aVarInfo)) {
          anInvariantStr = replaceAllString(anInvariantStr,
                                            aVarInfo.name.java_name(aVarInfo),
                                            makeValidJavaIdentifier(aVarInfo.name.java_name(aVarInfo)));
          variables = variables + "+\" with "+aVarInfo.name.java_name(aVarInfo)
            +" = \"+" + makeValidJavaIdentifier(aVarInfo.name.java_name(aVarInfo));
        }
      }
      assertions.add(UtilMDE.join(new String[]{
        "",
        "                         //"+ anInvariant.repr(),
        "                         assertT("+ anInvariantStr+",",
        "                                           \"" +
        anInvariant.format_using(Daikon.output_style) + "\""+ variables+");"},
                                  Daikon.lineSep));
    }
    return assertions;
  }
}
