// Main module for invariant checker.
package daikon.tools;

import daikon.*;
import daikon.inv.*;
import daikon.derive.Derivation;
import daikon.derive.ValueAndModified;
import daikon.config.Configuration;
import daikon.temporal.TemporalInvariantManager;
import daikon.inv.Invariant.OutputFormat;
import daikon.inv.filter.*;

import utilMDE.*;
import gnu.getopt.*;
import org.apache.log4j.Logger;

import java.io.*;
import java.util.*;
import java.util.Date;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

// InvariantChecker.

public final class InvariantChecker {

  // Invalid java identifier names such as "this.theArray[1...length]"
  // or "202.167.125.21 Requests per second " are modified by
  // replacing every invalid character with its byte code. This HashMap
  // contains the actual variable name (e.g. "this.theArray[1...length]") as
  // the key, and the modified variable name (e.g. "var_this46theArray451464646length45")
  // as the value. It is "protected" for convenient access within the package.
  protected static HashMap varNameMap;


  // These are not functional yet. Next thing to do.

  // Specifies the behavior for "assertions".
  public static String assertion_behavior_SWITCH = "assertion_behavior";

  // Defines which filters to use when eliminating invariants.
  public static String filter_behavior_SWITCH = "filter_behavior";

  // List of available flags.
  protected static String flagList =
    UtilMDE.join (new String [] {"    " + assertion_behavior_SWITCH +
                                 " -- defines the method to be called to check invariants.",
                                 filter_behavior_SWITCH +
                                 " -- determines what filters will be used in eliminating invariants."},
                  Daikon.lineSep + "    ");

  // Usage string.
  protected static String usage =
    UtilMDE.join(new String[] {
      "Daikon invariant checker ",
      "Usage:",
      "    java invcheck.InvariantChecker [flags...] files...",
      "  Each file is a .inv file or a java file; the file type",
      "  is determined by the file name (containing \".inv\" or \".java\").",
      "  List of flags : ",
      flagList,
      "  For a list of flags, see the Daikon manual, which appears in the ",
      "  Daikon distribution and also at http://pag.lcs.mit.edu/daikon/."},
                 Daikon.lineSep);



  // Main module.
  public static void main(String[] args) {

    show_banner();

    // Read command line options
    Set[] files = readOptions(args);
    Set inv_files = files[0];  //
    Set java_files = files[1]; //
    if ((inv_files.size() == 0) || (java_files.size()==0)) {
      System.out.println("No .inv or .java file was specified");
      System.exit(1);
    }
    else if (inv_files.size() != java_files.size())
      System.out.println("No. of .inv files must be equal to no. of java files");
    try{
      Iterator outputFiles = java_files.iterator();
      Iterator inputFiles = inv_files.iterator();
      while (outputFiles.hasNext()){
        File outputFile = (File) outputFiles.next();
        File inputFile = (File) inputFiles.next();
        HashMap invariants = processInvariants(inputFile);

        writeOutputFile(invariants, outputFile, inputFile);
        System.out.println("Created "  + outputFile.toString() + "...");
      }
      System.out.println("Finished creating files. Exiting...");
     }
    catch(IOException e){
      System.out.println(e.toString());
      e.printStackTrace();
    }
    catch(ClassNotFoundException e) {
      System.out.println(e.toString());
      e.printStackTrace();
    }

  }


  // Most of the code reused from Daikon.java
  // Reads in the command line options.
  private static Set[] readOptions(String args[])
  {
    if (args.length == 0) {
      System.out.println("InvariantChecker error: no files supplied on command line.");
      System.out.println(usage);
      System.exit(1);
    }

    Set inv_files = new HashSet();
    Set java_files = new HashSet();

    LongOpt[] longopts = new LongOpt[] {
      new LongOpt(Daikon.help_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(assertion_behavior_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(filter_behavior_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0)};

    Getopt g = new Getopt("invcheck.InvariantChecker", args, "ho:", longopts);
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
            System.out.println("Assertion behavior not implemented.");
          } catch (Exception e) {
            throw new Error(e.toString());
          }
          break;
        } else {
          throw new RuntimeException("Unknown long option received: " + option_name);
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

      // I am keeping the following comment, is it still true?
      // // These aren't "endsWith()" because there might be a suffix on the end
      // // (eg, a date).
      // However, this does not apply to java files. They need to
      // have a ".java" extension. use "endsWith()"
      String filename = file.toString();
      if ((! file.exists()) && (filename.indexOf(".inv") != -1)) {
        throw new Error("File " + file + " not found.");
      }

      if (filename.indexOf(".inv") != -1) {
        inv_files.add(file);
      } else if (filename.endsWith(".java")) {
        if (! UtilMDE.canCreateAndWrite(file)) {
          throw new Error("Cannot write to file " + filename);
        }
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



  // Print this banner
  private static void show_banner() {
    System.err.
      print("\n*************************************************\n" +
            "************ Invariant Checker Module ***********\n" +
            "*************************************************\n");
    System.err.flush();
  }




  // Reads in the ".inv" file invariantFile using FileIO.read_serialized_pptMap.
  // Extracts VarInfos and Invariants from the PptMap object contained in the file.
  // Returns a HashMap w/ "keys" as PptTopLevel objects and w/ "values" as arrays of
  // 3 Lists. new List[] {List VariableNames, List VariableTypes, List Invariants}.
  // VariableNames, VariableTypes and Invariants are all in String format.

  private static HashMap processInvariants(File invariantFile) throws IOException,ClassNotFoundException{

    // The variable names present in the .inv file might
    // not be compilable as in "class1.method1().var1".
    varNameMap = new HashMap();



    Daikon.output_style = OutputFormat.JAVA;

    // Holds all the information required to build assert statements.
    // Keys: PptTopLevel names. Values: array containing typeList,variableList,assertionList
    // for the associated PptTopLevel.
    HashMap invariantInfo = new HashMap();


    // Read the relevant PptMap in.
    PptMap pptMap = (PptMap) FileIO.read_serialized_pptmap(invariantFile,false);

    Iterator pptIterator = pptMap.pptIterator();
    while (pptIterator.hasNext()){
      PptTopLevel atoplevel = (PptTopLevel)pptIterator.next();

      List [] currentLists = (List [])invariantInfo.get(atoplevel.ppt_name.getShortClassName()
                                                        + atoplevel.ppt_name.getShortMethodName());
      if (currentLists == null) {
        currentLists = new List []{
          new ArrayList(),
          new ArrayList(),
          new ArrayList(),};
        invariantInfo.put(atoplevel.ppt_name.getShortClassName()+
                          atoplevel.ppt_name.getShortMethodName()
                          , currentLists);
      }

      // Get the variable information (declared type and name)
      // to generate variable declaration statements.

      VarInfo[] varInfos = atoplevel.var_infos;

      for (int i = 0; i < varInfos.length; i ++) {
        VarInfo aVarInfo = (VarInfo)varInfos[i];
        VarInfoName aVarInfoName = aVarInfo.name;

        varNameMap.put(aVarInfoName.java_name(),makeValidJavaIdentifier(aVarInfoName.java_name()));

        ProglangType representationType = aVarInfo.file_rep_type;
        ((List) currentLists[0]).add(makeValidJavaIdentifier(aVarInfoName.java_name()));
        ((List) currentLists[1]).add(representationType.toString());
      }

      // Get the invariants for this PptTopLevel.
      Iterator invariants = atoplevel.invariants_iterator();
      InvariantFilters fi = new InvariantFilters();
      fi.setPptMap(pptMap);

      while(invariants.hasNext()){

        Invariant anInvariant = (Invariant) invariants.next();
        boolean fi_accepted = fi.shouldKeep(anInvariant);
        String stringInvariant = anInvariant.format_using(Daikon.output_style);
        if (fi_accepted)
          ((List) currentLists[2]).add(stringInvariant);
      }
    }

    return invariantInfo;
  }

  private static List replaceNames(List L) {
    Iterator invariants = L.iterator();
    List returnList = new ArrayList();
    while (invariants.hasNext()) {
      String anInvariant = (String) invariants.next();
      returnList.add(replaceNames(anInvariant));
    }
    return returnList;
  }

  private static String replaceNames(String s) {
    Iterator varNames = varNameMap.keySet().iterator();
    String retVal = s;
    List possibleReplacements = new ArrayList();

    while (varNames.hasNext()) {
      String variableName = (String) varNames.next();
      if (retVal.indexOf(variableName) != -1)
        possibleReplacements.add(variableName);
      //retVal = replaceAllString(s,variableName,(String)varNameMap.get(variableName));
    }

    Iterator possibleRepl = possibleReplacements.iterator();
    while (possibleRepl.hasNext()) {
      String aVar = (String) possibleRepl.next();
      Iterator poRepl = possibleReplacements.iterator();
      while (poRepl.hasNext()) {
        String anotherVar = (String) poRepl.next();;
        if (!(anotherVar.equals(aVar)))
          if (anotherVar.indexOf(aVar) != -1)
            retVal = replaceAllString(retVal,anotherVar,(String) varNameMap.get(anotherVar));
          else if (aVar.indexOf(anotherVar) != -1)
            retVal = replaceAllString(retVal,aVar,(String) varNameMap.get(aVar));
      }
    }
    varNames = varNameMap.keySet().iterator();
    while(varNames.hasNext()) {
      String variableName = (String) varNames.next();
      if (retVal.indexOf(variableName) != -1)
        retVal = replaceAllString(retVal,variableName,(String)varNameMap.get(variableName));
    }
    return retVal;
  }




  private static String formatAssertions(String s) {
    return "                 Assert.assertTrue(" + s + ",\"" + s + "\");\n";
  }

  private static String formatVariableInitialization(String type, String varName, String val) {
    // Primitive types such as double, int, boolean need to be parsed
    // from the string value.
    if (type.equals("int"))
      return  "                    " + type + " " +
        varName + " = (" + type + ") Integer.parseInt(" +
        val + ".toString());\n";
    else if (type.equals("boolean"))
      return  "                    " + type + " " +
         varName + " = (" + type + ") Boolean.valueOf(" +
        val + ".toString());\n";
    else if (type.equals("double"))
      return  "                    " + type + " " + varName +
        " = (" + type + ") Double.parseDouble("+
        val + ".toString());\n";
    else if (type.equals("hashcode"))
      return  "                    int " +
        varName + " = (int) Integer.parseInt(" +
        val + ".toString());\n";
    else if (type.equals("hashcode[]"))
      return  "                    int[] " +
        varName + " = (int[])"+ val+";\n";
    else
      return  "                    " + type + " " + varName +
        " = (" + type + ") " + val + ";\n";
 }

  private static String formatClassDeclaration (String fileName) {
    return "public final class " + fileName.substring(0, fileName.lastIndexOf(".java")) + "{" ;
  }




  private static void writeOutputFile(HashMap invariantInformation, File javaFile, File invariantFile)
    throws IOException {

    String javaFileName = javaFile.toString();

    String header = "// This file is auto-generated by InvariantChecker.java.\n";

    String classDeclaration = formatClassDeclaration(javaFileName)+"\n";

    String importStatements = "\n//Import Statements \n" +
      "import daikon.tools.*;\n" +
      "import daikon.*;\n" +
      "import daikon.inv.*;\n" +
      "import daikon.config.Configuration;\n" +
      "import daikon.temporal.TemporalInvariantManager;\n" +
      "import utilMDE.*;\n" +
      "import java.io.*;\n" +
      "import java.util.*;\n" +
      "import java.util.zip.GZIPInputStream;\n" +
      "import java.util.zip.GZIPOutputStream;\n";

    Date dategenerated = new Date();
    String dateGenerated = dategenerated.toString();

    String bannerRoutine =
      "     private static void showBanner(){\n" +
      "          System.out.println(\"This program is auto-generated by InvariantChecker.\");\n" +
        "          System.out.println(\"Last updated =" + dateGenerated +"\");\n" +
      "     }\n" ;

    String mainModule = "\n" +
      "     public static void main(String[] args)\n" +
      "     {\n" +
      "          showBanner();\n" +
      "          String dtraceFileName = args[0];\n" +
      "          String invFileName =\"" + invariantFile + "\";\n" +
      "          try {\n" +
      "               PptMap pptMap = (PptMap) FileIO.read_serialized_pptmap(new File(invFileName),false);\n" +
      "               Set dtrace_files = new HashSet();\n" +
      "               dtrace_files.add(new File(dtraceFileName));\n" +
      "               AssertionChecker ac =  new AssertionChecker();\n"+
      "               FileIO.readDataTraceFile(dtrace_files,pptMap,null,ac);\n" +
      "               }\n" +
      "          catch (IOException e) {\n" +
      "               e.printStackTrace();\n" +
      "               System.exit(1);\n" +
      "          }}\n\n";


      String assertionProcedures ="\n";
      String assertionProcedureDispatch =
        "     private static void checkAssertions(PptTopLevel atoplevel, List info) {\n";


      Iterator pptPoints = invariantInformation.keySet().iterator();



      while (pptPoints.hasNext()){
        String initialization = "\n";
        String assertions = "\n";
        String pptPointName = (String) pptPoints.next();
        String procedureName = "     private static void assertPpt"
          + pptPointName;
        List[] pptInfo = (List []) invariantInformation.get(pptPointName);
        Assert.assertTrue(pptInfo != null);
        Assert.assertTrue(pptInfo.length == 3);
        List variables = (List) pptInfo[0];
        List types = (List) pptInfo[1];
        List invariantList = (List) pptInfo[2];

        Iterator variableIterator = variables.iterator();
        Iterator typesIterator = types.iterator();
        int count = 0;
        while (variableIterator.hasNext() && typesIterator.hasNext()){
          String newVar = (String) variableIterator.next();
          String newType = (String) typesIterator.next();
          initialization = initialization +
            formatVariableInitialization(newType, newVar,"objval["+count+"]");
          count = count + 1;
        }

        Assert.assertTrue(invariantList != null);
        Iterator invariantIterator = (replaceNames(invariantList)).iterator();
        while (invariantIterator.hasNext()) {
          String anAssertion = formatAssertions((String) invariantIterator.next());
          assertions = assertions + "   " + anAssertion;
        }

        assertionProcedureDispatch = assertionProcedureDispatch +
          "          if ((atoplevel.ppt_name.getShortClassName() + atoplevel.ppt_name.getShortMethodName()).equals(\"" + pptPointName  + "\")) \n"+
          "               assertPpt"+ pptPointName +"(info);\n";

        assertionProcedures = assertionProcedures + "\n" +
          procedureName + "(List valueList) {\n" +
          "          if (valueList != null) {\n" +
          "               Iterator objList = valueList.iterator();\n" +
          "               while (objList.hasNext()) {\n" +
          "                    Object[] objval = (Object[]) objList.next();\n" +
          initialization +
          assertions +
          "               }}\n"+
          "       }";
        assertions = "";
        initialization ="";
      }


      assertionProcedureDispatch = assertionProcedureDispatch +"     }\n";
      String assertionChecker = UtilMDE.join (new String [] {
        "     public static final class AssertionChecker implements DtraceProcessor{",
        "          public void visit(PptTopLevel atoplevel, List n) {",
        "               checkAssertions(atoplevel,n);",
        "          }"
      }, Daikon.lineSep);




      String footer = "}}";

      // Write to file
      PrintWriter javaSourceWriter = new PrintWriter (new FileWriter (new File (javaFileName)));

      javaSourceWriter.print(header +
                             importStatements +
                             classDeclaration + "\n" +
                             bannerRoutine +
                             mainModule +
                             assertionChecker +
                             assertionProcedureDispatch +
                             assertionProcedures +
                             footer
                             );


      javaSourceWriter.close();
    }


  // Checks whether the input string is
  // a valid java identifier.
  // Returns true if all the characters
  // in the string obeys:
  // character.isValidJavaIdentifierPart() == true
  // &&
  // and the first character obeys
  // character.isValidJavaIdentifierStart() == true
  // Returns false otherwise.

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


  // Returns true if the first
  // character of the input string
  // obeys:
  // character.isValidJavaIdentifierPart() == true
  // Returns false otherwise.
  private static boolean hasValidJavaIdentifierStart(String javaIdentifier) {
    if ((javaIdentifier == null) ||
        (javaIdentifier.length() == 0))
      return false;
    else
      return Character.isJavaIdentifierStart(javaIdentifier.charAt(0));
  }

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

  // Makes a valid java identifier name out of an invalid name.
  // If input argument is a valid java identifier, returns the input
  // argument.
  //
  // If input argument is not valid, appends the string "var_" to the
  // input argument and replaces every invalid character with its
  // bytecode representation.
  // @throws RuntimeException if input argument is null or 0 length
  // @args String javaIdentifier.

    private static String makeValidJavaIdentifier(String javaIdentifier) {
	boolean validBody = hasValidJavaIdentifierBody(javaIdentifier);
	boolean validStart = hasValidJavaIdentifierStart(javaIdentifier);
	if (validBody && validStart) {
          return javaIdentifier;
	}
	else if (validBody && !validStart) {
          if (isValidJavaIdentifier("var_" + javaIdentifier))
            return "var_" + javaIdentifier;
          else
            return makeValidJavaIdentifierHelper("var_"+javaIdentifier);
	}
	else
          return makeValidJavaIdentifierHelper("var_"+javaIdentifier);
    }


  // @throws RuntimeException if javaIdentifier null, or zero length.
  // @return a String that is identical to input argument except that
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
        if (!Character.isJavaIdentifierPart(javaIdentifier.charAt(i))){

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


  // Returns the byte representation of char c
  // in byte[] format.
  private static byte [] getBytesFromChar (char c) {
    String replacement = new String(new char [] {c});
    return replacement.getBytes();
  }

  // Returns a string representation
  // of byte array byte [] b.
  // Throws illegalArgumentException if b is
  // null or has zero length.
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


  // Replaces all occurences of String target in String actual
  // with String replacement.
  // If specified target is not contained within actual,
  // returns actual.
  // @throws IllegalArgumentException if actual or replacement
  // or target is null.
  private static String replaceAllString(String actual, String target, String replacement) {
    if (actual == null || target == null || replacement == null)
      throw new IllegalArgumentException();
    if (actual.indexOf(target) == -1 || target.equals(replacement))
      return actual;
    else
      return replaceAllString(replaceString(actual,target,replacement),target,replacement);

  }

  // Helper function for replaceAllString.
  private static String replaceString(String actual, String target, String replacement) {
    if (actual == null || target == null || replacement == null)
      throw new IllegalArgumentException();
    if (actual.indexOf(target) == -1)
      return actual;
    else
      return actual.substring(0,actual.indexOf(target))+
        replacement+ actual.substring(actual.indexOf(target)+target.length());
  }

}
