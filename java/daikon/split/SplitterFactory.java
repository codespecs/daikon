package daikon.split;

import utilMDE.*;
import org.apache.oro.text.regex.*;
import daikon.*;
import daikon.split.misc.*;
import java.io.*;
import java.util.*;
import java.lang.reflect.*;
import org.apache.log4j.Logger;

/**
 * This class creates Splitters from a .spinfo file. The public method is
 * read_spinfofile( spinfofilename ), which returns a vector containing program
 * point names and their corresponding arrays of Splitters
 **/

// todo: add logging and debugging
//      log error messages from compilation of splitters
public class SplitterFactory {

  public static final Logger debug = Logger.getLogger("daikon.split.SplitterFactory");
  // These are not Global.regexp_matcher and Global.regexp_compiler
  // because you can't reference fields of Global in your static
  // initializer if you have fields that are set via dkconfig.
  private static Perl5Matcher re_matcher = new Perl5Matcher();
  private static Perl5Compiler re_compiler = new Perl5Compiler();
  private static String tempdir;
  // Indicates whether javac is being used as the compiler;
  // javac's failure modes require special error recovery.
  private static boolean javac = false; //

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /**
   * Boolean.  Specifies whether or not the temporary Splitter files
   * should be deleted on exit.
   **/
  public static boolean dkconfig_delete_splitters_on_exit = true;

  // It would be better for SplitterFactory to use PrintWriters (or
  // PrintStreams) backed by ByteArrayOutputStreams, rather than
  // StringBuffers; that would eliminate the line separator ugliness.
  private static String lineSep = System.getProperty("line.separator");

  /// Regular expressions

  /** Matches the names of Java source files, without directory name. **/
  static Pattern splitter_classname_pattern;
  /**
   * Matches variables that might act as array indices, i.e., variable
   * names inside square brackets.
   **/
  static Pattern find_index_pattern;
  static Pattern blank_line;
  static Pattern operator_pattern;
  static Pattern null_pattern; // "null"
  static Pattern format_spec_pattern; // ^(JAVA|SIMPLIFY|...)_FORMAT
  static Substitution zero_substitution = new StringSubstitution("0");
  static Pattern equals_null_pattern; // "== null" or "!= null"
  static Substitution equals_null_subst = new Perl5Substitution(" $1= 0");

  /** Matches "orig" variable names. **/
  static Pattern find_orig_pattern;
  /** Replaces "orig(varname)" with "orig_varname", and similarly for
   * post(varname) and size(varname). **/
  static Substitution orig_subst = new Perl5Substitution("$1_$2");
  /** Matches the variable names of arguments inside function calls. **/
  static Pattern arg_pattern;
  static Pattern octothorpe_pattern; // "#"
  static Substitution octothorpe_subst = new StringSubstitution("#");

  static {
    try {
      splitter_classname_pattern
        = re_compiler.compile("([^" + UtilMDE.quote(File.separator) + "]+)\\.java");
      find_index_pattern = re_compiler.compile("\\[\\s*([^\\])]*)\\s*\\]");
      blank_line  = re_compiler.compile("^\\s*$");
      operator_pattern = re_compiler.compile("[+=!><-]");
      null_pattern = re_compiler.compile("\\bnull\\b");
      format_spec_pattern = re_compiler.compile("^([A-Z_]+)_FORMAT.*");
      equals_null_pattern = re_compiler.compile("(!|=)=\\s*null");
      find_orig_pattern = re_compiler.compile("\\b(orig|post|size)\\s*\\(\\s*(\\S*?)\\s*\\)");
      arg_pattern = re_compiler.compile("(\\S+)\\s*\\((.*)\\)");
      octothorpe_pattern = re_compiler.compile("#");
    } catch (MalformedPatternException me) {
      me.printStackTrace();
      throw new Error("Error in regexp: " + me.toString());
    }
  }

  //Observers

  /**
   * @return the directory that temporary files are stored at
   */
  public static String getTempDir() {
    return tempdir;
  }

  /// Methods

  /**
   * Reads the Splitter info.
   * @param <filename>.spinfo
   * @return Vector with the data: (Pptname, Splitter[], Pptname, Splitter[] ...)
   **/
  public static SplitterObject[][] read_spinfofile(File infofile, PptMap all_ppts)
    throws IOException, FileNotFoundException
  {
    tempdir = getTempdirName();
    if (FileCompiler.dkconfig_compiler.equals("javac"))
      javac = true;

    LineNumberReader reader = UtilMDE.LineNumberFileReader(infofile.toString());
    Vector splitterObjectArrays = new Vector(); // Vector of SplitterObject[]
    Vector replace = new Vector(); // [String] but paired
    Vector returnSplitters = new Vector();

    try {
      String line = reader.readLine();
      for ( ; line != null; line = reader.readLine()) {
        // skip blank lines and comments
        line.trim();
        if (re_matcher.matches(line, blank_line) || line.startsWith("#")) {
          continue;
        } else if (line.startsWith("REPLACE")) {
          replace.addAll(read_replace_statements(reader));
        } else if (line.startsWith("PPT_NAME")) {
          StringTokenizer tokenizer = new StringTokenizer(line);
          tokenizer.nextToken(); // throw away the first token "PPT_NAME"
          String pptName = tokenizer.nextToken().trim();
          SplitterObject[] spobjects = read_ppt_conditions(reader, pptName,
                                                           infofile);
          if (spobjects != null)
            splitterObjectArrays.addElement(spobjects);
        } else {
          System.err.println("Incorrect format in .spinfo " + infofile
                             + " at line number " + reader.getLineNumber());
        }
      }
    } catch (IOException ioe ) {
      System.err.println(ioe);
      System.err.println(" at line number " + reader.getLineNumber()
                         + " of .spinfo file");
      System.err.println();
    }
    SplitterObject[][] sArrays = (SplitterObject[][])splitterObjectArrays.toArray(new SplitterObject[0][0]);
    write_compile_load(sArrays, replace, all_ppts);

    return sArrays;
  }

  /**
   * Reads the statements in the REPLACE section of the Splitter info file.
   * The line "REPLACE" has just been read from the LineNumberReader
   * `reader'.
   *
   * @return a new Vector[String] containing the replace statements.
   **/
  static Vector read_replace_statements(LineNumberReader reader)
    throws IOException, FileNotFoundException
  {
    Vector result = new Vector();
    String line = reader.readLine();
    while ((line != null) && !re_matcher.matches(line, blank_line)) {
      // skip comments
      if (! line.startsWith("#")) {
        result.addElement(line.trim());
      }
      line = reader.readLine();
    }
    return result;
  }

  /**
   * Reads the splitting conditions associated with a program point.
   * @return an array of SplitterObjects representing the conditions
   * at a program point, or null if there is no condition under this
   * program point.
   **/
  static SplitterObject[] read_ppt_conditions(LineNumberReader reader,
                                              String pptname,
                                              File infofile)
    throws IOException, FileNotFoundException
  {
    Vector splitterObjects = new Vector(); // [String == splitting condition]
    String line;
    SplitterObject obj = null;
    while ((line = reader.readLine()) != null) {
      // Blank line ends conditions for the program point
      if (re_matcher.matches(line, blank_line))
        break;
      // skip comments
      if (line.startsWith("#"))
        continue;
      String condition = line.trim();
      if (Character.isWhitespace(line.charAt(0)) &&
          re_matcher.matches(condition, format_spec_pattern)) {
        if (obj == null) {
          System.err.println("Format spec must follow a condition in .spinfo "
                             + infofile.toString() + " at line number "
                             + reader.getLineNumber());
          continue;
        }
        if (condition.startsWith("DAIKON_FORMAT")) {
          obj.daikonFormat =
            condition.substring("DAIKON_FORMAT".length()).trim();
        } else if (condition.startsWith("JAVA_FORMAT")) {
          obj.javaFormat = condition.substring("JAVA_FORMAT".length()).trim();
        } else if (condition.startsWith("ESC_FORMAT")) {
          obj.escFormat = condition.substring("ESC_FORMAT".length()).trim();
        } else if (condition.startsWith("SIMPLIFY_FORMAT")) {
          obj.simplifyFormat =
            condition.substring("SIMPLIFY_FORMAT".length()).trim();
        } else if (condition.startsWith("IOA_FORMAT")) {
          obj.ioaFormat = condition.substring("IOA_FORMAT".length()).trim();
        } else {
          System.err.println("Unrecognized format spec in .spinfo "
                             + infofile.toString() + " at line number "
                             + reader.getLineNumber());
          continue;
        }
      } else {
        obj = new SplitterObject(pptname, condition, tempdir);
        if (!duplicate_condition(condition, pptname))
          splitterObjects.addElement(obj);
      }
      // // In addition to "== null", add "== 0".  Actually, we will convert
      // // to "0" unconditionally later.
      // if (re_matcher.contains(condition, equals_null_pattern)) {
      //  splitterObjects.addElement(
      //      new SplitterObject(pptname,
      //                         perform_equals_null_substitution(condition),
      //                         tempdir));
      // }
    }
    if ( splitterObjects.size() > 0)
      return (SplitterObject[]) splitterObjects.toArray(new SplitterObject[0]);
    else
      return null;
  }

  /**
   * Write the Splitter classes, compile and load the Splitter objects
   * for each condition.  The Vector ppts_and_conds contains the
   * pptnames and their associated splitting conditions.
   *
   * @param splitterObjectArrays an Array containing Arrays of
   * SplitterObjects. Each slice of the 2-Dimensional array
   * corresponds to the SplitterObjects at a program point. The Java
   * source for each splitter condition is  yet to be written
   *
   * @return another Array containing Arrays of SplitterObjects. The
   * source code for the splitterObjects have been written, compiled and loaded
   **/
  static void write_compile_load(SplitterObject[][] splitterObjectArrays,
                                 Vector replace,        // [String]
                                 PptMap all_ppts)
  {
    ArrayList processes = new ArrayList(); // the compilation processes

    for (int i = 0; i < splitterObjectArrays.length; i++) {
      // write the Splitter classes
      try {
        write_function_splitters(splitterObjectArrays[i], replace, all_ppts);

      } catch(IOException ioe) {
        System.err.println("SplitterFactory.write_compile_load: " + ioe);
      }

      ArrayList compile_list = new ArrayList();
      for (int j = 0; j < splitterObjectArrays[i].length; j++) {
        compile_list.add(splitterObjectArrays[i][j].getFullSourcePath());
      }

      // compile all the splitters under this program point
      processes.add(FileCompiler.compile_source(compile_list));
    }

    StringBuffer errorString = new StringBuffer(); // stores the error messages
    // Wait for all the compilation processes to terminate.
    // If compiling with javac, then processes.size() == 0.
    for (int i = 0; i < processes.size(); i++) {
      TimedProcess tp = (TimedProcess) processes.get(i);
      errorString.append(lineSep);
      errorString.append(tp.getErrorMessage());
      if (!tp.finished()) {
        tp.waitFor();
      }
    }

    // javac tends to stop without completing the compilation if there
    // is an error in one of the files. Remove all the erring files
    // and recompile only the good ones.
    if (javac) {
      recompile_without_errors (splitterObjectArrays, errorString.toString());
    }

    // Load the Splitters
    SplitterLoader loader = new SplitterLoader();
    for (int i = 0; i < splitterObjectArrays.length; i++) {
      for (int j = 0; j < splitterObjectArrays[i].length; j++) {
        splitterObjectArrays[i][j].load(loader);
      }
    }

    if ((splitterObjectArrays.length > 0)
        && (! dkconfig_delete_splitters_on_exit)) {
      System.out.println("Splitters for this run created in " + tempdir);
    }
  }

  /**
   * Examine the errorString to identify the Splitters that cannot
   * compile, then recompile all the other files. This function is
   * necessary when compiling with javac because javac does not
   * compile all the files supplied to it if some of them contain
   * errors. So some "good" files end up not being compiled.
   */
  private static void recompile_without_errors (SplitterObject[][] spArrays,
                                                String errorString) {
    // search the error string and extract the files with errors.
    if (errorString != null) {
      HashSet errors = new HashSet();
      PatternMatcherInput input = new PatternMatcherInput(errorString);
      while (re_matcher.contains(input, splitter_classname_pattern)) {
        MatchResult result = re_matcher.getMatch();
        errors.add(result.group(1));
      }

      List retry = new ArrayList();
      // Collect all the splitters that were not compiled.
      for (int i = 0; i < spArrays.length; i++) {
        for (int j = 0; j < spArrays[i].length; j++) {
          if (!spArrays[i][j].compiled()) {
            if (!errors.contains(spArrays[i][j].getClassName())) {
              retry.add(spArrays[i][j].getFullSourcePath());
            }
          }
        }
      }

      TimedProcess tp = FileCompiler.compile_source(retry);

      try {
        Thread.sleep(3000);
      } catch (InterruptedException ie) {
        ie.printStackTrace();
      }

      // We don't want to wait for the old process for too long. We
      // wait for a short time, kill the process and recompile the set
      // of files, removing the leading file
      if (tp != null && !tp.finished()) {
        tp.waitFor();
      }
    }
  }

  private static int guid = 0; // To give classes unique names

  /**
   * @param splitterObjects Must have at least one SplitterObject in it.
   * Writes the Java source code for the Splitters for this program point.
   **/
  static void write_function_splitters (SplitterObject[] splitterObjects,
                                        Vector replace,
                                        PptMap all_ppts)
    throws IOException
  {
    String ppt_name = splitterObjects[0].getPptName();
    PptTopLevel ppt = find_corresponding_ppt(ppt_name, all_ppts);
    debugPrintln("find_corresponding_ppt(" + ppt_name + ") => " + ppt);
    if (ppt == null) {
      // try with the OBJECT program point
      ppt = find_corresponding_ppt("OBJECT", all_ppts);
    }
    if (ppt == null) {
      // We just get a random program point (the first) from the pptmap.
      // Hopefully we can find the variables that we need at this ppt
      Iterator pptIter = all_ppts.pptIterator();
      if (pptIter.hasNext()) {
        ppt = (PptTopLevel)pptIter.next();
      }
    }
    if (ppt == null) {
      for (int i = 0; i < splitterObjects.length; i++) {
        splitterObjects[i].setError( "No corresponding program point found for " + ppt_name);
      }
      return;
    }

    Vector[] params_and_types = get_params_and_types(ppt);
    Vector varinfos = params_and_types[0];
    Vector parameters  = params_and_types[1];
    Vector types = params_and_types[2];

    sort_params_by_length(parameters, types, varinfos);

    debugPrintln("write_function_splitters: parameters.size() = " + parameters.size() + ", type.size() = " + types.size());
    if (parameters.size() > 0 && types.size() > 0) {
      String[] all_params = (String[]) parameters.toArray(new String[0]);
      String[] all_types = (String[]) types.toArray(new String[0]);
      VarInfo[] all_varinfos = (VarInfo[]) varinfos.toArray(new VarInfo[0]);
      int num_params = all_params.length;

      // These are the variable names in the generated Java source code for
      // the Splitter.
      String[] param_names = new String[num_params];
      for (int i = 0; i < num_params; i++) {
        debugPrintln("all_params[" + i + "] = " + all_params[i] + "; vi=" + all_varinfos[i].name.name());
        param_names[i] = cleanup_varname(all_params[i]);
      }

      // Get the function names and argument names of functions to be replaced.
      // For example, if the function "max(int a, int b)" is to be replaced by
      // the expression "a > b ? a : b", then the return value of this
      // function call is a vector containing a String representing the function
      // name "max" as the first element, an array of Strings representing the
      // argument names [ a b ] as the second element, and the replacement
      // expression "a > b ? a : b" as the third element
      Vector replace_data = get_fnames_and_args(replace);


      // Class names cannot only have legal identifier characters in
      // them. For example if the ppt_name is Foo.bar, the name of the
      // Splitter start with Foo_bar
      String splittername;
      {
        char[] cleaned = ppt_name.toCharArray();
        for (int i=0; i < cleaned.length; i++) {
          char c = cleaned[i];
          if (! Character.isJavaIdentifierStart(c)) {
            cleaned[i] = '_';
          }
        }
        splittername = new String(cleaned);
      }

      // write a Splitter class for each condition:
      for (int numsplitters = 0; numsplitters < splitterObjects.length; numsplitters++) {
        SplitterObject curSplitterObject = splitterObjects[numsplitters];

        // the String 'condition' is returned in this Splitter's public method
        // 'condition()' the String 'test_string' is used to do the splitting in
        // the 'test()' method
        String condition = curSplitterObject.condition();
        String test_string = condition;

        guid++;
        String splitter_fname = splittername + "_" + guid;
        int lastIndex = splitter_fname.indexOf(File.separator);
        curSplitterObject.setClassName(splitter_fname.substring(lastIndex + 1));
        String class_name = ppt_name.substring(0, ppt_name.indexOf('.')+1);
        curSplitterObject.setGUID(guid);

        // Each Splitter will use a different set of parameters depending on the
        // parameters used in its condition
        String[] params = (String[])all_params.clone();

        // Replace function calls in the condition by their bodies.
        test_string = replace_condition(test_string, replace_data);

        // Ensure that the declared variable names in the Splitter match the
        // variable names in the test string. For example: if the condition
        // tests for "this.mylength == 0", the variable corresponding to
        // "this.mylength" in the Splitter is declared as "this_mylength".
        // Therefore change the condition to "this_mylength == 0".

        // By side effect, this changes some elements of "params" to null.
        // It also may change elements of "param_names".
        test_string = find_applicable_variables(params, param_names, test_string, class_name, all_varinfos);
        debugPrintln("post find_applicable_variables: " + utilMDE.ArraysMDE.toString(params));

        // Replace all occurrences of "orig(varname)" with "orig_varname" in
        // the condition.
        test_string = replace_orig(test_string);

        // Look for all variables that are used as array indices and
        // change their type to "int_index". This is necessary because
        // daikon represents all numeric types as long. However array
        // accessors must be cast to ints so they can be used to access
        // arrays. Therefore for these variables, we need to call a
        // different method (VarInfo.getIndexValue()) to obtain their
        // values from the VarInfo.
        identify_array_index_variables_as_int(condition, params, all_types);

        StringBuffer file_string = new StringBuffer();
        file_string.append("import daikon.*;" + lineSep);
        file_string.append("import daikon.inv.*;" + lineSep);
        file_string.append("import daikon.split.*;" + lineSep + lineSep);

        file_string.append("public final class " + splitter_fname + " extends Splitter { " + lineSep + lineSep);

        // print the condition() method
        file_string.append("  public String condition () { return \"" +
                           UtilMDE.quote(condition) + "\"; } " + lineSep + lineSep);

        // print the parameters
        for (int i = 0; i < num_params; i++) {
          // The param has been replaced with null if it doesn't appear in the
          // test string. Therefore skip it since the splitter doesn't need any
          // information about it.
          if (params[i] == null)
            continue;

          VarInfo vi = all_varinfos[i];
          String par = param_names[i];
          String typ = all_types[i];
          print_parameter_declarations(file_string, par, typ, vi);
        }
        file_string.append("  static DummyInvariant dummyInvFactory;"
                           + lineSep);
        file_string.append("  DummyInvariant dummyInv;" + lineSep);

        // print the constructors
        file_string.append(lineSep);
        file_string.append("  public " + splitter_fname + "() { } " + lineSep);

        file_string.append("  public " + splitter_fname + "(Ppt ppt) {"
                           + lineSep);
        int good_param_count = 0;
        for (int i = 0; i < num_params; i++) {
          String param =  params[i];
          if (param == null) continue;
          good_param_count++;
          String param_name = param_names[i];
          String typ = all_types[i];
          file_string.append("    " + param_name
                             + "_varinfo = ppt.findVarByRepr(\""
                             + all_varinfos[i].name.repr() + "\");" + lineSep);
        }

        // file_string.append("    instantiated = true;" + lineSep);
        file_string.append("  }" + lineSep + lineSep);

        // print the instantiate method.
        file_string.append("  public Splitter instantiate(Ppt ppt) {"
                           + lineSep);
        file_string.append("    return new " + splitter_fname
                           + "(ppt);" + lineSep);
        file_string.append("  } " + lineSep + lineSep);

        // print the valid() method
        file_string.append("  public boolean valid() { " + lineSep);
        file_string.append("    return (");
        for (int i = 0; i < param_names.length; i++) {
          if (params[i] == null)
            continue;
          file_string.append("(" + param_names[i] + "_varinfo != null) && ");
        }
        file_string.append(" true );" + lineSep);
        file_string.append("  }" + lineSep + lineSep);


        // print the test() method
        print_test_method(file_string, param_names, params, all_types,
                          perform_null_substitution(test_string));
        curSplitterObject.setTestString(test_string);

        // print the repr() method
        file_string.append("  public String repr() { " + lineSep);
        file_string.append("    return \"" + splitter_fname + ": \"" + lineSep);
        for (int i = 0; i < num_params; i++) {
          if (params[i] == null) continue;
          String par = param_names[i];
          file_string.append("      + \"" + par + "_varinfo=\" + " + par + "_varinfo.repr() + \" \"" + lineSep);
        }
        file_string.append("      ;" + lineSep);
        file_string.append("  }" + lineSep + lineSep);

        // print the DummyInvariant-related methods
        file_string.append("  public void " +
                           "makeDummyInvariant(DummyInvariant inv) {" +
                           lineSep);
        file_string.append("    dummyInvFactory = inv;" + lineSep);
        file_string.append("  }" + lineSep);
        file_string.append(lineSep);

        file_string.append("  public void instantiateDummy(PptTopLevel ppt) {"
                           + lineSep);
//         file_string.append("    System.err.println(\"In instantiate dummy\");"
//                            + lineSep);
        file_string.append("    dummyInv = null;" + lineSep);
        if (good_param_count >= 1 && good_param_count <= 3) {
          for (int i = 0; i < num_params; i++) {
            String param =  params[i];
            if (param == null) continue;
            String param_name = param_names[i];
            String typ = all_types[i];
            file_string.append("    VarInfo " + param_name
                               + "_vi = ppt.findVarByRepr(\""
                               + all_varinfos[i].name.repr() + "\");"
                               + lineSep);
          }
          // Print DummyInvariant instantiation code
          file_string.append("    if (");
          boolean need_and = false;
          for (int i = 0; i < num_params; i++) {
            if (params[i] == null) continue;
            if (need_and)
              file_string.append(" && ");
            need_and = true;
            file_string.append(param_names[i] + "_vi != null");
          }
          file_string.append(") {" + lineSep);
          file_string.append("      " +
                             "dummyInv = dummyInvFactory.instantiate(ppt, " +
                             "new VarInfo[] {");
          boolean need_comma = false;
          for (int i = 0; i < num_params; i++) {
            if (params[i] == null) continue;
            if (need_comma)
              file_string.append(", ");
            need_comma = true;
            file_string.append(param_names[i] + "_vi");
          }
          file_string.append("});" + lineSep);
          file_string.append("    }" + lineSep);
        }
        file_string.append("  }" + lineSep);
        file_string.append(lineSep);


        file_string.append("  public DummyInvariant getDummyInvariant() {"
                           + lineSep);
        file_string.append("    return dummyInv;" + lineSep);
        file_string.append("  }" + lineSep);

        // finish off the class
        file_string.append("}" + lineSep);

        // write to the file. Assuming that the tempdir has already been set
        String basename = tempdir + splitter_fname;
        try {
          BufferedWriter writer = UtilMDE.BufferedFileWriter(basename + ".java");
          if (dkconfig_delete_splitters_on_exit) {
            (new File (basename + ".java")).deleteOnExit();
            (new File (basename + ".class")).deleteOnExit();
          }
          writer.write(file_string.toString());
          writer.flush();
        } catch (IOException ioe) {
          debugPrintln("Error while writing Splitter file " + basename + ".java " + lineSep);
          debugPrintln(ioe.toString());
        }
      }
    }
  }

  // Declared variable names in the Splitter class cannot have
  // characters like ".", "(" etc.  Change, for example, "node.parent"
  // to "node_parent" and orig(x) to orig_x.
  static String cleanup_varname(String varname) {
    varname = varname.replace('.','_');
    varname = varname.replace('[','_');
    varname = varname.replace(']','_');
    varname = varname.replace(':','_');
    varname = varname.replace('$','_');
    varname = varname.replace('@','_');
    varname = varname.replace('+','P');
    varname = varname.replace('-','M');
    if (varname.equals("return")) varname = "return_Daikon";
    if (varname.equals("this")) varname = "this_Daikon";
    varname = replace_orig(varname);
    return varname;
  }

  /**
   * Find a program point in daikon whose name matches "ppt_name".
   * ppt_name is usually of the form "MethodName.functionName"
   **/
  static PptTopLevel find_corresponding_ppt(String ppt_name, PptMap all_ppts) {
    Object exact_result = all_ppts.get(ppt_name);
    if (exact_result != null) {
      return (PptTopLevel) exact_result;
    }

    // look for corresponding EXIT ppt. This is because the exit ppt usually has
    // more relevant variables in scope (eg. return, hashcodes) than the enter.
    Vector corresponding_ppts = new Vector();
    // if (ppt_name.endsWith(":::ENTER")) {
    //   ppt_name = qm(ppt_name.substring(0, ppt_name.length()-5)) + "EXIT";
    // }
    int index = ppt_name.indexOf("OBJECT");
    if (index == -1) {
      // Didn't find "OBJECT" suffix; add ".*EXIT".
      ppt_name = qm(ppt_name) + ".*EXIT";
    } else {
      // Found "OBJECT" suffix.
      if (ppt_name.length() > 6)
        ppt_name = ppt_name.substring(0, index-1) + ":::OBJECT";
    }
    Pattern ppt_pattern;
    try {
      ppt_pattern = re_compiler.compile(ppt_name);
    } catch (MalformedPatternException e) {
      e.printStackTrace();
      throw new Error("Error in regexp ppt_name: " + e.toString());
    }

    Iterator ppt_itor = all_ppts.pptIterator();

    while (ppt_itor.hasNext()) {
      String name = ((PptTopLevel)ppt_itor.next()).name;
      if (re_matcher.contains(name, ppt_pattern)) {
        // return more than one? do more than one match??
        // XXX In particular, we get in trouble here if the method
        // name we want is a prefix of other method names. For
        // instance, if there's both a frob and a frobAll method, both
        // Pkg.frob(int):::EXIT8 and Pkg.frobAll(int[])::EXIT12 could
        // match /Pkg.frob.*EXIT/, and it's luck of the draw as to
        // which one we get. A workaround is to put "Pkg.frob(",
        // rather than just "Pkg.frob", in your .spinfo file. -smcc

        return all_ppts.get(name);
      }
    }
    return null;
  }


  static String replace_orig(String orig_string) {
    String result = orig_string;
    String str;
    // Keep replacing until the result is unchanged. Even with the
    // SUBSTITUTE_ALL, we need to make multiple passes because orig(),
    // post() and size() can nest.
    do {
      str = result;
      result = Util.substitute(re_matcher, find_orig_pattern, orig_subst,
                               str, Util.SUBSTITUTE_ALL);
    } while (!result.equals(str));
    debugPrintln("Rewriting " + orig_string + " into " + result);
    return result;
  }


  /**
   * Extract the in-scope variables and their corresponding types from the
   * program point.
   * @return a three-element array of Vector<VarInfo>, Vector<String>, Vector<String>
   **/
  static Vector[] get_params_and_types(PptTopLevel ppt) {
    debugPrintln("get_params_and_types(" + ppt.name + ")");

    Vector varinfos = new Vector();
    Vector parameters = new Vector();
    Vector types = new Vector();

    VarInfo[] var_infos = ppt.var_infos;
    for (int i = 0; i < var_infos.length; i++) {
      VarInfo vi = var_infos[i];

      // Omit ".class" variables.
      if (vi.name instanceof VarInfoName.TypeOf) {
        continue;
      }

      debugPrintln("get_params_and_types: adding " + vi.name.name());

      varinfos.addElement(vi);
      {
        String paramname = vi.name.name();
        if (vi.type.isArray()) {
          if (vi.file_rep_type == ProglangType.HASHCODE) {
            paramname += "_identity";
            debugPrintln("identity: " + vi.name.name());
          } else {
            if (paramname.endsWith("[]")) { // always true?
              paramname = paramname.substring(0, paramname.length() - 2);
            }
            paramname += "_array";
            debugPrintln("array: " + vi.name.name());
            debugPrintln("  rep_type = " + vi.rep_type);
          }
        }
        parameters.addElement(paramname);
      }

      // By default, use the rep_type; but in a few special cases, override
      // with the actual type instead.
      // We don't want hashcodes. We just want the variable values.
      if (vi.file_rep_type == ProglangType.HASHCODE) {
        types.addElement("int");
      } else if ((vi.type == ProglangType.CHAR_ARRAY)
          || (vi.type == ProglangType.BOOLEAN)
          || (vi.type == ProglangType.DOUBLE)
          || (vi.type == ProglangType.DOUBLE_ARRAY)) {
        types.addElement(vi.type.format());
      } else {
        types.addElement(vi.rep_type.format());
      }
    }
    Vector[] return_vector = { varinfos, parameters, types };
    return return_vector;
  }


  /**
   * For example the function "Max(int a, int b)" designated to be replaced
   * by "a > b ? a : b" is represended as follows.  The regexp is
   * "\bMax\s*\(\s*(.*),\s*(.*)\)", which matches any function call of Max with
   * two arguments. The arguments are the Vector ["a", "b"].  The expression
   * is "a > b ? a : b".
   **/
  static private class Replacement {
    String regexp;
    String[] arguments;
    String expression;
    Replacement(String regexp, String[] arguments, String expression) {
      this.regexp = regexp;
      this.arguments = arguments;
      this.expression = expression;
    }
  }


  /**
   * Get the function names and arguments of the functions to be replaced.
   *
   * @return Vector of Replacement objects
   **/
  static Vector get_fnames_and_args(Vector replace // [String] but paired
                                    )
  {
    Vector replace_data = new Vector();
    for (int i = 0; i < replace.size(); i+=2) {
      // try {
        String replace_function = (String)replace.elementAt(i); // eg Max(int a, int b)
        if (re_matcher.contains(replace_function, arg_pattern)) {
          MatchResult result = re_matcher.getMatch();
          String function_name = result.group(1);  // Max
          String arguments = result.group(2); // int a, int b
          // the arguments are in the form "type1 name1, type name2, type3 name3, ..."
          StringTokenizer split_args = new StringTokenizer(arguments.trim(), ",");
          Vector tempargs = new Vector();
          while (split_args.hasMoreTokens()) {
            String arg = split_args.nextToken().trim();
            // each argument is now of the form "type name" after splitting using ","
            StringTokenizer extract_argname = new StringTokenizer(arg);
            extract_argname.nextToken(); // throw away the type of the argument
            tempargs.addElement(extract_argname.nextToken().trim()); // the argument name
          }
          int num_args = tempargs.size();
          // This does not work right for "Max(a, Max(b,c)): it mis-pairs the
          // parentheses.
          String fname_regexp = "\\b" + function_name + "\\s*\\(\\s*";
          for (int j = 0; j < num_args; j++) {
            fname_regexp = fname_regexp + "\\s*(\\S*)";
            if (j+1 < num_args) {
              fname_regexp = fname_regexp + "\\s*,";
            }
          }
          fname_regexp = fname_regexp + "\\s*\\)";

          Replacement replacement
            = new Replacement(fname_regexp,
                              (String[])tempargs.toArray(new String[0]),
                              (String)replace.elementAt(i+1));
          replace_data.addElement(replacement);
        }
      // I'm not sure what this was for, so I have temporarily commented it
      // out; it should be removed if it never gets thrown.  -MDE 12/2/2002
      // } catch (ClassCastException e) {
      //   System.out.println(e.toString());
      // }
    }

    return replace_data;
  }

  /**
   * Replace the function call with its replacement expression.  In
   * doing this, do a replacement of the arguments too.
   *
   * @param replace_data vector of Replacement objects
   **/
  static String replace_condition(String condition,
                                  Vector replace_data
                                  )
  {
    for (int i = 0; i < replace_data.size(); i++) {
      try {
        // search for the expression
        Replacement repl = (Replacement)replace_data.elementAt(i);
        Pattern replace_expr_pattern = re_compiler.compile( repl.regexp );
        PatternMatcherInput input = new PatternMatcherInput(condition);
        while (re_matcher.contains(input, replace_expr_pattern)) {
          MatchResult result = re_matcher.getMatch();
          condition = Util.substitute(re_matcher, replace_expr_pattern, octothorpe_subst, condition, 1);
          String[] arguments = repl.arguments;
          String replacement = repl.expression;
          // replace the arguments and replace them
          for (int j = 1; j < result.groups(); j++) {
            Substitution arg_subst = new Perl5Substitution(result.group(j));
            Pattern argument_pattern = re_compiler.compile(arguments[j-1]);
            replacement = Util.substitute(re_matcher, argument_pattern, arg_subst, replacement, Util.SUBSTITUTE_ALL);
          }
          // substitute back into the condition
          replacement = "(" + replacement + ")";
          Substitution replace_subst = new Perl5Substitution(replacement);
          condition = Util.substitute(re_matcher, octothorpe_pattern, replace_subst, condition, 1);
        }
      } catch (MalformedPatternException e) {
        e.printStackTrace();
        throw new Error(e.toString() + " while performing replacement on condition " + condition);
      }
    }
    return condition;
  }

  /**
   * Change array variable names from "a" to "a_array".
   **/
  // keep this method in synch with convert_arraynames_to_arraycontents
  static String convert_arraynames_to_arraycontents(String varname, String test_string) {

    debugPrintln("cantac: " + varname + "  " + test_string);

    // Assert.assertTrue(varname.endsWith("_array"));
    if (varname.endsWith("_array")) {
      varname = varname.substring(0, varname.length() - 6);
    }

    debugPrintln("cantac: " + varname + "  " + test_string);

    String varname_base_regexp
      = (varname.startsWith("this.")
         ? varname_base_regexp = delimit(qm(varname)) + "|" + delimit(qm(varname.substring(5)))
         : delimit(qm(varname)));
    varname_base_regexp = "(" + varname_base_regexp + ")";
    debugPrintln("varname_base_regexp: " + varname_base_regexp);

    // "<arrayname>[i]" or "<arrayname>.length" or "<arrayname>.equals"
    String array_use_regexp = varname_base_regexp + "(\\s*\\[|\\.\\b)";
    debugPrintln("array_use_regexp: " + array_use_regexp);
    // "*<arrayname>"
    String array_dereference_regexp = "(\\*\\s*)" + varname_base_regexp;
    debugPrintln("array_dereference_regexp: " + array_use_regexp);


    Pattern array_use_pattern, array_dereference_pattern;
    try {
      array_use_pattern = re_compiler.compile(array_use_regexp);
      array_dereference_pattern = re_compiler.compile(array_dereference_regexp);
    } catch (MalformedPatternException e) {
      e.printStackTrace();
      throw new Error("Error in regexp: " + e.toString());
    }
    // Substitution array_use_subst = new Perl5Substitution("$1_array$2");
    // Substitution array_dereference_subst = new Perl5Substitution("$1$2_array");
    Substitution add_array_1_subst = new CleanupAppendSubst("_array", 1);
    Substitution add_array_2_subst = new CleanupAppendSubst("_array", 2);


    // Array substitutions
    test_string = Util.substitute(re_matcher, array_use_pattern, add_array_1_subst, test_string, Util.SUBSTITUTE_ALL);
    test_string = Util.substitute(re_matcher, array_dereference_pattern, add_array_2_subst, test_string, Util.SUBSTITUTE_ALL);

    debugPrintln("cantac => " + test_string);

    return test_string;
  }

  /**
   * Change array variable names from "a" to "a_identity".
   **/
  // keep this method in synch with convert_arraynames_to_arraycontents
  static String convert_arraynames_to_arrayidentity(String varname, String test_string) {

    debugPrintln("cantai: " + varname + "  " + test_string);

    // Assert.assertTrue(varname.endsWith("_identity"));
    if (varname.endsWith("_identity")) {
      varname = varname.substring(0, varname.length() - 9);
    }

    debugPrintln("cantai: " + varname + "  " + test_string);

    String varname_base_regexp
      = (varname.startsWith("this.")
         ? varname_base_regexp = delimit(qm(varname)) + "|" + delimit(qm(varname.substring(5)))
         : delimit(qm(varname)));
    varname_base_regexp = "(" + varname_base_regexp + ")";
    debugPrintln("varname_base_regexp: " + varname_base_regexp);

    // "== arrayname" or "arrayname =="
    String pre_equals_regexp = "([!=]=\\s*)" + varname_base_regexp + "($|[^\\[.])";
    String post_equals_regexp = varname_base_regexp + "(\\s*[!=]=)";


    Pattern pre_equals_pattern, post_equals_pattern;
    try {
      pre_equals_pattern = re_compiler.compile(pre_equals_regexp);
      post_equals_pattern = re_compiler.compile(post_equals_regexp);
    } catch (MalformedPatternException e) {
      e.printStackTrace();
      throw new Error("Error in regexp: " + e.toString());
    }


    // // I don't really want to substitute the variable literally; I want
    // // to do varname_cleanup on it first.
    // Substitution add_identity_1_subst = new Perl5Substitution("$1_identity$2");
    // Substitution add_identity_2_subst = new Perl5Substitution("$1$2_identity");
    Substitution add_identity_1_subst = new CleanupAppendSubst("_identity", 1);
    Substitution add_identity_2_subst = new CleanupAppendSubst("_identity", 2);


    // Hashcode substitutions
    test_string = Util.substitute(re_matcher, pre_equals_pattern, add_identity_2_subst, test_string, Util.SUBSTITUTE_ALL);
    test_string = Util.substitute(re_matcher, post_equals_pattern, add_identity_1_subst, test_string, Util.SUBSTITUTE_ALL);

    debugPrintln("cantai => " + test_string);

    return test_string;
  }


  // This class appends a suffix to some group, and also applies
  // cleanup_varname() to that group.
  static class CleanupAppendSubst implements Substitution {
    String suffix;
    int groupno;
    CleanupAppendSubst(String suffix, int groupno) {
      this.suffix = suffix;
      this.groupno = groupno;
    }
    public void appendSubstitution(StringBuffer appendBuffer,
                                   MatchResult match,
                                   int substitutionCount,
                                   PatternMatcherInput ignore, // String originalInput,
                                   PatternMatcher matcher,
                                   Pattern pattern) {
      for (int i=1; i<match.groups(); i++) {
        if (i==groupno) {
          appendBuffer.append( cleanup_varname(match.group(i)) );
          appendBuffer.append( this.suffix );
        } else {
          appendBuffer.append( match.group(i) );
        }
      }
    }
  }


  // Short name for purposes of abbreviation.
  private static String qm(String exp) {
    return re_compiler.quotemeta(exp);
  }

  private static String delimit(String exp) {
    return delimit_end(delimit_beginning(exp));
  }

  private static String delimit_end(String exp) {
    if ((exp == null) || exp.equals(""))
      return exp;
    char lastchar = exp.charAt(exp.length()-1);
    if (Character.isLetterOrDigit(lastchar)
        || lastchar == '_') {
      exp = exp + "\\b";
    }
    return exp;
  }

  private static String delimit_beginning(String exp) {
    if ((exp == null) || exp.equals(""))
      return exp;
    char firstchar = exp.charAt(0);
    if (Character.isLetterOrDigit(firstchar)
        || firstchar == '_') {
      exp = "\\b" + exp;
    }
    return exp;
  }

  /**
   * Find the variables at the program point that apply to this splitter and
   * substitute their correct form in the test string. For example, change
   * this.<varname> in the test_string to this_<varname>.
   * <p>
   * This method also changes any unused parameters in "params" to null,
   * and may side effect elements of "param_names" as well.
   * @param class_name includes a final "."
   **/
  static String find_applicable_variables(String[] params, String[] param_names,
                                          String test_string, String class_name,
                                          VarInfo[] varinfos) {
   debugPrintln("find_applicable_variables(" + utilMDE.ArraysMDE.toString(params) + ", " + utilMDE.ArraysMDE.toString(param_names) + ", " + test_string + ", " + class_name);

   // String orig_test_string = test_string;
   for (int i = 0; i < params.length; i++) {

     debugPrintln("fav considering " + varinfos[i].name.name());

     boolean is_array_identity = false;
     boolean is_array_contents = false;
     VarInfo vi = varinfos[i];
     if (vi.type.isArray()) {
       debugPrintln("is_array: file_rep_type=" + vi.file_rep_type);
       String new_test_string;
       if (vi.file_rep_type == ProglangType.HASHCODE) {
         debugPrintln("is_array_identity: " + params[i] + " " + param_names[i]);
         new_test_string = convert_arraynames_to_arrayidentity(params[i], test_string);
       } else {
         debugPrintln("is_array_contents: " + params[i] + " " + param_names[i]);
         new_test_string = convert_arraynames_to_arraycontents(params[i], test_string);
       }
       if (test_string.equals(new_test_string)) {
         debugPrintln("Removing unused (is_array) param " + params[i] + " " + test_string);
         params[i] = null;
       } else {
         test_string = new_test_string;
         debugPrintln("Changed test_string (for " + vi.name.name() + "): " + test_string);
       }
       // no need to do param_pattern substitution; it has already been done.
       continue;
     }

     // May change param_names[i] by side effect.
     Pattern param_pattern = param_pattern(params[i], param_names, i, class_name, varinfos[i]);

     Substitution param_subst = new Perl5Substitution("$1" + param_names[i] + "$3");
     // Remove any parameters that are not used in the condition.
     debugPrintln("Considering param " + params[i]
                  + " with re_matchers.contains(\"" + test_string
                  + "\", \"" + param_pattern.getPattern() + "\")");
     debugPrintln("  Match = " + re_matcher.contains(test_string,
                                                     param_pattern));
     // This was originally a test against orig_test_string.  Why?
     if (re_matcher.contains(test_string, param_pattern)) {
       test_string = Util.substitute(re_matcher, param_pattern, param_subst, test_string, Util.SUBSTITUTE_ALL);
       debugPrintln("Substituted " + params[i] + " => " + test_string);
     } else {
       debugPrintln("Removing unused param " + params[i] + " after re_maters.contains(\"" + test_string + "\", \"" + param_pattern.getPattern() + "\")");
       params[i] = null;
     }
   }
   {
     // Change "this_Daikon.this_" to "this_".
     int this_loc = test_string.indexOf("this_Daikon.this_");
     while (this_loc != -1) {
       test_string = (test_string.substring(0, this_loc)
                      + test_string.substring(this_loc + 12));
       this_loc = test_string.indexOf("this_Daikon.this_");
     }
   }
   debugPrintln("find_applicable_variables => " + test_string);

   return test_string;
 }

  /**
   * Create a regular expression that matches instances of a particular
   * variable.  The regular expression will be matched against an
   * expression to see whether the expression uses the variable, and will
   * be replaced by a Java-friendly version of the name.  The
   * variable is matched by regexp group 2; groups 1 and 3 hold the
   * preceding and following text, respectively.
   * <p>
   * Also side-effects param_names[i].
   * @param class_name includes a final "."
   **/
  static Pattern param_pattern(String param, String[] param_names, int i, String class_name, VarInfo vi) {
    // Some instrumented variables start with "this." whereas the "this" is
    // not always used in the test string. Eg. instrumented variable is
    // "this.myArray", but the condition test is "myArray.length == 0". In
    // such a situation, search the test_string for this.myArray or myArray
    // and change the test string to this_myArray.length == 0.

    // If the contents of an array are not used, then the array content
    // variable ("a[]", as opposed to the address "a") should not be
    // extracted.  (If the array value is null, then it is an error to
    // access elements; don't do so unless the condition does.)

    String regexp;
    if (param.charAt(0) == '*') {
      // Variable is "*p".
      param_names[i] = "star_" + param.substring(1);
      regexp = delimit(qm(param));
      regexp = "()(" + regexp + ")()";
    } else if (param.startsWith("orig(*")) {
      // Variable is "orig(*p)".
      param_names[i] = "orig_star_" + param.substring(6, param.length() - 1) + "_";
      regexp = delimit_end("orig\\(\\*" + qm(param.substring(6)));
      regexp = "()(" + regexp + ")()";
    } else if (param.startsWith("this.")) {
      // Variable is "this.foo".
      String params_minus_this = param.substring(5);
      // For example for the variable 'this.myArray', we will be searching
      // the condition for the regex "myArray|this.myArray" and replacing
      // it with this_myArray as declared in the Splitter.
      regexp = "(" + delimit(qm(param)) + "|" + delimit(qm(params_minus_this)) + ")";
      regexp = "()" + regexp + "()";
    } else if (!class_name.equals("") && param.startsWith(class_name)) {
      // Variable is a static (class) variable.
      regexp = qm(param) + "|" + qm(param.substring(class_name.length()));
      regexp = "()(" + regexp + ")()";
    } else {
      // All other variables
      if (vi.rep_type.isArray()) {
        // Array contents:  look for "a[" or "*a" or "a.length
        regexp = ("(" + delimit(qm(param + "\\["))
                  + "|" + delimit(qm("*" + param))
                  + ")");
        regexp = "()" + regexp + "()";
      } else {
        // All others, including array addresses
        regexp = delimit(qm(param));
        regexp = "()(" + regexp + ")()";
      }
      // // TEMPORARY OVERRIDE, MUST BE REMOVED.
      // regexp = delimit(qm(param)) + "|" + delimit(qm("*" + param));
      // regexp = "()(" + regexp + ")()";
    }

    debugPrintln("param_pattern(" + param + ") => " + regexp);
    try {
      Pattern param_pattern = re_compiler.compile(regexp);
      return param_pattern;
    }
    catch (MalformedPatternException e) {
      debugPrintln(e.toString());
      e.printStackTrace(System.out);
      throw new Error(e.toString());
    }
  }



  /**
   * Find all variables that are used to index into arrays and change their
   * type to "int" (Most numeric variables have type long in Daikon, apart from
   * array indices which are represented as int). To find array index variables,
   * Find all occurrences of arrayname[varname] in the test_string and
   * mark the variable(s) in the square brackets as array index variables.
   */
  static void identify_array_index_variables_as_int(String condition, String[] params, String[] types) {
    Vector arrayIndexVariables = new Vector();

    PatternMatcherInput input = new PatternMatcherInput(condition);
    while (re_matcher.contains(input, find_index_pattern)) {
      MatchResult result = re_matcher.getMatch();
      String possible_varname = result.group(1);
      Vector tempIndices = new Vector();
      Util.split(tempIndices, re_matcher, operator_pattern, possible_varname);
      for (int i = 0; i < tempIndices.size(); i++) {
        arrayIndexVariables.addElement(((String)tempIndices.elementAt(i)).trim());
      }
    }

    for (int i = 0; i < arrayIndexVariables.size(); i++) {
      for (int j = 0; j < params.length; j++) {
        String variable = (String) arrayIndexVariables.elementAt(i);
        if (params[j] == null) continue;
        if (variable.equals(params[j])) {
          types[j] = "int_index";
        } else if (params[j].startsWith("this.") && (params[j].substring(5)).equals(variable)) {
          types[j] = "int_index";
        }
      }
    }
  }

  // Replace "== null" by "== 0".
  /**
   * Daikon represents the value of null as zero (for some classes of objects).
   * Replace all occurrences of equality tests against null with tests against 0
   * in the condition.
   **/
  static String perform_equals_null_substitution(String test_string) {
    if (re_matcher.contains(test_string, equals_null_pattern)) {
      test_string =
        Util.substitute(re_matcher, equals_null_pattern, equals_null_subst, test_string, Util.SUBSTITUTE_ALL);
    }
    return test_string;
  }

  // Replace "null" by "0".
  /**
   * Daikon represents the value of null as zero, so replace "null" by "0".
   **/
  static String perform_null_substitution(String test_string) {
    return Util.substitute(re_matcher, null_pattern, zero_substitution, test_string, Util.SUBSTITUTE_ALL);
  }

  /**
   * get the name of the temporary directory. This is where the Splitters are created.
   **/
  static String getTempdirName() {

    try {
      String fs = File.separator;
      String path = System.getProperty("java.io.tmpdir") + fs + System.getProperty("user.name") + fs;
      File pathFile =  new File(path);
      pathFile.mkdirs();
      File tmpfile = File.createTempFile("daikon_", "_", pathFile);
      File splitdir = new File(tmpfile.getPath() + "Split");
      tmpfile.delete();
      splitdir.mkdirs();
      if (dkconfig_delete_splitters_on_exit) {
        splitdir.deleteOnExit();
      }
      if (splitdir.exists() && splitdir.isDirectory()) {
        tempdir = splitdir.getPath() + File.separator;
      } else {
        tempdir = "";
      }
    } catch (IOException e) {
      debugPrintln(e.toString());
    }
    return tempdir;
  }

  /**
   * Declare the VarInfo for the parameter <parameter> of type <type> in the
   * Java source of the Splitter. For example, for a variable named "myint"
   * of type "int", it would print "VarInfo myint_varinfo" and for an array
   * "myarr", it would print "VarInfo myarr_array_varinfo"
   **/
  static void print_parameter_declarations(StringBuffer splitter_source, String parameter, String type, VarInfo vi) {
    // if (type.equals("char[]")) {
    //   splitter_source.append("  VarInfo " + parameter + "_varinfo;");
    // } else if (type.endsWith("[]")) {
    //   splitter_source.append("  VarInfo " + parameter + "_varinfo;");
    // } else {
    //   splitter_source.append("  VarInfo " + parameter + "_varinfo;");
    // }
    splitter_source.append("  VarInfo " + parameter + "_varinfo;");
    splitter_source.append("	// " + vi.name.name());
    splitter_source.append(lineSep);
  }

  /**
   * Print the test() method of the splitter
   **/
  static void print_test_method(StringBuffer splitter_source, String[] param_names,
                                  String[] params, String[] all_types, String test_string ) {

    splitter_source.append("  public boolean test(ValueTuple vt) {" + lineSep);
    for (int i = 0; i < params.length; i++) {
      if (params[i] == null) continue;
      String type = all_types[i];
      String parameter = param_names[i];
      String get_expr;
      if (type.equals("int_index")) {
        type = "int";
        get_expr = "getIndexValue(vt)";
      } else if (type.equals("boolean")) {
        get_expr = "getIntValue(vt) > 0";
      } else if (type.equals("int")) {
        type = "long";
        get_expr = "getIntValue(vt)";
      } else if (type.equals("int[]")) {
        type = "long[]";
        get_expr = "getIntArrayValue(vt)";
      } else if (type.equals("double")) {
        get_expr = "getDoubleValue(vt)";
      } else if (type.equals("double[]")) {
        get_expr = "getDoubleArrayValue(vt)";
      } else if (type.equals("String") || type.equals("java.lang.String") || type.equals("char[]")) {
        type = "String";
        get_expr = "getStringValue(vt)";
      } else if (type.equals("String[]") || type.equals("java.lang.String[]")) {
        get_expr = "getStringArrayValue(vt)";
      } else {
        debugPrintln("Can't deal with this type " + type + " declared in Splitter File");
        throw new Error("Can't deal with this type " + type + " declared in Splitter File");
      }
      splitter_source.append("    " + type + " " + parameter + " = "
                             + parameter + "_varinfo." + get_expr + ";"
                             + lineSep);
    }

    splitter_source.append("    return ( " + test_string + " ); " + lineSep);
    splitter_source.append("  }" + lineSep + lineSep);
  }

  // Set of all invariants that are good for printing.
  private static HashSet all_conditions = new HashSet();
  private static HashMap pptname_to_conditions = new HashMap();

  // Store the invariant for later printing, if it needs to be stored.
  // To determine whether an invariant should be printed or not, see
  // cases for indiscriminate and non-indiscriminate splitting below.
  private static boolean duplicate_condition (String inv, String pptname) {
    if (!pptname_to_conditions.containsKey(pptname)) {
      pptname_to_conditions.put(pptname, new HashSet());
    }

    // With indiscriminate splitting, we need just one occurence of
    // each splitting condition, and it doesn't matter under which ppt
    // it appears. However with non-indiscriminate splitting, each
    // condition must be printed under every ppt that it appears.
    if (daikon.split.SplitterList.dkconfig_all_splitters) {
      if (all_conditions.contains(inv))
        return true;
      else
        all_conditions.add(inv);
    } else {
      Set conditions = (Set)pptname_to_conditions.get(pptname);
      if (conditions.contains(inv))
        return true;
      else
        conditions.add(inv);
    }

    return false;
  }

  /**
   * Put the parameter names in order of length (longest first).
   */
  private static void sort_params_by_length (Vector params, Vector types, Vector varinfos) {
    int num_params = params.size();

    ParamTypePair[] ptp = new ParamTypePair[num_params];
    for (int i = 0; i < num_params; i++) {
      ptp[i] = new ParamTypePair((String)params.elementAt(i),
                                 (String)types.elementAt(i),
                                 (VarInfo)varinfos.elementAt(i));
    }
    Arrays.sort(ptp, new ParamTypePairComparator());

    params.clear();
    types.clear();
    varinfos.clear();
    for (int i = 0; i < num_params; i++) {
      params.addElement(ptp[i].param);
      types.addElement(ptp[i].type);
      varinfos.addElement(ptp[i].vi);
    }
  }

  // A pair consisting of a parameter and its type.
  static class ParamTypePair {
    String param, type;
    VarInfo vi;

    public ParamTypePair (String param, String type, VarInfo vi) {
      this.param = param;
      this.type = type;
      this.vi = vi;
    }
  }

  // Compares the parameter names in ParamTypePairs by their length
  static class ParamTypePairComparator implements Comparator {
    public int compare (Object a, Object b) {
      ParamTypePair pa = (ParamTypePair) a;
      ParamTypePair pb = (ParamTypePair) b;

      return (pb.param.length() - pa.param.length());
    }
  }

  /**
   * Print out a message if the debugPptSplit variable is set to "true"
   **/
  static void debugPrintln(String s) {
    Global.debugSplit.debug (s);
  }


}
