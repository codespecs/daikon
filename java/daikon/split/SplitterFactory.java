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
 * read_spinfofile( spinfofilename ) which returns a vector containing program
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
          SplitterObject[] spobjects = read_ppt_conditions(reader, pptName);
          if (spobjects != null)
            splitterObjectArrays.addElement(spobjects);
        } else {
          System.err.println("Incorrect format in .spinfo " + infofile
                             + " at line number " + reader.getLineNumber());
        }
      }
    } catch (IOException ioe ) {
      System.err.println(ioe + " \n at line number " + reader.getLineNumber()
                         + " of .spinfo file \n");
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
  static SplitterObject[] read_ppt_conditions(LineNumberReader reader, String pptname)
    throws IOException, FileNotFoundException
  {
    Vector splitterObjects = new Vector(); // [String == splitting condition]
    String line = reader.readLine();
    while ((line != null) && !re_matcher.matches(line, blank_line)) {
      // skip comments
      if (!line.startsWith("#")) {
        String condition = (line.trim());
        if (duplicate_condition(condition, pptname)) {
          line = reader.readLine();
          continue;
        }
        splitterObjects.addElement(new SplitterObject(pptname, condition, tempdir));
        if (re_matcher.contains(condition, null_pattern)) {
          splitterObjects.addElement(new SplitterObject(pptname, perform_null_substitution(condition), tempdir));
        }
      }
      line = reader.readLine();
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
        System.err.println("SplitterFactory: " + ioe.toString()
                           + "\n while writing Splitter source");
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
      errorString.append("\n");
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

    if (splitterObjectArrays.length > 0) {
      System.out.println("Splitters for this run created in " + tempdir + ".... Will " +
                         ((dkconfig_delete_splitters_on_exit == true) ? "": "Not ") + "be deleted on exit");
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
      // collect all the splitters which were not compiled.
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

  // This pattern matches the names of Java source files, without directory
  // name.
  static Pattern splitter_classname_pattern;
  static {
    try {
      splitter_classname_pattern
        = re_compiler.compile("([^" + UtilMDE.quote(File.separator) + "]+)\\.java");
    } catch (MalformedPatternException me) {
      System.err.println("Error while compiling javafile_pattern in SplitterFactory");
      me.printStackTrace();
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
    // System.out.println("find_corresponding_ppt(" + ppt_name + ") => " + ppt);
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
    Vector parameters = params_and_types[0];
    Vector types = params_and_types[1];

    sort_params_by_length(parameters, types);

    // System.out.println("write_function_splitters: parameters.size() = " + parameters.size() + ", type.size() = " + types.size());
    if (parameters.size() > 0 && types.size() > 0) {
      String[] all_params = (String[])parameters.toArray(new String[0]);
      String[] all_types = (String[])types.toArray(new String[0]);
      int num_params = all_params.length;

      // The Splitter variable names corresponding to the variables
      // at the program point.
      ArrayList p_names = new ArrayList();
      for (int i = 0; i < num_params; i++) {
        // Declared variable names in the Splitter class cannot have
        // characters like ".", "(" etc.  Change, for example, "node.parent"
        // to "node_parent" and orig(x) to orig_x.
        String temp = all_params[i];
        temp = temp.replace('.','_');
        temp = temp.replace('[','_');
        temp = temp.replace(']','_');
        temp = temp.replace(':','_');
        temp = temp.replace('$','_');
        temp = temp.replace('+','P');
        temp = temp.replace('-','M');
        if (temp.equals("return")) temp = "return_Daikon";
        if (temp.equals("this")) temp = "this_Daikon";
        temp = replace_orig(temp);
        if (p_names.contains(temp))
          p_names.add(temp + "_2");
        else
          p_names.add(temp);
      }

      String[] param_names = (String[]) p_names.toArray(new String[0]);

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

        String splitter_fname = splittername + "_" + (guid++);
        int lastIndex = splitter_fname.indexOf(File.separator);
        curSplitterObject.setClassName(splitter_fname.substring(lastIndex + 1));
        String class_name = ppt_name.substring(0, ppt_name.indexOf('.')+1);
        curSplitterObject.setGUID(guid);

        // Each Splitter will use a different set of parameters depending on the
        // parameters used in its condition
        String[] params = (String[])all_params.clone();

        // Replace function calls in the condition by their bodies.
        test_string = replace_condition(test_string, replace_data);

        // ensure that the declared variable names in the Splitter match the
        // variable names in the test string. For example: if the condition tests
        // for "this.mylength == 0", the variable corresponding to "this.mylength"
        // in the Splitter is declared as "this_mylength". Therefore change the
        // condition to "this_mylength == 0"

        // By side effect, this changes some elements of "params" to null.
        test_string = find_applicable_variables(params, param_names, test_string, class_name);
        System.out.println("post find_applicable_variables: " + utilMDE.ArraysMDE.toString(params));

        // replace all occurences of "orig(varname)" with "orig_varname" in the condition.

        test_string = replace_orig(test_string);

        // look for all variables which are used as array accessors and change
        // their type to "int_index". This is necessary because daikon represents
        // all numeric types as long. However array accessors must be cast to ints
        // so they can be used to access arrays. Therefore for these variables,
        // we need to call a different method (VarInfo.getIndexValue()) to
        // obtain their values from the VarInfo.

        identify_array_index_variables_as_int(condition, params, all_types);

        StringBuffer file_string = new StringBuffer();
        file_string.append("import daikon.*; \n");
        file_string.append("import daikon.split.*; \n");

        file_string.append("public final class " + splitter_fname + " extends Splitter { \n\n");

        // print the condition() method
        file_string.append("  public String condition () { return\"" +
                           UtilMDE.quote(condition) + "\" ;} \n\n");

        // print the parameters
        for (int i = 0; i < num_params; i++) {
          // the param has been replaced with null if it doesn't appear in the
          // test string. Therefore skip it since the splitter doesn't need any
          // information about it
          String par = param_names[i].trim();
          String typ = all_types[i].trim();
          print_parameter_declarations(file_string, par, typ);
        }

        // print the constructors
        file_string.append("\n  public " + splitter_fname + "() { } \n");

        file_string.append("  public " + splitter_fname + "(Ppt ppt) {  \n");
        for (int i = 0; i < num_params; i++) {
          String param =  params[i];
          if (param == null) continue;
          String param_name = param_names[i];
          String typ = all_types[i];
          if (typ.equals("char[]")) {
            // char[] is not an array
            file_string.append("    " + param_name + "_varinfo = ppt.findVar(VarInfoName.parse(\"" + param + "[]\")) ; \n");
          } else if (typ.endsWith("[]")) {
            // attach "_array" to an array variable name anytime to distinguish
            // it from its hashCode, which has the same name. (eg change
            // "<arrayname>.length" to "<arrayname>_array.length" and
            // <arrayname>[i] to <arrayname>_array[i] so that the array , and not
            // the hashCode of the array is used in the test). The arrayname is
            // also changed to <arrayname>_array in the Splitter.
            file_string.append("    " + param_name + "_array_varinfo = ppt.findVar(VarInfoName.parse(\"" + param + "[]\")) ; \n");
            test_string = distinguish_arraynames_from_hashCodes_in_teststring(param, test_string);
          } else {
            file_string.append("    " + param_name + "_varinfo = ppt.findVar(VarInfoName.parse(\"" + param + "\")) ; \n");
          }
        }
        // file_string.append("    instantiated = true;\n");
        file_string.append("  }\n\n");

        // print the instantiate method.
        file_string.append("  public Splitter instantiate(Ppt ppt) { \n    return new ");
        file_string.append(splitter_fname + "(ppt);\n  } \n\n");

        // print the valid() method
        file_string.append("  public boolean valid() { \n    return(");
        for (int i = 0; i < param_names.length; i++) {
          if (params[i] == null) continue;
          if (all_types[i].endsWith("[]") && !all_types[i].equals("char[]")) {
            file_string.append( "(" + param_names[i] + "_array_varinfo != null) && ");
          } else {
            file_string.append("(" + param_names[i] + "_varinfo != null) && ");
          }

        }
        file_string.append(" true );\n  }\n\n");


        // print the test() method
        test_string = print_test_method(file_string, param_names, params, all_types, test_string);
        curSplitterObject.setTestString(test_string);

        // print the repr() method
        file_string.append("  public String repr() { \n    return \"" + splitter_fname + ": \"");
        for (int i = 0; i < num_params; i++) {
          if (params[i] == null) continue;
          String par = param_names[i].trim();
          if (all_types[i].endsWith("[]") && !all_types[i].equals("char[]")) {
            file_string.append(" + \"" + par + "_varinfo=\" + " + par + "_array_varinfo.repr() + \" \"\n");
          } else {
            file_string.append(" + \"" + par + "_varinfo=\" + " + par + "_varinfo.repr() + \" \"\n");
          }
        }
        file_string.append("        ;\n  }\n\n");

        // finish off the class
        file_string.append("}\n");

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
          debugPrint("Error while writing Splitter file " + basename + ".java \n");
          debugPrint(ioe.toString());
        }
      }
    }
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
    try {
      Pattern ppt_pattern = re_compiler.compile(ppt_name);
      Iterator ppt_itor = all_ppts.pptIterator();

      while (ppt_itor.hasNext()) {
        String name = ((PptTopLevel)ppt_itor.next()).name;
        if (re_matcher.contains(name, ppt_pattern)) {
          // return more than one? do more than one match??
          return all_ppts.get(name);
        }
      }
    } catch (MalformedPatternException e) {
      debugPrint(e.toString() + " while matching " + ppt_name);
    }
    return null;
  }


  static Pattern find_orig_pattern;
  static Perl5Substitution orig_subst;
  // This regex pattern is used to search for the variable names of arguments
  // inside function calls.
  static Pattern arg_pattern;
  static {
    try {
      // this regex pattern is used to search for "orig" variable names.
      // it replaces orig(varname) with orig_varname, and similarly for
      // post(varname) and size(varname)
      find_orig_pattern =
        re_compiler.compile("(orig|post|size)\\s*\\(\\s*(\\S*?)\\s*\\)");
      orig_subst =
        new Perl5Substitution("$1_$2", Perl5Substitution.INTERPOLATE_ALL);
      arg_pattern = re_compiler.compile("(\\S+)\\s*\\((.*)\\)");
    } catch (MalformedPatternException me) {
      System.err.println("Error while compiling regular expresssion in SplitterFactory");
    }
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
    // System.out.println("Rewriting " + orig_string + " into " + result);
    return result;
  }

  /**
   * Extract the in-scope variables and their corresponding types from the
   * program point.
   * @return a two-element vector, each of whose elements is a Vector.
   **/
  static Vector[] get_params_and_types(PptTopLevel ppt) {
    // System.out.println("get_params_and_types(" + ppt.name + ")");

    Vector parameters = new Vector();
    Vector types = new Vector();

    VarInfo[] var_infos = ppt.var_infos;
    for (int i = 0; i < var_infos.length; i++) {
      VarInfo vi = var_infos[i];
      // System.out.println("get_params_and_types: considering " + vi.name.name());
      // we don't want hashcodes. We just want the variable values
      if (vi.file_rep_type == ProglangType.HASHCODE) {
        parameters.addElement(vi.name.name().trim());
        types.addElement("int");
        continue;
      }
      String temp = vi.name.name().trim();
      if (temp.endsWith(".class"))
        continue;
      if (temp.endsWith("[]")) {
        // strip off the brackets and search for the variable name in the test string.
        temp = temp.substring(0, temp.length() - 2);
      }
      parameters.addElement(temp);
      // do rep_type changes here.
      if (vi.type.format().trim().equals("char[]")) {
        // if char[], we need to treat as a String in Daikon
        types.addElement ("char[]");
      } else if (vi.type.format().trim().equals("boolean")) {
        types.addElement("boolean");
      } else if (vi.type.format().trim().equals("double")) {
        types.addElement("double");
      } else if (vi.type.format().trim().equals("double[]")) {
        types.addElement("double[]");
      } else {
        types.addElement(vi.rep_type.format().trim());
      }
    }
    Vector[] return_vector = { parameters, types };
    return return_vector;
  }


  /**
   * For example the function "Max(int a, int b)" designated to be replaced
   * by "a > b ? a : b" is represended as follows.  The regexp is
   * "\bMax\s*\(\s*(.*),\s*(.*)\)" which matches any function call of Max with
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
        PatternMatcherInput replace_function_pattern = new PatternMatcherInput(replace_function);
        if (re_matcher.contains(replace_function_pattern, arg_pattern)) {
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
          Perl5Substitution temp_subst = new Perl5Substitution("#", Perl5Substitution.INTERPOLATE_ALL);
          condition = Util.substitute(re_matcher, replace_expr_pattern, temp_subst, condition, 1);
          String[] arguments = repl.arguments;
          String replacement = repl.expression;
          // replace the arguments and replace them
          for (int j = 1; j < result.groups(); j++) {
            Perl5Substitution arg_subst = new Perl5Substitution(result.group(j));
            Pattern argument_pattern = re_compiler.compile(arguments[j-1]);
            replacement = Util.substitute(re_matcher, argument_pattern, arg_subst, replacement, Util.SUBSTITUTE_ALL);
          }
          // substitute back into the condition
          replacement = "(" + replacement + ")";
          Perl5Substitution replace_subst =
            new Perl5Substitution(replacement, Perl5Substitution.INTERPOLATE_ALL);
          condition = Util.substitute(re_matcher, re_compiler.compile("#"), replace_subst, condition, 1);
        }
      } catch (MalformedPatternException e) {
        debugPrint( e.toString() + " while performing replacement on condition " + condition);
      }
    }
    return condition;
  }

  /**
   * fix the variable names (of arrays) in the condition to match the declared
   * variable names in the Splitter. Anytime you see a <varname>.length or
   * <varname>[] being used in a condition, add a "_array" to the varname.
   **/
  static String distinguish_arraynames_from_hashCodes_in_teststring(String varname, String test_string) {

    String dot_length; // The regular expression used to search for "<arrayname>.length"
    String bracket;  // the regular expression used to search for "<arrayname>[]"

    if (varname.startsWith("this.")) {
        bracket = "(" + varname + "|" + varname.substring(5) ;
        dot_length = "(" + varname + "|" + varname.substring(5);
    } else {
        bracket = "(" + varname ;
        dot_length = "(" + varname ;
    }

    bracket = bracket + ")(\\s*\\[[^\\]]*)";
    dot_length = dot_length  + ")\\.length";

    // try performing the substitution on every condition
    Pattern brack_pattern, length_pattern;
    Perl5Substitution brack_subst, length_subst;
    try {
      brack_pattern = re_compiler.compile(bracket);
      brack_subst = new Perl5Substitution("$1_array$2", Perl5Substitution.INTERPOLATE_ALL);

      if (re_matcher.contains(test_string, brack_pattern)) {
        test_string =
          Util.substitute(re_matcher, brack_pattern, brack_subst, test_string, Util.SUBSTITUTE_ALL);
      }

      length_pattern = re_compiler.compile(dot_length);
      length_subst = new Perl5Substitution("$1_array.length", Perl5Substitution.INTERPOLATE_ALL);

      if (re_matcher.contains(test_string, length_pattern)) {
        test_string =
          Util.substitute(re_matcher, length_pattern, length_subst, test_string, Util.SUBSTITUTE_ALL);
      }
    } catch (MalformedPatternException e) {
      debugPrint(e.toString() + "\n while performing substitution on teststring " + test_string + "\n");
    }
    return test_string;
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
   * Find the variables at the program point which apply to this splitter and
   * substitute their correct form in the test string. For example, change
   * this.<varname> in the test_string to this_<varname>.
   * This method also changes any unused parameters in "params" to null.
   **/
  static String find_applicable_variables(String[] params, String[] param_names,
                                          String test_string, String class_name ) {
    // System.out.println("find_applicable_variables(" + utilMDE.ArraysMDE.toString(params) + ", " + utilMDE.ArraysMDE.toString(param_names) + ", " + test_string + ", " + class_name);

    String orig_test_string = test_string;
    for (int i = 0; i < params.length; i++) {

       Pattern param_pattern;
        // some instrumented variables start with "this." whereas the "this" is
        // not always used in the test string. Eg. instrumented variable is
        // "this.myArray", but the condition test is "myArray.length == 0". In
        // such a situation, search the test_string for this.myArray or myArray
        // and change the test string to this_myArray.length == 0.
        try {
          if (params[i].charAt(0) == '*') {
            param_names[i] = "star_" + params[i].substring(1);
            param_pattern = re_compiler.compile(delimit(qm(params[i])));
          } else if (params[i].startsWith("orig(*")) {
            param_names[i] = "orig_star_" + params[i].substring(6, params[i].length() - 1) + "_";
            param_pattern = re_compiler.compile(delimit_end("orig\\(\\*" + qm(params[i].substring(6))));
          } else if (params[i].startsWith("this.")) {
            String params_minus_this = params[i].substring(5);
            // for example for the variable 'this.myArray', we will be searching
            // the condition for the regex "myArray|this.myArray" and replacing
            // it with this_myArray as declared in the Splitter.
            param_pattern = re_compiler.compile("(" + delimit(qm(params[i])) + "|" + delimit(qm(params_minus_this)) + ")");
          } else if (!class_name.equals("") && params[i].startsWith(class_name)) {
            // static variable
            param_pattern = re_compiler.compile(qm(params[i]) + "|" + qm(params[i].substring(class_name.length())));
          } else if (params[i].startsWith("orig")) {
            // we've already substituted for example orig(this.Array) with "orig(this_theArray)",
            // so search for "orig(this_theArray)" in the test_string
            String temp = param_names[i].replace('.','_');
            String search_string = "orig\\s*\\(\\s*" + temp.substring(5) + "\\s*\\)";
            if (temp.length() > 10) {
              search_string = search_string + "|" + "orig\\s*\\(\\s*" + qm(temp.substring(10)) + "\\s*\\)";
            }
            param_pattern = re_compiler.compile(search_string);
          } else if (params[i].charAt(0) == '$') {
            // remove unwanted characters from the param name. These confuse the regexp.
            // (find a better solution for arbitrary characters at arbitrary locations)
            param_pattern = re_compiler.compile(delimit(qm(params[i].substring(1))));
          } else {
            // to take care of pointer variables too, we search for "(varname|*varname)"
            param_pattern = re_compiler.compile("(" + delimit(qm(params[i])) + "|" + delimit(qm("*" + params[i])) + ")");
          }
          Perl5Substitution param_subst = new Perl5Substitution(param_names[i], Perl5Substitution.INTERPOLATE_ALL);
          PatternMatcherInput input = new PatternMatcherInput(orig_test_string);
          // remove any parameters which are not used in the condition
          // System.out.println("Considering param " + params[i] + " with re_maters.contains(\"" + input + "\", \"" + param_pattern.getPattern() + "\")");
          if (re_matcher.contains(input, param_pattern)) {
            test_string = Util.substitute(re_matcher, param_pattern, param_subst, test_string, Util.SUBSTITUTE_ALL);
          } else {
            // System.out.println("Removing unused param " + params[i] + " after re_maters.contains(\"" + input + "\", \"" + param_pattern.getPattern() + "\")");
            params[i] = null;
          }
        } catch (MalformedPatternException e) {
          debugPrint(e.toString());
        }
    }
      {
        // Change "this_Daikon.this_" to "this_"
        int this_loc = test_string.indexOf("this_Daikon.this_");
        while (this_loc != -1) {
          test_string = (test_string.substring(0, this_loc)
                         + test_string.substring(this_loc + 12));
          this_loc = test_string.indexOf("this_Daikon.this_");
        }
      }
      return test_string;
  }

  static Pattern find_index_pattern;
  static {
    try {
      // this regex pattern is used to search for variables which might act as array indices.
      // ie. variable names inside square brackets
      find_index_pattern = re_compiler.compile("\\[\\s*([^\\])]*)\\s*\\]");
    } catch (MalformedPatternException me) {
      System.err.println("Error while compiling regular expresssion find_index_pattern in SplitterFactory");
    }
  }

  static Pattern blank_line;
  static {
    try {
      blank_line  = re_compiler.compile("^\\s*$");
    } catch (MalformedPatternException me) {
      System.out.println("Error while compiling regular expression " + me.toString());
    }
  }

    /**
     * Find all variables which are used to index into arrays and change their
     * type to "int" (Most numeric variables have type long in Daikon, apart from
     * array indices which are represented as int). To find array index variables,
     * Find all occurences of arrayname[varname] in the test_string and
     * mark the variable(s) in the square brackets as array index variables.
     */
    static void identify_array_index_variables_as_int(String condition, String[] params, String[] types) {
        Vector arrayIndexVariables = new Vector();
        try {

            PatternMatcherInput input = new PatternMatcherInput(condition);
            while (re_matcher.contains(input, find_index_pattern)) {
                MatchResult result = re_matcher.getMatch();
                String possible_varname = result.group(1);
                // if we have say myarray[ i + j ], we have to identify both i and j
                // as array index variables and treat them as integers.
                Pattern operator_pattern = re_compiler.compile("[+=!><-]"); // split on this pattern
                Vector tempIndices = new Vector();
                Util.split(tempIndices, re_matcher, operator_pattern, possible_varname);
                for (int i = 0; i < tempIndices.size(); i++) {
                  arrayIndexVariables.addElement(((String)tempIndices.elementAt(i)).trim());
                }
            }
        } catch (MalformedPatternException e) {
          debugPrint(e.toString());
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

  static Pattern null_pattern;
  static {
    try {
      null_pattern = re_compiler.compile("(!|=)=\\s*null");
    } catch (MalformedPatternException e) {
      debugPrint("Error compiling regex (!|=)=\\s*null");
    }
  }

  // Replace "== null" by "== 0".
  /**
   * Daikon represents the value of null as zero (for some classes of objects).
   * Replace all occurences of equality tests against null with tests against 0
   * in the condition.
   **/
  static String perform_null_substitution(String test_string) {
    Perl5Substitution null_subst = new Perl5Substitution(" $1= 0", Perl5Substitution.INTERPOLATE_ALL);
    if (re_matcher.contains(test_string, null_pattern)) {
      test_string =
        Util.substitute(re_matcher, null_pattern, null_subst, test_string, Util.SUBSTITUTE_ALL);
    }
    return test_string;
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
      debugPrint(e.toString());
    }
    return tempdir;
  }

  /**
   * Print out a message if the debugPptSplit variable is set to "true"
   **/
  static void debugPrint(String s) {
    Global.debugSplit.debug (s);
  }

  /**
   * Declare the VarInfo for the parameter <parameter> of type <type> in the
   * Java source of the Splitter. For example, for a variable named "myint"
   * of type "int", it would print "VarInfo myint_varinfo" and for an array
   * "myarr", it would print "VarInfo myarr_array_varinfo"
   **/
  static void print_parameter_declarations(StringBuffer splitter_source, String parameter, String type) {
    if (type.equals("char[]")) {
      splitter_source.append("  VarInfo " + parameter + "_varinfo; \n");
    } else if (type.endsWith("[]")) {
      splitter_source.append("  VarInfo " + parameter + "_array_varinfo; \n");
    } else {
      splitter_source.append("  VarInfo " + parameter + "_varinfo; \n");
    }
  }

  /**
   * Print the test() method of the splitter
   **/
  static String print_test_method(StringBuffer splitter_source, String[] param_names,
                                        String[] params, String[] all_types, String test_string ) {

    splitter_source.append("  public boolean test(ValueTuple vt) { \n");
    for (int i = 0; i < params.length; i++) {
      if (params[i] == null) continue;
      String type = all_types[i];
      String parameter = param_names[i];
      if (type.equals("int_index")) {
        splitter_source.append("   int " + parameter + " = "
                               + parameter + "_varinfo.getIndexValue(vt); \n");
      } else if (type.equals("boolean")) {
        // we get the boolean as an int
        splitter_source.append("    boolean " + parameter + " = (" + parameter
                               + "_varinfo.getIntValue(vt) > 0 ? true : false ); \n");
      } else if (type.equals("int")) {
        splitter_source.append("    long " + parameter + " = "
                               + parameter + "_varinfo.getIntValue(vt); \n");
      } else if (type.equals("int[]")) {
        splitter_source.append("  long[] " + parameter + "_array = " + parameter
                               + "_array_varinfo.getIntArrayValue(vt); \n");
      } else if (type.equals("double")) {
        splitter_source.append("    double " + parameter + " = "
                               + parameter + "_varinfo.getDoubleValue(vt); \n");
      } else if (type.equals("double[]")) {
        splitter_source.append("  long[] " + parameter + "_array = " + parameter
                               + "_array_varinfo.getDoubleArrayValue(vt); \n");
      } else if (type.equals("String") || type.equals("java.lang.String") || type.equals("char[]")) {
        splitter_source.append("    String " + parameter + " = "
                               + parameter + "_varinfo.getStringValue(vt); \n");
      } else if (type.equals("String[]") || type.equals("java.lang.String[]")) {
        splitter_source.append("    String[] " + parameter + "_array = "
                               + parameter + "_array_varinfo.getStringArrayValue(vt); \n");
      } else {
        debugPrint("Can't deal with this type " + type + " declared in Splitter File");
      }
    }

    splitter_source.append("    return( " + test_string + " ); \n  }\n");
    return test_string;
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
  private static void sort_params_by_length (Vector params, Vector types) {
    int num_params = params.size();

    paramTypePair[] ptp = new paramTypePair[num_params];
    for (int i = 0; i < num_params; i++) {
      ptp[i] = new paramTypePair((String)params.elementAt(i), (String)types.elementAt(i));
    }
    Arrays.sort(ptp, new paramTypePairComparator());

    params.clear();
    types.clear();
    for (int i = 0; i < num_params; i++) {
      params.addElement(ptp[i].getParam());
      types.addElement(ptp[i].getType());
    }
  }

  // A pair consisting of a parameter and its type.
  static class paramTypePair {
    String param, type;

    public paramTypePair (String param, String type) {
      this.param = param;
      this.type = type;
    }

    public String getParam () {
      return this.param;
    }

    public String getType () {
      return this.type;
    }
  }

  // Compares the parameter names in paramTypePairs by their length
  static class paramTypePairComparator implements Comparator {
    public int compare (Object a, Object b) {
      paramTypePair pa = (paramTypePair) a;
      paramTypePair pb = (paramTypePair) b;

      return ((pb.getParam()).length() - (pa.getParam()).length());
    }
  }
}
