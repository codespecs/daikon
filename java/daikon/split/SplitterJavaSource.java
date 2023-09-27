package daikon.split;

import daikon.Global;
import daikon.ProglangType;
import daikon.VarInfo;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import jtb.ParseException;
import jtb.syntaxtree.NodeToken;
import org.checkerframework.checker.initialization.qual.UnknownInitialization;
import org.checkerframework.checker.regex.qual.Regex;
import org.checkerframework.dataflow.qual.Pure;

/**
 * SplitterJavaSource writes the splitter Java file's contents to a string buffer for a given
 * condition, Ppt, and StatementReplacer.
 */
class SplitterJavaSource implements jtb.JavaParserConstants {

  /** The text contents of the splitter file, a java class. */
  private StringBuilder fileText = new StringBuilder();

  /** The name of the class from which this Ppt is from. */
  private String className;

  /** The name of the class that is being written. */
  private String fileName;

  /**
   * A data structure defined in an inner-class that contains information about all the variables to
   * be defined in fileText.
   */
  private VariableManager[] vars;

  /** A StatementReplacer for the .spinfo from which this file is being created. */
  private StatementReplacer statementReplacer;

  /** Java reserved words that are replaced by replaceReservedWords. */
  private static final @Regex String[] reservedWords = new @Regex String[] {"return"};

  private static final String lineSep = System.lineSeparator();

  /**
   * Creates a new instance of SplitterJavaSource.
   *
   * @param splitObj the SplitterObject for which this splitter java file is be written
   * @param pptName the name of the Ppt to which splitObj belongs
   * @param fileName the name of the file that should be written by the new SplitterJavaSource
   * @param varInfos the varInfos for this the Ppt to which splitObj belongs
   * @param statementReplacer a statementReplacer for the {@code .spinfo} file from which {@code
   *     splitObj} is being created
   */
  public SplitterJavaSource(
      SplitterObject splitObj,
      String pptName,
      String fileName,
      VarInfo[] varInfos,
      StatementReplacer statementReplacer)
      throws ParseException {
    className = getClassName(pptName);
    Global.debugSplit.fine(
        "<<enter>> SplitterJavaSource; pptName: " + pptName + ", className: " + className);
    this.fileName = fileName;
    this.statementReplacer = statementReplacer;
    varInfos = filterNonVars(varInfos);
    String originalCondition = splitObj.condition();
    Global.debugSplit.fine("originalCondition =  " + originalCondition);
    String condition = replaceReservedWords(originalCondition);
    condition = this.statementReplacer.makeReplacements(condition);
    condition = convertVariableNames(condition, className, varInfos);
    Global.debugSplit.fine("modified condition = " + condition);
    vars = makeVariableManagerArray(varInfos, condition);

    // extra white space at the end of lines used only to increase readability.
    add("import daikon.*;");
    add("import daikon.inv.*;");
    add("import daikon.split.*;");
    skipLine();
    add("public final class " + fileName + " extends Splitter {");
    skipLine();
    add("  public String condition() {");
    add("    return \"" + protectQuotations(originalCondition) + "\";");
    add("  }");
    skipLine();
    writeFields();
    skipLine();
    add("  static DummyInvariant dummyInvFactory;");
    add("  DummyInvariant dummyInv;");
    skipLine();
    add("  public " + fileName + "() { }");
    add("  public " + fileName + "(Ppt ppt) {");
    writeConstructorBody();
    add("  }");
    skipLine();
    add("  public Splitter instantiateSplitter(Ppt ppt) {");
    add("    return new " + fileName + "(ppt);");
    add("  }");
    skipLine();
    add("  public boolean valid() {");
    writeValidBody();
    add("  }");
    skipLine();
    add("  public boolean test(ValueTuple vt) {");
    writeTestBody();
    add("    return(" + NullReplacer.replaceNull(condition) + ");");
    add("  }");
    skipLine();
    add("  public String repr() {");
    writeReprBody();
    add("  }");
    skipLine();
    add("  public void makeDummyInvariantFactory(DummyInvariant inv) {");
    add("    dummyInvFactory = inv;");
    add("  }");
    skipLine();
    add("  public void instantiateDummy(PptTopLevel ppt) {");
    add("    dummyInv = null;");
    writeInstantiateDummyBody();
    add("  }");
    skipLine();
    add("  public DummyInvariant getDummyInvariant() {");
    add("    return dummyInv;");
    add("  }");
    skipLine();
    add("  public int[] getIntArray(long[] longArray) {");
    add("    int[] intArray = new int[longArray.length];");
    add("    for (int i = 0; i < intArray.length; i++) {");
    add("      intArray[i] = (int) longArray[i];");
    add("    }");
    add("    return intArray;");
    add("  }");
    skipLine();
    add("}");

    Global.debugSplit.fine("<<exit>>  SplitterJavaSource");
  }

  /** Returns a StringBuilder with the file text for the java file written by this. */
  public StringBuilder getFileText() {
    return fileText;
  }

  /** Writes the field declarations of the class to fileText. */
  private void writeFields(
      @UnknownInitialization(SplitterJavaSource.class) SplitterJavaSource this) {
    for (int i = 0; i < vars.length; i++) {
      add("  VarInfo " + vars[i].getFieldName() + "; // " + vars[i].getNormalName());
    }
  }

  /** Writes the body of the of constructor which takes a Ppt in as an argument. */
  private void writeConstructorBody(
      @UnknownInitialization(SplitterJavaSource.class) SplitterJavaSource this) {
    for (int i = 0; i < vars.length; i++) {
      add(
          "    "
              + vars[i].getFieldName()
              + " = ppt.find_var_by_name(\""
              + vars[i].getNormalName()
              + "\");");
    }
  }

  /** Writes the body of the valid method to fileText. */
  private void writeValidBody(
      @UnknownInitialization(SplitterJavaSource.class) SplitterJavaSource this) {
    if (vars.length > 0) {
      fileText.append("    return (" + vars[0].getFieldName() + " != null)");
      for (int i = 1; i < vars.length; i++) {
        fileText.append(" && (" + vars[i].getFieldName() + " != null)");
      }
      add(";");
    } else {
      add("    /* no variables were found */");
      add("    return false;");
    }
  }

  /** Writes the body of the test method to fileText. */
  private void writeTestBody(
      @UnknownInitialization(SplitterJavaSource.class) SplitterJavaSource this) {
    add("    /* writeTestBody: " + vars.length + " declarations */");
    for (int i = 0; i < vars.length; i++) {
      String type = vars[i].getType();
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
      } else if (type.equals("String")
          || type.equals("java.lang.String")
          || type.equals("char[]")) {
        type = "String";
        get_expr = "getStringValue(vt)";
      } else if (type.equals("String[]") || type.equals("java.lang.String[]")) {
        get_expr = "getStringArrayValue(vt)";
      } else if (type.equals("index[]")) {
        type = "int[]";
        get_expr = "getIntArrayValue(vt)";
      } else {
        Global.debugSplit.fine("Can't deal with this type " + type + " declared in Splitter File");
        throw new Error("Can't deal with this type " + type + " declared in Splitter File");
      }
      if (type.equals("int[]")) {
        add(
            "    "
                + type
                + " "
                + vars[i].getCompilableName()
                + " = "
                + "getIntArray("
                + vars[i].getFieldName()
                + "."
                + get_expr
                + ");");
      } else {
        add(
            "    "
                + type
                + " "
                + vars[i].getCompilableName()
                + " = "
                + vars[i].getFieldName()
                + "."
                + get_expr
                + ";");
      }
    }
  }

  /** Writes the body of the repr method to fileText. */
  private void writeReprBody(
      @UnknownInitialization(SplitterJavaSource.class) SplitterJavaSource this) {
    add("    return \"" + fileName + ": \"");
    for (int i = 0; i < vars.length; i++) {
      add(
          "      + \""
              + vars[i].getFieldName()
              + " = \" + "
              + vars[i].getFieldName()
              + ".repr() + \" \"");
    }
    add("      ;");
  }

  /** Writes the body of the instantiateDummy method to fileText. */
  private void writeInstantiateDummyBody(
      @UnknownInitialization(SplitterJavaSource.class) SplitterJavaSource this) {
    if (vars.length >= 1 && vars.length <= 3) {
      for (int i = 0; i < vars.length; i++) {
        add(
            "    VarInfo "
                + vars[i].getVarName()
                + " = ppt.find_var_by_name(\""
                + vars[i].getNormalName()
                + "\");");
      }
      fileText.append("    if (");
      fileText.append(vars[0].getVarName() + " != null");
      for (int i = 1; i < vars.length; i++) {
        fileText.append(" && " + vars[i].getVarName() + " != null");
      }
      add(") {");
      fileText.append("      dummyInv = dummyInvFactory.instantiate(ppt, new VarInfo[] {");
      fileText.append(vars[0].getVarName());
      for (int i = 1; i < vars.length; i++) {
        fileText.append(", " + vars[i].getVarName());
      }
      add("});");
      add("    }");
    }
  }

  /**
   * Appends st to fileText and then ends that line with lineSep.
   *
   * @param st the string to added to fileText
   */
  private void add(
      @UnknownInitialization(SplitterJavaSource.class) SplitterJavaSource this, String st) {
    fileText.append(st + lineSep);
  }

  /** Skips a line in fileText by adding a black line to fileText. */
  private void skipLine(@UnknownInitialization(SplitterJavaSource.class) SplitterJavaSource this) {
    fileText.append(lineSep);
  }

  /**
   * Replaces instances of Java reserved words that could not appear in a valid Java condition or
   * Java variable name that are being used as variable names in string.
   *
   * @param string the string in which the Java reserved words should be replaced
   * @return string with the Java reserved words replaced with a substitute names
   */
  private static String replaceReservedWords(String string) {
    // cheap hack so that pattern never need to look for a key word at
    // the beginning or end of string.  That way one may simplify the pattern
    // to looking for a reserved word that is not prefixed or suffix with a
    // letter or number.
    string = "(" + string + ")";
    for (int i = 0; i < reservedWords.length; i++) {
      String reservedWord = reservedWords[i];
      Pattern p = Pattern.compile("([\\W])(" + reservedWord + ")([\\W])");
      Matcher m = p.matcher(string);
      while (m.find()) {
        string = m.replaceFirst(m.group(1) + "daikon" + reservedWord + m.group(3));
        m = p.matcher(string);
      }
    }
    return string.substring(1, string.length() - 1);
  }

  /**
   * Returns a version of this condition in which the variable names are converted to the names that
   * will be used by the java class written to fileText. Instances of "this." are removed. Instances
   * of the package and class names prefixing variable names are removed and reattached to the
   * variable name with a "_" separating the two parts. Instances of a public field name suffixing a
   * variable name are removed and appended to the end of variable name with a "_" separating the
   * two parts. Instances of "orig(variableName)" are replaced by instances of "orig_variableName".
   * For example "orig(varName.publicField)" would yield "orig_varName_publicField".
   *
   * @param condition a string representation of a conditional statement
   * @return a version of the conditional with the variable names converted
   */
  private static String convertVariableNames(String condition, String className, VarInfo[] varInfos)
      throws ParseException {
    // These methods keep converting between strings and jtb syntax trees
    // because the visitors causes the trees to become invalid.  Therefore,
    // it is needed to re-parse the condition in a new jtb syntax tree each
    // time.  (All the parsing is hidden in the static methods.)
    condition = NameFixer.fixUnqualifiedMemberNames(condition, className, varInfos);
    condition = ThisFixer.fixThisUsage(condition, varInfos);
    condition = OrigFixer.fixOrig(condition);
    condition = PrefixFixer.fixPrefix(condition);
    // UNDONE: If the condition contains a naked reference to a class
    // variable, we should prepend the classname.  (markro)
    String[] baseNames = getBaseNames(varInfos);
    condition = ArrayFixer.fixArrays(condition, baseNames, varInfos);
    return condition;
  }

  /** Eliminates all non-normal variables from varInfos. See isNormalVar() for details. */
  private static VarInfo[] filterNonVars(VarInfo[] varInfos) {
    List<VarInfo> filteredList = new ArrayList<>();
    for (VarInfo vi : varInfos) {
      if (isNormalVar(vi)) {
        filteredList.add(vi);
        Global.debugSplit.fine("filterNonVars added   " + vi.name());
      } else {
        Global.debugSplit.fine("filterNonVars removed " + vi.name());
      }
    }
    return filteredList.toArray(new VarInfo[0]);
  }

  /**
   * Determines if the variable represented by varInfo may appear in the splitting condition.
   *
   * @param varInfo the VarInfo for the variable that may be use in the condition
   * @return true iff the variable represented by varInfo may appear in the splitting condition
   */
  @Pure
  private static boolean isNormalVar(VarInfo varInfo) {
    return !isTypeOfVar(varInfo) && !isSizeVar(varInfo) && !isThisVar(varInfo);
  }

  /**
   * Determines if the variable represented by varInfo contains a CLASSNAME variable.
   *
   * @param varInfo the VarInfo of the variable being tested
   * @return true iff varInfo is a CLASSNAME variable
   */
  @Pure
  private static boolean isTypeOfVar(VarInfo varInfo) {
    return varInfo.has_typeof();
  }

  /**
   * Determines if the variable represented by varInfo is a "size" variable.
   *
   * @param varInfo the VarInfo of the variable being tested
   * @return true iff varInfo is a "size" variable
   */
  @Pure
  private static boolean isSizeVar(VarInfo varInfo) {
    return varInfo.is_size();
  }

  /** Determines if the variable represented by varInfo is a "this" variable. */
  @Pure
  private static boolean isThisVar(VarInfo varInfo) {
    return varInfo.isThis();
  }

  /**
   * Protects quotations that appear in fileText by placing "\" in front of quotation marks.
   *
   * @return condition with a backslash placed in front of every quotation mark
   */
  private static String protectQuotations(String condition) {
    for (int i = 0; i < condition.length(); i++) {
      if (condition.charAt(i) == '"') {
        condition = condition.substring(0, i) + "\\" + condition.substring(i, condition.length());
        i = i + 2;
      }
    }
    return condition;
  }

  /** Returns the name of the class from which pptName is from. */
  private static String getClassName(String pptName) {
    int lastIndex = pptName.lastIndexOf('.');
    if (lastIndex != -1) {
      return pptName.substring(0, lastIndex);
    } else {
      return pptName;
    }
  }

  /**
   * Calculates the name of the variable represented by varInfo in a compilable form. Names are the
   * same as base names (see getBaseName) except that the names of arrays are suffixed with
   * "_identity" if it is a variable representing the array for equality tests or "_array" if it is
   * a variable representing the elements of the array.
   *
   * @param varInfo the VarInfo of the variable whose compilable name is desired
   * @return the name of the variable represented by varInfo in a compilable form
   */
  private static String compilableName(VarInfo varInfo) {
    String name = getBaseName(varInfo);
    if (varInfo.type.isArray()) {
      if (varInfo.file_rep_type == ProglangType.HASHCODE) {
        name += "_identity";
      } else {
        name += "_array";
      }
    }
    return name;
  }

  /**
   * Calculates the base name of a variable. The base name of a variable is the part of the variable
   * with prefix "this." replaced by "this_", and "orig()" replaced * by "orig_". In addition, '.'
   * are converted to '_'. For example, orig(this.x) goes to orig_this_x. Finally, Java Reserved
   * words are replaced with appropriate substitutes.
   *
   * @param varInfo the VarInfo for the variable whose base name is desired
   * @return the base name of the variable represented by varInfo
   */
  private static String getBaseName(VarInfo varInfo) {
    String name = varInfo.name();
    name = replaceReservedWords(name);
    if (name.length() > 5 && name.substring(0, 5).equals("orig(") && name.endsWith(")")) {
      name = name.substring(5, name.length() - 1);
      if (name.startsWith("this.")) {
        name = "this_" + name.substring(5);
      }
      name = "orig_" + name;
    } else {
      if (name.startsWith("this.")) {
        name = "this_" + name.substring(5);
      }
    }

    name = name.replace('.', '_');
    // originally array names in type infos end in "[..]"
    // but the replace above will change it to "[__]".   (markro)
    if (varInfo.type.isArray()) {
      if (name.endsWith("[__]")) {
        name = name.substring(0, name.length() - 4);
      }
    }
    return name;
  }

  /**
   * Returns an array of the base names of the variable in varInfos. The returned array is in the
   * same order as varInfos.
   */
  private static String[] getBaseNames(VarInfo[] varInfos) {
    String[] baseNames = new String[varInfos.length];
    for (int i = 0; i < varInfos.length; i++) {
      baseNames[i] = getBaseName(varInfos[i]);
    }
    return baseNames;
  }

  /**
   * Returns the name of the variable represented by varInfo as it would appear in the field
   * declaration of a java splitter file.
   *
   * @param varInfo the VarInfo representing the variable for which the field name is desired
   * @return the name of the variable represented by varInfo as it would appear in the field
   *     declaration of a java splitter file
   */
  private static String fieldName(VarInfo varInfo) throws ParseException {
    return compilableName(varInfo) + "_varinfo";
  }

  /**
   * Returns the name of the variable used to hold this varInfo in a java splitter file.
   *
   * @param varInfo the VarInfo for which the name of the variable is desired
   * @return the name of the variable used to hold this varInfo in a java splitter file
   */
  private static String varName(VarInfo varInfo) throws ParseException {
    return compilableName(varInfo) + "_vi";
  }

  /**
   * Returns the type of the variable represented by varInfo.
   *
   * @param varInfo the VarInfo for the variable whose type is desired
   * @return the type of the variable represented by varInfo
   */
  private static String getVarType(VarInfo varInfo) {
    if (varInfo.file_rep_type == ProglangType.HASHCODE) {
      return "int";
    } else if ((varInfo.type == ProglangType.CHAR_ARRAY)
        || (varInfo.type == ProglangType.BOOLEAN)
        || (varInfo.type == ProglangType.DOUBLE)
        || (varInfo.type == ProglangType.DOUBLE_ARRAY)) {
      return varInfo.type.format();
    } else {
      return varInfo.rep_type.format();
    }
  }

  /** VariableManager is a data structure for containing information about a variable. */
  private static class VariableManager {

    // /** VarInfo for the variable. */
    // private VarInfo varInfo;

    /** Name of variable as how it appears in the original file. */
    private String name;

    /** Name of variable in a compilable format. */
    private String compilableName;

    /** Name of the variable as it appears in the fields of the splitter file. */
    private String fieldName;

    /** Name of the variable that holds the varInfo in the splitter file. */
    private String varName;

    /** The type of the variable. */
    private String type;

    private VariableManager(VarInfo varInfo, String condition) throws ParseException {
      // this.varInfo = varInfo;
      name = varInfo.name();
      compilableName = compilableName(varInfo);
      fieldName = fieldName(varInfo);
      varName = varName(varInfo);
      type = makeIndexIfNeeded(getVarType(varInfo), compilableName, varInfo, condition);
    }

    /**
     * Returns the name of the variable.
     *
     * @return the name of the variable
     */
    private String getNormalName() {
      return name;
    }

    /**
     * Returns the compilable name of the variable.
     *
     * @return the compilable name of the variable
     */
    private String getCompilableName() {
      return compilableName;
    }

    /**
     * Returns the field name of the variable.
     *
     * @return the field name of the variable
     */
    private String getFieldName() {
      return fieldName;
    }

    /**
     * Returns the VarInfo name of the variable.
     *
     * @return the VarInfo name of the variable
     */
    private String getVarName() {
      return varName;
    }

    /**
     * Returns the type of the variable.
     *
     * @return the type of the variable
     */
    private String getType() {
      return type;
    }
  }

  /**
   * Creates a new instance of VariableManager.
   *
   * @param varInfos the varInfos for the variables to be managed
   * @param condition the condition in which the variables are used
   */
  private static VariableManager[] makeVariableManagerArray(VarInfo[] varInfos, String condition)
      throws ParseException {

    Global.debugSplit.fine("<<enter>> makeVariableManagerArray");

    List<VariableManager> variableManagerList = new ArrayList<>();
    List<String> classVars = findPossibleClassVariables(condition);
    for (VarInfo varInfo : varInfos) {
      try {
        String compilableName = compilableName(varInfo);
        Global.debugSplit.fine(
            "varInfo "
                + varInfo.name()
                + " isNeeded("
                + compilableName
                + ", "
                + classVars
                + ")="
                + isNeeded(compilableName, classVars));
        if (isNeeded(compilableName, classVars)) {
          variableManagerList.add(new VariableManager(varInfo, condition));
        }
      } catch (ParseException e) {
        System.out.println("ParseException: " + e.toString());
      }
    }
    Global.debugSplit.fine("<<exit>>  makeVariableManagerArray");

    return variableManagerList.toArray(new VariableManager[0]);
  }

  /** Returns true if the variable represented by varInfo is used in this splitting condition. */
  @Pure
  private static boolean isNeeded(String name, List<String> vars) {
    return vars.contains(name);
  }

  /**
   * requires: condition is a string representation of a conditional
   *
   * @return a list of all possible variable variable names in condition. This attempts not to
   *     return method names nor the base names of qualified names (why not the latter?). Arrays
   *     appear with "[]" at the end if their elements or accessed in the condition.
   */
  private static List<String> findPossibleClassVariables(String condition) throws ParseException {

    Global.debugSplit.fine("<<enter>> findPossibleClassVariables");

    NodeToken[] tokens = TokenExtractor.extractTokens(condition);
    Global.debugSplit.fine(
        "TokenExtractor.extractTokens(" + condition + ") ==> " + Arrays.toString(tokens));
    Set<String> variables = new LinkedHashSet<>();

    for (int i = 0; i < tokens.length; i++) {
      NodeToken token = tokens[i];
      if (token.kind == IDENTIFIER) {
        if ((i > 0) && (tokens[i - 1].kind == DOT)) {
          continue;
        }
        if ((i < tokens.length - 1) && (tokens[i + 1].kind == LPAREN)) {
          continue;
        }
        variables.add(token.tokenImage);
      }
    }

    Global.debugSplit.fine(
        "<<exit>>  findPossibleClassVariables(" + condition + ") => " + variables.toString());
    return new ArrayList<String>(variables);
  }

  /**
   * Returns type converted to index type if needed. A index type variable in java splitter file has
   * type "int" or "int[]" instead of "long" or "long[]". This is needed if the variable or the an
   * element of the variable is used as an index to an array. This method converts the type of the
   * variable to "int_index" or "index[]" if it is used as an index to an array or an element of it
   * is used as an index to an array.
   *
   * @param type the original type of the variable
   * @param name the name of the variable
   * @param varInfo the VarInfo of the variable
   * @param condition the condition in which the variable occurs
   * @return the type converted to index type if needed
   */
  private static String makeIndexIfNeeded(
      String type, String name, VarInfo varInfo, String condition) throws ParseException {
    if ((type.equals("int") || varInfo.type.isArray())
        && varInfo.file_rep_type != ProglangType.HASHCODE) {
      Deque<Boolean> inArrayIndex = new ArrayDeque<Boolean>();
      inArrayIndex.push(Boolean.FALSE);
      NodeToken[] tokens = TokenExtractor.extractTokens(condition);
      for (int i = 0; i < tokens.length; i++) {
        if (tokens[i].kind == LBRACKET) {
          inArrayIndex.push(Boolean.TRUE);
        } else if (tokens[i].kind == RBRACKET) {
          inArrayIndex.pop();
        } else if (tokens[i].kind == LPAREN) {
          inArrayIndex.push(Boolean.FALSE);
        } else if (tokens[i].kind == RPAREN) {
          inArrayIndex.pop();
        } else if (inArrayIndex.getFirst().booleanValue() && tokens[i].tokenImage.equals(name)) {
          if (type.equals("int") || type.equals("int_index")) {
            // Note the type can only equal "int_index" if the variable
            // was already visited by this if statement since it appears
            // more than once in the condition.
            type = "int_index";
          } else {
            type = "index[]";
          }
        }
      }
    }
    return type;
  }
}
