package daikon.mints;

import daikon.PptName;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * @author Huascar Sanchez
 */
class Source {

  private static final Map<String, String> ENV;

  static {
    final Map<String, String> lookUpTable = new HashMap<>();
    //Primitive types
    lookUpTable.put("byte", "java.lang.Byte");
    lookUpTable.put("short", "java.lang.Short");
    lookUpTable.put("int", "java.lang.Integer");
    lookUpTable.put("long", "java.lang.Long");
    lookUpTable.put("float", "java.lang.Float");
    lookUpTable.put("double", "java.lang.Double");
    lookUpTable.put("boolean", "java.lang.Boolean");
    lookUpTable.put("char", "java.lang.Character");
    lookUpTable.put("byte[]", "java.lang.Byte[]");
    lookUpTable.put("short[]", "java.lang.Short[]");
    lookUpTable.put("int[]", "java.lang.Integer[]");
    lookUpTable.put("long[]", "java.lang.Long[]");
    lookUpTable.put("float[]", "java.lang.Float[]");
    lookUpTable.put("double[]", "java.lang.Double[]");
    lookUpTable.put("boolean[]", "java.lang.Boolean[]");
    lookUpTable.put("char[]", "java.lang.Character[]");

    ENV = Collections.unmodifiableMap(lookUpTable);
  }


  private final String        fullClassName;
  private final boolean       isConstructor;
  private final List<String>  guessedClassLabels;
  private final String        paramsString;
  private final String        methodName;
  private final boolean       isEntry;

  private Source(String fullClassName, String methodName,
          String paramString, boolean isConstructor, boolean isEntry) {

    this.fullClassName      = Objects.requireNonNull(fullClassName);
    this.paramsString       = Objects.requireNonNull(paramString);
    this.isConstructor      = isConstructor;
    this.guessedClassLabels = Strings.generateLabel(methodName);
    this.methodName         = methodName;
    this.isEntry            = isEntry;
  }

  /**
   * Creates an object that holds info about the origin of these
   * likely invariants.
   *
   * @param pointName the program point name
   * @return a new source object.
   */
  static Source from(PptName pointName, boolean isEntry) {

    final String signature = pointName.getSignature();
    if (Objects.isNull(signature)) {
      return null;
    }

    final Matcher matcher = Pattern.compile("\\((.*?)\\)")
      .matcher(signature);

    List<String> params = null;
    String guessedName = null;

    final String NOTHING = "";
    if (matcher.find()) {

      params = Arrays.stream(matcher.group(1).split(",")).collect(Collectors.toList());
      guessedName = signature.replace(matcher.group(1), NOTHING);
      guessedName = guessedName.replace("()", "");
    }

    final boolean isConstructor = pointName.getShortClassName().equals(guessedName);

    final String paramsString = Strings.joinParameters(ENV, params);

    return new Source(
      pointName.getFullClassName(),
      guessedName,
      paramsString,
      isConstructor,
      isEntry
    );
  }

  String className() {
    return fullClassName;
  }

  boolean isConstructor() {
    return isConstructor;
  }

  List<String> labelList() {
    return guessedClassLabels;
  }

  String paramString() {
    return paramsString;
  }

  String methodName() {
    return methodName;
  }

  boolean isEntry() {
    return isEntry;
  }

  @Override public int hashCode() {
    return Objects.hash(fullClassName, methodName, paramsString);
  }

  @Override public boolean equals(Object obj) {

    if (!(obj instanceof Source)) return false;

    final Source other = (Source) obj;

    final boolean sameClassName   = other.fullClassName.equals(fullClassName);
    final boolean sameMethodName  = other.methodName.equals(methodName);
    final boolean sameParams      = other.paramsString.equals(paramsString);

    return sameClassName && sameMethodName && sameParams;
  }

  @Override public String toString() {

    final String classLabels = labelList().toString().replace("[", "").replace("]", "");

    return ("(" + className() + ",	" +
      (isConstructor()
        ? ("(new,	" + classLabels + ")")
        : (("(" + classLabels + ")")))
      +
      ((Objects.isNull(paramString()) || paramString().isEmpty())
        ? ""
        : (",	" + paramString())) +
      ")");
  }
}
