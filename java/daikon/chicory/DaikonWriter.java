package daikon.chicory;

import daikon.Chicory;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.StringJoiner;
import org.checkerframework.checker.signature.qual.BinaryName;

/** DaikonWriter is the parent class of DeclWriter and DTraceWriter. */
public abstract class DaikonWriter {
  /** Controls whether modifiers and the return type are included in the decl output. */
  protected static final boolean no_modifiers_ppt = true;

  /** Platform-dependent line separator. Should be "\n" on Unix. */
  public static final String lineSep = System.lineSeparator();

  /** Create a new DaikonWriter. */
  protected DaikonWriter() {
    // This constructor is intentially empty.
  }

  /**
   * Determines if this field warrants an [ = val ] entry in decls file.
   *
   * @param field requires field != null
   * @return true iff field warrants an [ = val ] entry in the decls files
   */
  protected static boolean isStaticConstField(Field field) {
    Class<?> type = field.getType();
    int mod = field.getModifiers();

    if (DaikonVariableInfo.dkconfig_constant_infer) {
      return Modifier.isFinal(mod) && Modifier.isStatic(mod);
    } else {
      return Modifier.isFinal(mod) && Modifier.isStatic(mod) && type.isPrimitive();
    }
  }

  /**
   * Given a method, returns the method entry program point name for Daikon.
   *
   * @param method non-null method
   * @return the decorated method entry name for Daikon
   */
  public static String methodEntryName(Member method) {
    return methodPptName(method, daikon.FileIO.enter_suffix);
  }

  /**
   * Given a method, returns the method entry program point name for Daikon. Used when reflection
   * information is not available.
   *
   * @param fullClassName packageName.className
   * @param types string representation of the declared types of the parameters
   * @param name the method with modifiers and parameters, such as "public static void
   *     DataStructures.StackArTester.doNew(int size)"
   * @param short_name just the method's name ("{@code <init>}" for constructors)
   * @return the decorated method entry name for Daikon
   */
  public static String methodEntryName(
      String fullClassName, String[] types, String name, String short_name) {
    return methodPptName(fullClassName, types, name, short_name, daikon.FileIO.enter_suffix);
  }

  /**
   * Given a method, returns the method exit program point name for Daikon.
   *
   * @param method non-null method
   * @param lineNum the line number of a return statement in the method
   * @return the decorated method exit name for Daikon
   */
  public static String methodExitName(Member method, int lineNum) {
    return methodPptName(method, daikon.FileIO.exit_suffix + lineNum);
  }

  /**
   * Given a method, returns the method exit program point name for Daikon. Used when reflection
   * information is not available.
   *
   * @param fullClassName packageName.className
   * @param types string representation of the declared types of the parameters
   * @param name the method name with modifiers and parameters
   * @param short_name just the method's name ("{@code <init>}" for constructors)
   * @param lineNum the line number of a return statement in the method
   * @return the decorated method exit name for Daikon
   */
  public static String methodExitName(
      String fullClassName, String[] types, String name, String short_name, int lineNum) {
    return methodPptName(
        fullClassName, types, name, short_name, daikon.FileIO.exit_suffix + lineNum);
  }

  /**
   * Constructs the program point name (which includes the point string at the end)
   *
   * @param fullClassName packageName.className
   * @param types string representation of the declared types of the parameters. For example:
   *     {"int", "java.lang.Object", "float"}.
   * @param name the method with modifiers and parameters, such as "public static void
   *     DataStructures.StackArTester.doNew(int size)"
   * @param short_name just the method's name ("{@code <init>}" for constructors)
   * @param point program point type/suffix such as "EXIT" or "ENTER"
   * @return same thing as {@link #methodPptName(Member, String)}
   */
  private static String methodPptName(
      String fullClassName, String[] types, String name, String short_name, String point) {

    // UNDONE: name is no longer used

    // System.out.printf("fullclass: %s !!! name: %s !!! short_name: %s %n",
    //                  fullClassName, name, short_name);

    boolean isConstructor = short_name.equals("<init>") || short_name.equals("");

    if (isConstructor) {
      // replace <init>'s with the actual class name
      // so "public void <init>" becomes "public void StackAr" for example
      short_name = fullClassName.substring(fullClassName.lastIndexOf('.') + 1);
      name = name.replace("<init>", short_name);
    }

    // build up the string to go inside the parens
    StringJoiner paramTypes = new StringJoiner(",", "(", ")");
    for (String type : types) {
      paramTypes.add(type);
    }
    String pptname = fullClassName + "." + short_name + paramTypes + ":::" + point;

    if (Chicory.debug_ppt_names) {
      System.out.printf("methodPptName final ppt name = '%s'%n", pptname);
    }

    // Throwable t = new Throwable("debug");
    // t.fillInStackTrace();
    // t.printStackTrace();
    // System.out.printf("ppt name = %s%n", pptname);

    return pptname;
  }

  /**
   * Constructs the program point name. It includes {@code point} at the end, after ":::".
   *
   * @param member reflection object for the method/constructor
   * @param point usually "ENTER" or "EXIT"
   * @return the program point name
   */
  private static String methodPptName(Member member, String point) {
    String fullname;
    Class<?>[] params;
    Class<?> declaring_class = member.getDeclaringClass();
    if (member instanceof Method) {
      Method method = (Method) member;
      fullname = declaring_class.getName() + "." + method.getName();
      params = method.getParameterTypes();
    } else {
      Constructor<?> constructor = (Constructor<?>) member;
      fullname = declaring_class.getName() + "." + declaring_class.getSimpleName();
      params = constructor.getParameterTypes();
    }
    String param_str = "";
    for (Class<?> param : params) {
      if (param_str.length() > 0) {
        param_str += ", ";
      }
      if (param.isArray()) {
        param_str += Runtime.classGetNameToBinaryName(param.getName());
      } else {
        param_str += param.getName();
      }
    }
    return String.format("%s(%s):::%s", fullname, param_str, point);
  }

  /** Determines if the given method should be instrumented. */
  protected boolean shouldInstrumentMethod(Member method) {
    if (method == null) { // <clinit>
      return Chicory.instrument_clinit;
    }
    int modifiers = method.getModifiers();
    return !(Modifier.isAbstract(modifiers) || Modifier.isNative(modifiers));
  }

  /**
   * Returns the class name of the specified class as a binary name (i.e., as the class would have
   * been declared in Java source code, except with '$' instead of '.' separating outer and inner
   * classes).
   */
  public static @BinaryName String stdClassName(Class<?> type) {
    return Runtime.classGetNameToBinaryName(type.getName());
  }

  /** Escapes blanks and backslashes in names written to the decl/dtrace files. */
  public String escape(String str) {

    // If there is nothing to escape, return the original string
    if ((str.indexOf('\\') == -1) && (str.indexOf(' ') == -1)) {
      return str;
    }

    str = str.replace("\\", "\\\\");
    str = str.replace(" ", "\\_");
    return str;
  }
}
