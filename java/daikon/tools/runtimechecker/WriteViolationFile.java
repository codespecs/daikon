package daikon.tools.runtimechecker;

import static java.nio.charset.StandardCharsets.UTF_8;

import daikon.Daikon;
import daikon.DaikonGetopt;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import org.checkerframework.checker.signature.qual.BinaryName;
import org.plumelib.util.StringsPlume;

/**
 * This program runs a given program (which is assumed to contain some classes that have been
 * instrumented with the runtimechecker instrumentation tool) and prints a list of all the invariant
 * violations that occur during execution.
 */
class WriteViolationFile {

  /** Do not instantiate. */
  private WriteViolationFile() {
    throw new UnsupportedOperationException("Do not instantiate");
  }

  /** The usage message for this program. */
  private static final String usageMessage =
      StringsPlume.joinLines(
          "Usage:  java WriteViolationFile CLASS ARGS",
          "  CLASS and ARGS are just as they would be when being run directly,",
          "  except that CLASS is written as a binary name, not a fully-qualified name",
          "Output is written to file \"violations.txt\" in the current directory.");

  /**
   * Returns the main method of the given class.
   *
   * @param className the binary name of a class
   * @return the main method of the class
   * @throws Daikon.UserError if the class does not exist or has no public static main method
   */
  private static Method getMainMethod(String className) {
    Class<?> cls;
    try {
      @SuppressWarnings("signature") // checked by Class.forName, which throws if it is malformed
      @BinaryName String binaryName = className;
      cls = Class.forName(binaryName);
    } catch (ClassNotFoundException e) {
      throw new Daikon.UserError(e, "Cannot find class " + className);
    }
    Method mainMethod;
    try {
      mainMethod = cls.getMethod("main", new Class<?>[] {String[].class});
    } catch (NoSuchMethodException e) {
      throw new Daikon.UserError(e, "Cannot find public main method in class " + className);
    }
    if (!Modifier.isStatic(mainMethod.getModifiers())) {
      throw new Daikon.UserError("main method is not static in class " + className);
    }
    return mainMethod;
  }

  public static void main(String[] args) {
    try {
      mainHelper(args);
    } catch (Daikon.DaikonTerminationException e) {
      Daikon.handleDaikonTerminationException(e);
    }
  }

  /**
   * This does the work of {@link #main(String[])}, but it never calls System.exit.
   *
   * @param args the command-line arguments
   * @throws Daikon.DaikonTerminationException if the arguments are malformed or request help
   */
  @SuppressWarnings("Finally")
  private static void mainHelper(String[] args) {
    String[] classAndArgs = DaikonGetopt.argsAfterLeadingOptions(args, usageMessage);
    if (classAndArgs.length == 0) {
      throw new Daikon.UserError("No class specified" + Daikon.lineSep + usageMessage);
    }
    Method main_method = getMainMethod(classAndArgs[0]);
    String[] main_args = Arrays.copyOfRange(classAndArgs, 1, classAndArgs.length);

    // This will produce no output if the program under test calls
    // System.exit(1).  Let's hope it doesn't (and fix this later).
    try {
      // Permit access to method in default-access classes inside a package.
      main_method.setAccessible(true);

      @SuppressWarnings({
        "nullness", // static method, so null first arg is OK: main()
        "UnusedVariable" // exists to give a place to write @SuppressWarnings("nullness")
      })
      Object dummy = main_method.invoke(null, new Object[] {main_args});
    } catch (IllegalAccessException e) {
      // This can't happen
      throw new Error("Problem while invoking main", e);
    } catch (InvocationTargetException e) {
      // This can't happen
      throw new Error("Problem while invoking main", e);
    } finally {

      List<Violation> vios = daikon.tools.runtimechecker.Runtime.getViolations();
      // Don't use this; I want output in order.
      // String vstring = daikon.tools.runtimechecker.Runtime.toNiceString("", vios, 0);

      // On-the-fly implementation should flush after each violation is
      // written to disk.
      Path violationsPath = Paths.get("violations.txt");
      try (BufferedWriter writer = Files.newBufferedWriter(violationsPath, UTF_8)) {
        writer.write(
            "# Times an invariant was evaluated ----------- "
                + Long.toString(Runtime.numEvaluations)
                + daikon.Global.lineSep
                + "# Entry program points traversed ------------- "
                + Long.toString(Runtime.numPptEntries)
                + daikon.Global.lineSep
                + "# Normal-exit program points traversed ------- "
                + Long.toString(Runtime.numNormalPptExits)
                + daikon.Global.lineSep
                + "# Exceptional-exit program points traversed -- "
                + Long.toString(Runtime.numExceptionalPptExits)
                + daikon.Global.lineSep
                + "# Total exit program points traversed -------- "
                + Long.toString(Runtime.numNormalPptExits + Runtime.numExceptionalPptExits)
                + daikon.Global.lineSep
                + daikon.Global.lineSep
                + "# Violations: ");

        if (vios.isEmpty()) {
          writer.write("none." + daikon.Global.lineSep);
        } else {
          writer.write(daikon.Global.lineSep);
          for (Violation v : vios) {
            writer.write(v.toStringWithMethod());
            writer.newLine();
          }
        }
      } catch (IOException e) {
        throw new UncheckedIOException("Problem while writing file " + violationsPath, e);
      }
    }
  }
}
