package daikon.tools.runtimechecker;

import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.BufferedWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import org.checkerframework.checker.signature.qual.BinaryName;

/**
 * This program runs a given program (which is assumed to contain some classes that have been
 * instrumented with the runtimechecker instrumentation tool) and prints a list of all the invariant
 * violations that occur during execution.
 */
class WriteViolationFile {

  public static void usage() {
    System.out.println("Usage:  java WriteViolationFile CLASS ARGS");
    System.out.println("  CLASS and ARGS are just as they would be when being run directly,");
    System.out.println(
        "  except that CLASS is written as a binary name, not a fully-qualified name");
    System.out.println("Output is written to file \"violations.txt\" in the current directory.");
  }

  @SuppressWarnings("Finally")
  public static void main(String[] args) {
    if (args.length == 0) {
      System.out.println("Error: no class specified");
      usage();
      System.exit(1);
    }
    @SuppressWarnings("signature") // will be checked immediately below, and exception is caught
    @BinaryName String class_name = args[0];
    String[] main_args = new String[args.length - 1];
    for (int i = 0; i < main_args.length; i++) {
      main_args[i] = args[i + 1];
    }
    Class<?> cls;
    try {
      cls = Class.forName(class_name);
    } catch (Exception e) {
      e.printStackTrace();
      throw new Error("Cannot find class " + class_name);
    }
    Method main_method;
    try {
      main_method = cls.getMethod("main", new Class<?>[] {String[].class});
    } catch (Exception e) {
      throw new Error("Cannot find main method in class " + class_name);
    }
    int mods = main_method.getModifiers();
    if (!Modifier.isPublic(mods)) {
      throw new Error("main method is not public in class " + class_name);
    }
    if (!Modifier.isStatic(mods)) {
      throw new Error("main method is not static in class " + class_name);
    }

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
      try (BufferedWriter writer = Files.newBufferedWriter(Paths.get("violations.txt"), UTF_8)) {
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

        if (vios.size() == 0) {
          writer.write("none." + daikon.Global.lineSep);
        } else {
          writer.write(daikon.Global.lineSep);
          for (Violation v : vios) {
            writer.write(v.toStringWithMethod());
            writer.newLine();
          }
        }
      } catch (IOException e) {
        throw new Error("Problem while writing file violations.txt", e);
      }
    }
  }
}
