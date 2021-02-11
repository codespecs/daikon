package daikon.dcomp;

import static java.nio.charset.StandardCharsets.UTF_8;

import daikon.DynComp;
import daikon.chicory.DaikonVariableInfo;
import daikon.chicory.DeclWriter;
import daikon.plumelib.bcelutil.BcelUtil;
import daikon.plumelib.options.Option;
import daikon.plumelib.options.Options;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.lang.instrument.ClassFileTransformer;
import java.lang.instrument.Instrumentation;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import org.apache.bcel.*;
import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.*;

/**
 * This class is the entry point for the DynComp instrumentation agent. It is the only code in
 * dcomp_premain.jar.
 */
public class Premain {

  // These command-line options cannot be accessed from DynComp.  These are internal debugging
  // options that may be used when Premain is invoked directly from the command line.

  /** Turn on basic DCInstrument debugging options. */
  @Option("Turn on basic DCInstrument debugging options")
  public static boolean debug_dcinstrument = false;

  /** Turn on basic DCRuntime debugging options. */
  @Option("Turn on basic DCRuntime debugging options")
  public static boolean debug_dcruntime = false;

  /** Turn on most DCRuntime debugging options. */
  @Option("Turn on most DCRuntime debugging options")
  public static boolean debug_dcruntime_all = false;

  /** Set of pre-instrumented JDK classes. */
  protected static Set<String> pre_instrumented = new HashSet<>();

  /** Set of packages known to cause problems when instrumented. */
  protected static Set<String> problem_packages =
      new HashSet<>(
          Arrays.asList(
              // Packages to support reflection and Lambda expressions cause instrumentation
              // problems and probably don't affect user program comparability values.
              // JDK8 and JDK11
              "java.lang.invoke",
              "java.lang.reflect",
              "sun.reflect.annotation",
              "sun.reflect.misc",
              // JDK8
              "sun.reflect",
              // JDK11
              "jdk.internal.reflect"));

  /** Set of classes known to cause problems when instrumented. */
  protected static Set<String> problem_classes =
      new HashSet<>(
          Arrays.asList(
              // <clinit> gets a JNI error during initialization.
              "java.lang.StackTraceElement$HashedModules"));

  /** Set of methods known to cause problems when instrumented. */
  protected static Set<String> problem_methods =
      new HashSet<>(
          Arrays.asList(
              // (none at present)
              ));

  /**
   * One of the last phases for DynComp is to write out the comparability values after the user
   * program completes execution. One of the steps is to assign values to the arguments of methods
   * that have not been executed. We use reflection to get type information about these arguments,
   * which causes the method to be loaded; which causes the main part of DynComp to try to
   * instrument the method. As the user program has completed execution, doing instrumentation at
   * this point can lead to problems. The correct fix for this problem is to use BCEL to get the
   * type information instead of reflection, thus avoiding loading the method into the JVM. This
   * will be a large change, so a temporary fix is to indicate if the program is in shutdown mode
   * and not instrument any methods when this flag is true. TODO: Couldn't we just call
   * removeTransformer at the start of shutdown?
   */
  protected static boolean in_shutdown = false;

  /** Flag to indicate if we are retransforming a previously loaded class. */
  protected static boolean retransform_preloads;

  /** Keep track of classes that have already been retransformed. */
  private static Set<Class<?>> previously_processed_classes = new HashSet<>();

  // For debugging
  // protected static Instrumentation instr;

  /**
   * This method is the entry point of the java agent. Its main purpose is to set up the transformer
   * so that when classes from the target app are loaded, they are first transformed in order to add
   * comparability instrumentation.
   *
   * <p>If this code is running on Java 9+ and jdk_instrumented is true it also retransforms any JDK
   * methods that were loaded prior to premain getting control.
   *
   * @param agentArgs string containing the arguments passed to this agent
   * @param inst instrumentation instance to be used to transform classes
   * @throws IOException if jdk_classes.txt cannot be read or if the correct version of BCEL cannot
   *     be found or loaded
   */
  public static void premain(String agentArgs, Instrumentation inst) throws IOException {
    // For debugging
    // this.instr = inst;

    // Because DynComp started Premain in a separate process, we must rescan
    // the options to set up the DynComp static variables.
    Options options = new Options(DynComp.synopsis, DynComp.class, Premain.class);
    String[] args = options.parse(true, agentArgs.split("  *"));
    if (args.length > 0) {
      System.err.printf("Unexpected arguments %s%n", Arrays.toString(args));
      options.printUsage();
      System.exit(-1);
    }
    if (DynComp.rt_file != null && DynComp.rt_file.getName().equalsIgnoreCase("NONE")) {
      DynComp.no_jdk = true;
      DynComp.rt_file = null;
    }

    // Note that the following references to static fields have an important
    // side effect: They cause the corresponding class to be loaded. This helps
    // Instrument.transform() avoid ClassCircularityErrors during initialization.
    DaikonVariableInfo.std_visibility = DynComp.std_visibility;
    DCRuntime.depth = DynComp.nesting_depth;
    DCInstrument.jdk_instrumented = !DynComp.no_jdk;
    @SuppressWarnings("UnusedVariable") // loads the BcelUtil class; otherwise, Premain gives errors
    int junk = BcelUtil.javaVersion;

    // Another 'trick' to force needed classes to be loaded prior to retransformation.
    String buffer =
        String.format("In dcomp premain, agentargs ='%s', Instrumentation = '%s'", agentArgs, inst);
    if (DynComp.verbose) {
      System.out.println(buffer);
      System.out.printf("Options settings: %n%s%n", options.settings());
    }

    // Read the list of pre-instrumented classes.
    if (DCInstrument.jdk_instrumented) {
      // location is: daikon/java/dcomp-rt/java/lang/jdk_classes.txt .
      InputStream strm = Object.class.getResourceAsStream("jdk_classes.txt");
      if (strm == null) {
        System.err.println(
            "Can't find jdk_classes.txt; see Daikon manual, section \"Instrumenting the JDK with DynComp\"");
        System.exit(1);
      }
      BufferedReader reader = new BufferedReader(new InputStreamReader(strm, UTF_8));

      while (true) {
        String line = reader.readLine();
        if (line == null) {
          break;
        }
        // System.out.printf("adding '%s'%n", line);
        pre_instrumented.add(line);
      }
    }

    // Setup the shutdown hook
    Thread shutdown_thread = new ShutdownThread();
    java.lang.Runtime.getRuntime().addShutdownHook(shutdown_thread);

    // Setup the transformer
    ClassFileTransformer transformer;
    // use a special classloader to ensure correct version of BCEL is used
    ClassLoader loader = new daikon.chicory.ChicoryPremain.ChicoryLoader();
    try {
      transformer =
          (ClassFileTransformer)
              loader.loadClass("daikon.dcomp.Instrument").getDeclaredConstructor().newInstance();
    } catch (Exception e) {
      throw new RuntimeException("Unexpected error loading Instrument", e);
    }
    if (DynComp.verbose) {
      // If DCInstrument.jdk_instrumented is true then the printf below will output
      // 'null' to indicate we are using the bootstrap loader.
      System.out.printf(
          "Classloader of transformer = %s%n", transformer.getClass().getClassLoader());
    }

    // Check that we got a newer version of BCEL that includes JDK 11 support. At present,
    // this is only the PLSE 6.4.1.1 release version.  We can verify this version by the
    // presence of the method FieldGenOrMethodGen.removeAnnotationEntries().
    try {
      Class<?> c = loader.loadClass("org.apache.bcel.generic.FieldGenOrMethodGen");
      c.getMethod("removeAnnotationEntries", (Class<?>[]) null);
    } catch (Exception e) {
      System.err.printf("%nBCEL jar found is not the version included with the Daikon release.%n");
      System.exit(1);
    }

    // now turn on instrumentation
    if (DynComp.verbose) {
      System.out.println("call addTransformer");
    }
    inst.addTransformer(transformer, true);

    // For Java 9+ we only partially instrument the JDK as part of building DynComp.
    // We complete the instrumentation when a JDK class is loaded during program execution.
    // However, there is Catch-22: "All future class definitions will be seen by the transformer,
    // except definitions of classes upon which any registered transformer is dependent"
    // (from the documentation for Instrumentation.addTransformer()).
    // Thus a JDK class used by DynComp will not be seen by our transformer.  To get around this
    // we get the list of all the classes already loaded and call retransformClasses on them.
    // Unfortunately, this process may need to be repeated multiple times as each time we call
    // retransform on a class it may require the use of a previously unexecuted part of DynComp
    // which may, in turn, cause more JDK classes to be loaded without our knowledge.
    //
    // Possibility:
    // There may be a way to use the JDeps tool to get a list of all the dependencies as part of
    // the DynComp build process and then pass this list to Premain for processing.
    //
    // Future work:
    // We should revisit the problems associated with pre-instrumenting the JDK via BuildJDK.
    // The current system has a noticeable performance hit at startup in addition to the
    // retransformation complications noted above.
    //
    if (BcelUtil.javaVersion > 8 && DCInstrument.jdk_instrumented) {

      retransform_preloads = true;
      Class<?>[] classes_to_retransform = get_retransform_list(inst);

      while (classes_to_retransform.length > 0) {
        if (DynComp.verbose) {
          System.out.println("call retransformClasses");
        }
        try {
          inst.retransformClasses(classes_to_retransform);
        } catch (Exception e) {
          System.err.println("Unable to retransformClasses.");
          System.err.println(e);
        }
        classes_to_retransform = get_retransform_list(inst);
      }
    }
    retransform_preloads = false;

    // Iterator<Class<?>> value = previously_processed_classes.iterator();
    // while (value.hasNext()) {
    //   System.out.println(value.next());
    // }

    // Initialize the static tag array
    if (DynComp.verbose) {
      System.out.println("call DCRuntime.init");
    }
    DCRuntime.init();

    if (DynComp.verbose) {
      System.out.println("exit premain");
    }
  }

  /**
   * Get an array of already loaded classes that need to be retransformed.
   *
   * @param inst instrumentation instance to be used to transform classes
   * @return an array containing the classes to be retransformed
   */
  private static Class<?>[] get_retransform_list(Instrumentation inst) {
    if (DynComp.verbose) {
      System.out.println("get retransformation list");
    }

    ArrayList<Class<?>> class_list = new ArrayList<>();

    // Get the set of already loaded classes.
    Class<?>[] loaded_classes = inst.getAllLoadedClasses();

    for (Class<?> loaded_class : loaded_classes) {
      // System.out.println(loaded_class + ": " + loaded_class.getClassLoader());
      // Skip previously processed classes.
      if (previously_processed_classes.contains(loaded_class)) continue;
      // Skip Daikon classes.
      if (loaded_class.getName().startsWith("daikon.")) continue;
      // Skip BCEL classes.
      if (loaded_class.getName().startsWith("org.apache.bcel.")) continue;
      // Object cannot be instrumented due to its fundimental nature.
      if (loaded_class.getName().equals("java.lang.Object")) continue;
      if (inst.isModifiableClass(loaded_class)) {
        // System.out.println(loaded_class);
        class_list.add(loaded_class);
      }
    }
    previously_processed_classes = new HashSet<>(Arrays.asList(loaded_classes));
    return class_list.toArray(new Class<?>[class_list.size()]);
  }

  /** Shutdown thread that writes out the comparability results. */
  public static class ShutdownThread extends Thread {

    @Override
    public void run() {

      if (DynComp.verbose) {
        System.out.println("in shutdown");
      }
      in_shutdown = true;

      // for debugging
      // Class<?>[] loaded_classes = instr.getAllLoadedClasses();
      // for (Class<?> loaded_class : loaded_classes) {
      //   System.out.println(loaded_class + ": " + loaded_class.getClassLoader());
      // }

      // If requested, write the comparability data to a file
      if (DynComp.comparability_file != null) {
        if (DynComp.verbose) {
          System.out.println("Writing comparability sets to " + DynComp.comparability_file);
        }
        PrintWriter compare_out = open(DynComp.comparability_file);
        long startTime = System.nanoTime();
        DCRuntime.printAllComparable(compare_out);
        compare_out.close();
        if (DynComp.verbose) {
          long duration = System.nanoTime() - startTime;
          System.out.printf(
              "Comparability sets written in %ds%n", TimeUnit.NANOSECONDS.toSeconds(duration));
        }
      }

      if (DynComp.trace_file != null) {
        if (DynComp.verbose) {
          System.out.println("Writing traced comparability sets to " + DynComp.trace_file);
        }
        PrintWriter trace_out = open(DynComp.trace_file);
        long startTime = System.nanoTime();
        DCRuntime.traceAllComparable(trace_out);
        trace_out.close();
        if (DynComp.verbose) {
          long duration = System.nanoTime() - startTime;
          System.out.printf(
              "Traced comparability sets written in %ds%n",
              TimeUnit.NANOSECONDS.toSeconds(duration));
        }
      } else {
        // Writing comparability sets to standard output?
      }

      if (DynComp.verbose) {
        DCRuntime.decl_stats();
      }

      // Write the decl file out
      @SuppressWarnings(
          "nullness:argument.type.incompatible") // DynComp guarantees decl_file is non null
      File decl_file = new File(DynComp.output_dir, DynComp.decl_file);
      if (DynComp.verbose) System.out.println("Writing decl file to " + decl_file);
      PrintWriter decl_fp = open(decl_file);
      // Create DeclWriter so can share output code in Chicory.
      DCRuntime.declWriter = new DeclWriter(decl_fp);
      DCRuntime.declWriter.debug = DynComp.debug_decl_print;
      // Used for calling ComparabilityProvider.getComparability.
      DCRuntime.comparabilityProvider = new DCRuntime();

      long startTime = System.nanoTime();
      DCRuntime.printDeclFile(decl_fp);
      decl_fp.close();
      if (DynComp.verbose) {
        long duration = System.nanoTime() - startTime;
        System.out.printf("Decl file written in %ds%n", TimeUnit.NANOSECONDS.toSeconds(duration));
        System.out.printf("comp_list = %,d%n", DCRuntime.comp_list_ms);
        System.out.printf("ppt name  = %,d%n", DCRuntime.ppt_name_ms);
        System.out.printf("decl vars = %,d%n", DCRuntime.decl_vars_ms);
        System.out.printf("total     = %,d%n", DCRuntime.total_ms);
      }
      if (DynComp.verbose) System.out.println("DynComp complete");
    }
  }

  /**
   * Helper method to create a PrintWriter from a File.
   *
   * @param filename the File to be opened
   * @return a new PrintWriter from filename
   */
  public static PrintWriter open(File filename) {
    File canonicalFile;
    try {
      canonicalFile = filename.getCanonicalFile();
    } catch (IOException e) {
      throw new Error(
          "Can't get canonical file for " + filename + " in " + System.getProperty("user.dir"));
    }

    // I don't know why, but without this, the call to newBufferedWriter fails in some contexts.
    try {
      canonicalFile.createNewFile();
    } catch (IOException e) {
      throw new Error("createNewFile failed for " + canonicalFile, e);
    }

    try {
      return new PrintWriter(Files.newBufferedWriter(canonicalFile.toPath(), UTF_8));
    } catch (Exception e) {
      throw new Error("Can't open " + filename + " = " + canonicalFile, e);
    }
  }
}
