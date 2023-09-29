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
import java.io.UncheckedIOException;
import java.lang.instrument.ClassDefinition;
import java.lang.instrument.ClassFileTransformer;
import java.lang.instrument.Instrumentation;
import java.net.URL;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;

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

  /** If true, print information about the classes being transformed. */
  public static boolean verbose = false;

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
              // (none at present)
              ));

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

  // For debugging
  // protected static Instrumentation instr;

  /**
   * This method is the entry point of the Java agent. Its main purpose is to set up the transformer
   * so that when classes from the target app are loaded, they are first transformed in order to add
   * comparability instrumentation.
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
    String[] target_args = options.parse(true, agentArgs.trim().split("  *"));
    if (target_args.length > 0) {
      System.err.printf("Unexpected Premain arguments %s%n", Arrays.toString(target_args));
      System.out.println();
      options.printUsage();
      System.exit(1);
    }

    // Turn on dumping of instrumented classes if debug was selected
    if (DynComp.debug) {
      DynComp.dump = true;
    }

    verbose = DynComp.verbose || DynComp.debug;

    if (DynComp.rt_file != null && DynComp.rt_file.getName().equalsIgnoreCase("NONE")) {
      DynComp.no_jdk = true;
      DynComp.rt_file = null;
    }

    // Note that the following references to static fields have an important
    // side effect: They cause the corresponding class to be loaded. This helps
    // Instrument.transform() avoid ClassCircularityErrors during initialization.
    DaikonVariableInfo.std_visibility = DynComp.std_visibility;
    DCRuntime.depth = DynComp.nesting_depth;
    // daikon.chicory.Instrument#shouldIgnore is shared by Chicory and DynComp.
    // It uses the Chicory Runtime copy of the patterns.
    daikon.chicory.Runtime.ppt_omit_pattern = DynComp.ppt_omit_pattern;
    daikon.chicory.Runtime.ppt_select_pattern = DynComp.ppt_select_pattern;

    DCInstrument.jdk_instrumented = !DynComp.no_jdk;
    @SuppressWarnings("UnusedVariable") // loads the BcelUtil class; otherwise, Premain gives errors
    int junk = BcelUtil.javaVersion;

    // Another 'trick' to force needed classes to be loaded prior to retransformation.
    String buffer =
        String.format(
            "In DynComp premain, agentargs ='%s', Instrumentation = '%s'", agentArgs, inst);
    if (verbose) {
      System.out.println(buffer);
      System.out.printf("Options settings: %n%s%n", options.settings());
    }

    // Read the list of pre-instrumented classes.
    if (DCInstrument.jdk_instrumented) {
      // location is: daikon/java/dcomp-rt/java/lang/jdk_classes.txt .
      try (InputStream strm = Object.class.getResourceAsStream("jdk_classes.txt")) {
        if (strm == null) {
          System.err.println(
              "Can't find jdk_classes.txt;"
                  + " see Daikon manual, section \"Instrumenting the JDK with DynComp\"");
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
    if (verbose) {
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
    if (verbose) {
      System.out.println("call addTransformer");
    }
    inst.addTransformer(transformer, true);

    // See the "General Java Runtime instrumentation strategy" comments in DCInstrument.java
    // for an explaination of how we deal with instrumenting the JDK 11 runtime.
    //
    // At this point in DynComp start up, we use java.lang.instrument.redefineClasses to replace the
    // dummy java.lang.DCRuntime with a version where each method calls the corresponding method in
    // daikon.dcomp.DCRuntime. The Java runtime does not enforce the security check in this case.
    //
    if (BcelUtil.javaVersion > 8 && DCInstrument.jdk_instrumented) {

      // Buffer for input of our replacement java.lang.DCRuntime.
      // The size of the current version is 6326 bytes and we do not
      // anticipate any significant changes.
      byte[] repClass = new byte[9999];
      String classname = "daikon/dcomp-transfer/DCRuntime.class";
      URL class_url = ClassLoader.getSystemResource(classname);
      if (class_url != null) {
        try (InputStream inputStream = class_url.openStream()) {
          if (inputStream != null) {
            int size = inputStream.read(repClass, 0, repClass.length);
            byte[] truncated = new byte[size];
            System.arraycopy(repClass, 0, truncated, 0, size);
            ClassDefinition cd =
                new ClassDefinition(Class.forName("java.lang.DCRuntime"), truncated);
            inst.redefineClasses(cd);
          } else {
            throw new Error("openStream failed for " + class_url);
          }
        } catch (Throwable t) {
          throw new Error("Unexpected error reading " + class_url, t);
        }
      } else {
        throw new Error("Could not locate " + classname);
      }
    }

    // Initialize the static tag array
    if (verbose) {
      System.out.println("call DCRuntime.init");
    }
    DCRuntime.init();

    if (verbose) {
      System.out.println("exit premain");
    }
  }

  /** Shutdown thread that writes out the comparability results. */
  public static class ShutdownThread extends Thread {

    @Override
    public void run() {

      if (Premain.verbose) {
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
        if (Premain.verbose) {
          System.out.println("Writing comparability sets to " + DynComp.comparability_file);
        }
        PrintWriter compare_out = open(DynComp.comparability_file);
        long startTime = System.nanoTime();
        DCRuntime.printAllComparable(compare_out);
        compare_out.close();
        if (Premain.verbose) {
          long duration = System.nanoTime() - startTime;
          System.out.printf(
              "Comparability sets written in %ds%n", TimeUnit.NANOSECONDS.toSeconds(duration));
        }
      }

      if (DynComp.trace_file != null) {
        if (Premain.verbose) {
          System.out.println("Writing traced comparability sets to " + DynComp.trace_file);
        }
        PrintWriter trace_out = open(DynComp.trace_file);
        long startTime = System.nanoTime();
        DCRuntime.traceAllComparable(trace_out);
        trace_out.close();
        if (Premain.verbose) {
          long duration = System.nanoTime() - startTime;
          System.out.printf(
              "Traced comparability sets written in %ds%n",
              TimeUnit.NANOSECONDS.toSeconds(duration));
        }
      } else {
        // Writing comparability sets to standard output?
      }

      if (Premain.verbose) {
        DCRuntime.decl_stats();
      }

      // Write the decl file out
      @SuppressWarnings("nullness:argument") // DynComp guarantees decl_file is non null
      File decl_file = new File(DynComp.output_dir, DynComp.decl_file);
      if (Premain.verbose) {
        System.out.println("Writing decl file to " + decl_file);
      }
      PrintWriter decl_fp = open(decl_file);
      // Create DeclWriter so can share output code in Chicory.
      DCRuntime.declWriter = new DeclWriter(decl_fp);
      DCRuntime.declWriter.debug = DynComp.debug_decl_print;
      // Used for calling ComparabilityProvider.getComparability.
      DCRuntime.comparabilityProvider = new DCRuntime();

      long startTime = System.nanoTime();
      DCRuntime.printDeclFile(decl_fp);
      decl_fp.close();
      if (Premain.verbose) {
        long duration = System.nanoTime() - startTime;
        System.out.printf("Decl file written in %ds%n", TimeUnit.NANOSECONDS.toSeconds(duration));
        System.out.printf("comp_list = %,d%n", DCRuntime.comp_list_ms);
        System.out.printf("ppt name  = %,d%n", DCRuntime.ppt_name_ms);
        System.out.printf("decl vars = %,d%n", DCRuntime.decl_vars_ms);
        System.out.printf("total     = %,d%n", DCRuntime.total_ms);
      }
      if (Premain.verbose) {
        System.out.println("DynComp complete");
      }
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
      throw new UncheckedIOException(
          "Can't get canonical file for " + filename + " in " + System.getProperty("user.dir"), e);
    }

    // I don't know why, but without this, the call to newBufferedWriter fails in some contexts.
    try {
      canonicalFile.createNewFile();
    } catch (IOException e) {
      throw new UncheckedIOException("createNewFile failed for " + canonicalFile, e);
    }

    try {
      return new PrintWriter(Files.newBufferedWriter(canonicalFile.toPath(), UTF_8));
    } catch (Exception e) {
      throw new Error("Can't open " + filename + " = " + canonicalFile, e);
    }
  }
}
