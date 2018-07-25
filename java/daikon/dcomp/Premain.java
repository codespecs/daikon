package daikon.dcomp;

import static java.nio.charset.StandardCharsets.UTF_8;

import daikon.DynComp;
import daikon.chicory.DaikonVariableInfo;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.lang.instrument.ClassFileTransformer;
import java.lang.instrument.Instrumentation;
import java.nio.file.Files;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import org.apache.bcel.*;
import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.*;
import org.plumelib.options.Option;
import org.plumelib.options.Options;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.checker.signature.qual.*;
*/

public class Premain {

  /**
   * Any command line options declared here are 'hidden' as they cannot be accessed from DynComp.
   * These are internal debugging options that may be used when Premain is invoked directly from the
   * command line.
   */
  @Option("Experimental no instrumentation if no track option")
  public static boolean notrack_instrumentation = false;

  @Option("Turn on basic DCInstrument debugging options")
  public static boolean debug_dcinstrument = false;

  @Option("Turn on basic DCRuntime debugging options")
  public static boolean debug_dcruntime = false;

  @Option("Turn on most DCRuntime debugging options")
  public static boolean debug_dcruntime_all = false;

  /**
   * Set of pre_instrumented jdk classes. Needed so that we will instrument classes generated on the
   * fly in the jdk.
   */
  public static Set<String> pre_instrumented = new LinkedHashSet<String>();

  // One of the last phases for DynComp is to write out the comparability values
  // after the user program completes execution.  One of the steps is to assign
  // values to the arguments of methods that have not been executed.  We use
  // reflection to get type information about these arguments, which causes the
  // method to be loaded; which causes the main part of DynComp to try and
  // instrument the method.  As the user program has completed execution, doing
  // instrumentation at this point can lead to problems.  The correct fix for
  // this problem is to use BCEL to get the type information instead of reflection,
  // thus avoiding loading the method into the JVM.  This will be a large change,
  // so a temporary fix is to indicate if the program is in shutdown mode and
  // not instrument any methods when this flag is true.
  public static boolean in_shutdown = false;

  public static void premain(String agentArgs, Instrumentation inst) throws IOException {

    // Because DynComp started Premain in a separate process, we must rescan
    // the options to setup the DynComp static variables.
    Options options = new Options(DynComp.usage_synopsis, DynComp.class, Premain.class);
    String[] args = options.parse(true, agentArgs.split("  *"));
    if (args.length > 0) {
      System.out.printf("Unexpected argument %s%n", args[0]);
      options.printUsage();
      System.exit(-1);
    }
    if (DynComp.rt_file != null && DynComp.rt_file.getName().equalsIgnoreCase("NONE")) {
      DynComp.no_jdk = true;
      DynComp.rt_file = null;
    }

    DaikonVariableInfo.std_visibility = DynComp.std_visibility;
    DCRuntime.depth = DynComp.nesting_depth;

    if (DynComp.no_jdk) DCInstrument.jdk_instrumented = false;

    if (DynComp.verbose) {
      System.out.format(
          "In dcomp premain, agentargs ='%s', Instrumentation = '%s'\n", agentArgs, inst);
      System.out.printf("Options settings: %n%s%n", options.settings());
    }

    // Read in the list of pre-instrumented classes
    if (!DynComp.no_jdk) {
      // location is: daikon/java/dcomp-rt/java/lang/jdk_classes.txt .
      // As of build b68, this fails when using JDK 7 (OpenJDK).
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

    // Find out what classes are already loaded
    // Class<?>[] loaded_classes = inst.getAllLoadedClasses();
    // for (Class<?> loaded_class : loaded_classes) {
    // System.out.printf("loaded class = %s\n", loaded_class.getName());
    // }

    // Setup the shutdown hook
    Thread shutdown_thread = new ShutdownThread();
    java.lang.Runtime.getRuntime().addShutdownHook(shutdown_thread);

    // Setup the transformer
    Object transformer;
    // use a special classloader to ensure correct version of BCEL is used
    ClassLoader loader = new daikon.chicory.ChicoryPremain.ChicoryLoader();
    try {
      transformer =
          loader.loadClass("daikon.dcomp.Instrument").getDeclaredConstructor().newInstance();
      @SuppressWarnings("unchecked")
      Class<Instrument> c = (Class<Instrument>) transformer.getClass();
      // System.out.printf("Classloader of tranformer = %s%n",
      //                    c.getClassLoader());
    } catch (Exception e) {
      throw new RuntimeException("Unexpected error loading Instrument", e);
    }

    // check that we got a newer version of BCEL that includes the LocalVariable fix.
    try {
      Class<?> c = loader.loadClass("org.apache.bcel.generic.LocalVariableGen");
      c.getMethod("getLiveToEnd", (Class<?>[]) null);
    } catch (Exception e) {
      System.err.printf("%nBCEL jar found is not the version included with the Daikon release.%n");
      System.exit(1);
    }

    // now turn on instrumentation
    inst.addTransformer((ClassFileTransformer) transformer);

    // Initialize the static tag array
    DCRuntime.init();
  }

  /** Shutdown thread that writes out the comparability results. */
  public static class ShutdownThread extends Thread {

    @Override
    public void run() {

      in_shutdown = true;

      // If requested, write the comparability data to a file
      if (DynComp.comparability_file != null) {
        if (DynComp.verbose) {
          System.out.println("Writing comparability sets to " + DynComp.comparability_file);
        }
        assert DynComp.comparability_file != null
            : "@AssumeAssertion(nullness): limited side effects don't change this field";
        PrintWriter compare_out = open(DynComp.comparability_file);
        long startTime = System.nanoTime();
        if (DynComp.no_primitives) {
          DCRuntime.print_all_comparable_refs_only(compare_out);
        } else {
          DCRuntime.print_all_comparable(compare_out);
        }
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
        assert DynComp.trace_file != null
            : "@AssumeAssertion(nullness): limited side effects don't change this field";
        PrintWriter trace_out = open(DynComp.trace_file);
        long startTime = System.nanoTime();
        DCRuntime.trace_all_comparable(trace_out);
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
      long startTime = System.nanoTime();
      DCRuntime.print_decl_file(decl_fp);
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

  public static PrintWriter open(File filename) {
    try {
      return new PrintWriter(Files.newBufferedWriter(filename.toPath(), UTF_8));
      // return new PrintWriter (filename);
      // return new PrintStream (new BufferedWriter
      //            (new Outpu32tStreamWriter (new FileOutputStream(filename))));
    } catch (Exception e) {
      throw new Error("Can't open " + filename, e);
    }
  }
}
