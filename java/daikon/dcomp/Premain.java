package daikon.dcomp;

import daikon.DynComp;
import daikon.chicory.DaikonVariableInfo;
import daikon.dcomp.DCRuntime.BranchInfo;
import daikon.util.*;
import java.io.*;
import java.lang.instrument.*;
import java.security.*;
import java.util.*;
import java.util.regex.*;
import org.apache.bcel.*;
import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.*;

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
  @Option("Turn on most DCRuntime debugging options")
  public static boolean debug_dcruntime = false;

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

    Options options = new Options(DynComp.usage_synopsis, DynComp.class, Premain.class);
    String[] args = options.parse_or_usage(agentArgs.split("  *"));
    if (args.length > 0) {
      options.print_usage("Unexpected argument %s", args[0]);
      System.exit(-1);
    }

    DaikonVariableInfo.std_visibility = DynComp.std_visibility;
    DCRuntime.depth = DynComp.nesting_depth;

    if (DynComp.no_jdk) DCInstrument.jdk_instrumented = false;

    if (DynComp.verbose) {
      System.out.format(
          "In dcomp premain, agentargs ='%s', " + "Instrumentation = '%s'\n", agentArgs, inst);
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
      BufferedReader reader = new BufferedReader(new InputStreamReader(strm));
      while (true) {
        String line = reader.readLine();
        if (line == null) break;
        // System.out.printf ("adding '%s'%n", line);
        pre_instrumented.add(line);
      }
    }

    // Find out what classes are already loaded
    //Class<?>[] loaded_classes = inst.getAllLoadedClasses();
    //for (Class<?> loaded_class : loaded_classes) {
    // System.out.printf ("loaded class = %s\n", loaded_class.getName());
    //}

    // Setup the shutdown hook
    Thread shutdown_thread = new ShutdownThread();
    java.lang.Runtime.getRuntime().addShutdownHook(shutdown_thread);

    // Setup the transformer
    Object transformer = null;
    // use a special classloader to ensure correct version of BCEL is used
    ClassLoader loader = new daikon.chicory.ChicoryPremain.ChicoryLoader();
    try {
      transformer = loader.loadClass("daikon.dcomp.Instrument").newInstance();
      @SuppressWarnings("unchecked")
      Class<Instrument> c = (Class<Instrument>) transformer.getClass();
      // System.out.printf ("Classloader of tranformer = %s%n",
      //                    c.getClassLoader());
    } catch (Exception e) {
      throw new RuntimeException("Unexpected error loading Instrument", e);
    }
    inst.addTransformer((ClassFileTransformer) transformer);

    // Initialize the static tag array
    DCRuntime.init();
  }

  /** Shutdown thread that writes out the comparability results. */
  public static class ShutdownThread extends Thread {

    public void run() {

      in_shutdown = true;

      // If DataFlow, print out the DF for the specified branch
      if (DynComp.branch != null) {

        if (DynComp.verbose) {
          System.err.printf(
              "Branch %s executed %d times\n", DynComp.branch, DCRuntime.branch_tags.size());
          for (BranchInfo bi : DCRuntime.branch_tags) {
            DCRuntime.debug_timing.log_time("writing branch data");
            System.err.printf("  --------------- compare-to: %s%n", bi.compared_to);
            if (bi.value_source == null) {
              System.err.printf("  Warning: null vs encountered%n");
              continue;
            }
            System.err.printf("%s%n", bi.value_source.tree_dump());
          }
        }

        // if an output file was requested, write the index of each local
        // in the test sequence that was associated with the dataflow to
        // the file in the format <local-offset> <local-offset>...
        // The file contains exactly one line.
        if (DynComp.dataflow_out != null) {
          PrintWriter dataflow_fp = null;
          try {
            dataflow_fp = new PrintWriter(DynComp.dataflow_out);
          } catch (Exception e) {
            throw new RuntimeException("Can't open dataflow output file" + DynComp.dataflow_out, e);
          }
          if (DCRuntime.exit_exception != null) {
            System.out.printf("Writing error output to %s%n", DynComp.dataflow_out);
            dataflow_fp.printf("Error: %s%n", DCRuntime.exit_exception);
            assert DCRuntime.exit_exception != null
                : "@AssumeAssertion(nullness): limited side effects don't change this field";
            DCRuntime.exit_exception.printStackTrace(dataflow_fp);
          } else { // no error was encountered, results should be good

            System.out.printf("Writing dataflow output to %s%n", DynComp.dataflow_out);
            Map<String, Set<String>> locals = new LinkedHashMap<String, Set<String>>();
            for (BranchInfo bi : DCRuntime.branch_tags) {
              DCRuntime.debug_timing.log_time("Processing bi %s%n", bi);
              Map<String, Set<String>> bi_locals = bi.value_source.get_var_compares(bi.compared_to);
              for (String local : bi_locals.keySet()) {
                Set<String> compare_to_set = locals.get(local);
                if (compare_to_set == null) {
                  compare_to_set = new LinkedHashSet<String>();
                  locals.put(local, compare_to_set);
                }
                compare_to_set.addAll(bi_locals.get(local));
              }
            }

            for (String local : locals.keySet()) {
              dataflow_fp.printf("%s ", local);
              for (String ct : locals.get(local)) {
                dataflow_fp.printf("%s ", ct);
              }
              dataflow_fp.println();
            }
          }
          dataflow_fp.close();
        }
        return;
      }

      // If requested, write the comparability data to a file
      if (!DynComp.no_cset_file) {
        if (DynComp.compare_sets_file != null) {
          if (DynComp.verbose) {
            System.out.println("Writing comparability sets to " + DynComp.compare_sets_file);
          }
          assert DynComp.compare_sets_file != null
              : "@AssumeAssertion(nullness): limited side effects don't change this field";
          PrintWriter compare_out = open(DynComp.compare_sets_file);
          Stopwatch watch = new Stopwatch();
          if (DynComp.no_primitives) {
            DCRuntime.print_all_comparable_refs_only(compare_out);
          } else {
            DCRuntime.print_all_comparable(compare_out);
          }
          compare_out.close();
          if (DynComp.verbose) {
            System.out.printf("Comparability sets written in %s%n", watch.format());
          }
        } else {
          System.out.println("Writing comparability sets to standard output");
          if (DynComp.no_primitives) {
            DCRuntime.print_all_comparable_refs_only(new PrintWriter(System.out, true));
          } else {
            DCRuntime.print_all_comparable(new PrintWriter(System.out, true));
          }
        }
      }

      if (DynComp.trace_sets_file != null) {
        if (DynComp.verbose) {
          System.out.println("Writing traced comparability sets to " + DynComp.trace_sets_file);
        }
        assert DynComp.trace_sets_file != null
            : "@AssumeAssertion(nullness): limited side effects don't change this field";
        PrintWriter trace_out = open(DynComp.trace_sets_file);
        Stopwatch watch = new Stopwatch();
        DCRuntime.trace_all_comparable(trace_out);
        trace_out.close();
        if (DynComp.verbose) {
          System.out.printf("Comparability sets written in %s%n", watch.format());
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
      Stopwatch watch = new Stopwatch();
      DCRuntime.print_decl_file(decl_fp);
      decl_fp.close();
      if (DynComp.verbose) {
        System.out.printf("Decl file written in %s%n", watch.format());
        System.out.printf("comp_list = %,d%n", DCRuntime.comp_list_ms);
        System.out.printf("ppt name  = %,d%n", DCRuntime.ppt_name_ms);
        System.out.printf("decl vars = %,d%n", DCRuntime.decl_vars_ms);
        System.out.printf("total     = %,d%n", DCRuntime.total_ms);
      }
      if (DynComp.verbose) System.out.println("DynComp complete");
    }
  }

  /** Returns the local name (eg, var0, var1) that corresponds to a specific local-store. */
  public static String seq_local_name(String local_store) {
    assert local_store.startsWith("local-store");
    int local_index = Integer.decode(local_store.split(" ")[1]);
    String local_name = DFInstrument.test_seq_locals[local_index];
    return local_name;
  }

  public static PrintWriter open(File filename) {
    try {
      return new PrintWriter(new BufferedWriter(new FileWriter(filename)));
      //return new PrintWriter (filename);
      //return new PrintStream (new BufferedWriter
      //            (new Outpu32tStreamWriter (new FileOutputStream(filename))));
    } catch (Exception e) {
      throw new Error("Can't open " + filename, e);
    }
  }
}
