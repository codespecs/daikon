// TraceSelect.java
package daikon.tools;

import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.StringTokenizer;
import org.checkerframework.dataflow.qual.Pure;
import org.plumelib.util.MultiRandSelector;
import org.plumelib.util.StringsPlume;

/**
 * The TraceSelect tool creates several small subsets of the data by randomly selecting parts of the
 * original trace file.
 */
public class TraceSelect {

  /** Do not instantiate. */
  private TraceSelect() {
    throw new UnsupportedOperationException("Do not instantiate");
  }

  /** The usage message for this program. */
  private static final String usage =
      StringsPlume.joinLines(
          "USAGE: TraceSelect num_reps sample_size [options] dtrace_file [Daikon-args]...",
          "Example: java TraceSelect 20 10 -NOCLEAN -INCLUDE_UNRETURNED -SEED 1000 foo.dtrace"
              + " foo.decls RatPoly.decls");

  /** The TraceSelect options, in upper case. */
  private static final Set<String> TRACESELECT_OPTIONS =
      Set.of("-SEED", "-NOCLEAN", "-INCLUDE_UNRETURNED", "-DO_DIFFS");

  /**
   * The entry point of TraceSelect.
   *
   * @param args command-line arguments
   */
  public static void main(String[] args) {
    try {
      mainHelper(args);
    } catch (daikon.Daikon.DaikonTerminationException e) {
      daikon.Daikon.handleDaikonTerminationException(e);
    }
  }

  /**
   * This does the work of {@link #main(String[])}, but it never calls System.exit, so it is
   * appropriate to be called programmatically.
   *
   * @param args command-line arguments, like those of {@link #main}
   */
  public static void mainHelper(String[] args) {
    if (args.length == 0) {
      throw new daikon.Daikon.UserError("No arguments found." + daikon.Daikon.lineSep + usage);
    }

    int numReps = Integer.parseInt(args[0]);
    int numPerSample = Integer.parseInt(args[1]);

    // If true, delete the trace samples after the invariants from them have been generated.
    boolean clean = true;
    // If true, select method invocations that entered the method successfully but did not exit
    // normally, either from a thrown Exception or abnormal termination.
    boolean includeUnreturned = false;
    // If true, create a spinfo file for generating conditional invariants and implications by
    // running daikon.diff.MultiDiff over each of the samples and finding properties that appear
    // in some but not all of the samples.
    boolean doDiffs = false;
    Random rand = new Random();

    // Process the TraceSelect options, which precede the Daikon arguments.
    int i = 2;
    for (; i < args.length; i++) {
      String option = args[i].toUpperCase(Locale.ENGLISH);
      if (option.equals("-SEED")) {
        if (i + 1 >= args.length) {
          throw new daikon.Daikon.UserError("-SEED requires an argument");
        }
        rand = new Random(Long.parseLong(args[++i]));
      } else if (option.equals("-NOCLEAN")) {
        clean = false;
      } else if (option.equals("-INCLUDE_UNRETURNED")) {
        includeUnreturned = true;
      } else if (option.equals("-DO_DIFFS")) {
        doDiffs = true;
      } else if (option.startsWith("-") && !option.startsWith("--")) {
        // Every Daikon option starts with "--", except "-o", which TraceSelect supplies.
        throw new daikon.Daikon.UserError(
            "Unknown TraceSelect option: " + args[i] + daikon.Daikon.lineSep + usage);
      } else {
        break;
      }
    }

    // The remaining arguments are the Daikon arguments, such as .dtrace and .decls files,
    // "--nohierarchy", or "--format java".  Only the .dtrace file is sampled.
    String inputFile = null;
    List<String> daikonArgs = new ArrayList<>();
    for (; i < args.length; i++) {
      if (TRACESELECT_OPTIONS.contains(args[i].toUpperCase(Locale.ENGLISH))) {
        throw new daikon.Daikon.UserError(
            "TraceSelect option "
                + args[i]
                + " must precede the Daikon arguments"
                + daikon.Daikon.lineSep
                + usage);
      }
      if (args[i].endsWith(".dtrace")) {
        if (inputFile != null) {
          throw new daikon.Daikon.UserError("Only 1 dtrace file for input allowed");
        }
        inputFile = args[i];
      } else {
        daikonArgs.add(args[i]);
      }
    }
    if (inputFile == null) {
      throw new daikon.Daikon.UserError("No .dtrace file name specified");
    }

    // The .inv file for each sample, from the first sample to the last.
    List<String> sampleNames = new ArrayList<>();

    System.out.println("*******Processing********");

    List<List<String>> samples;
    try {
      samples = selectSamples(inputFile, numReps, numPerSample, rand, includeUnreturned);
    } catch (IOException e) {
      throw new daikon.Daikon.UserError(
          e, "Cannot read trace file " + inputFile + ": " + e.getMessage());
    }

    try {
      for (int rep = 1; rep <= numReps; rep++) {

        List<String> al = samples.get(rep - 1);

        String filePrefix = calcOut(inputFile, rep);

        sampleNames.add(filePrefix + ".inv");

        try {
          // Files.newBufferedWriter truncates an existing file, such as a sample from a previous
          // run with -NOCLEAN.
          try (PrintWriter pwOut =
              new PrintWriter(Files.newBufferedWriter(Path.of(filePrefix), UTF_8))) {
            for (String toPrint : al) {
              pwOut.println(toPrint);
            }
          } catch (IOException e) {
            throw new daikon.Daikon.UserError(
                e, "Cannot write sample file " + filePrefix + ": " + e.getMessage());
          }

          invokeDaikon(filePrefix, daikonArgs);
        } finally {
          // Clean up the mess, even if Daikon failed.  A failed cleanup is not fatal.
          if (clean) {
            deleteQuietly(filePrefix);
          }
        }
      }

      if (doDiffs) {
        // spinfo format.  The arguments are "-p" followed by the samples.
        List<String> multiDiffArgs = new ArrayList<>();
        multiDiffArgs.add("-p");
        multiDiffArgs.addAll(sampleNames);
        daikon.diff.MultiDiff.mainHelper(multiDiffArgs.toArray(new String[0]));
      }

    } catch (IOException e) {
      throw new daikon.Daikon.UserError(e, "I/O error while processing samples: " + e.getMessage());
    } catch (ReflectiveOperationException e) {
      throw new daikon.Daikon.BugInDaikon(e);
    } finally {
      // Clean up the mess!  A failed cleanup is not fatal.
      if (clean) {
        for (String sampleName : sampleNames) {
          deleteQuietly(sampleName);
        }
      }
    }
  }

  /**
   * Randomly selects several samples of invocations from a trace file. Reads the trace file twice,
   * regardless of the number of samples.
   *
   * @param inputFile the trace file to sample
   * @param numReps the number of samples to select
   * @param numPerSample the number of invocations to select for each program point, in each sample
   * @param rand the source of randomness
   * @param includeUnreturned if true, also select invocations that entered a method but did not
   *     exit it normally
   * @return the samples. Each sample contains the selected invocations, in no particular order.
   *     Each ENTER record is followed by its matching EXIT record, if any; an ENTER record without
   *     an EXIT record appears only if {@code includeUnreturned} is true. OBJECT and CLASS
   *     records appear alone.
   * @throws IOException if the trace file cannot be read
   */
  public static List<List<String>> selectSamples(
      String inputFile, int numReps, int numPerSample, Random rand, boolean includeUnreturned)
      throws IOException {
    try (DtracePartitioner dec = new DtracePartitioner(inputFile)) {
      List<MultiRandSelector<String>> selectors = new ArrayList<>(numReps);
      for (int rep = 0; rep < numReps; rep++) {
        selectors.add(new MultiRandSelector<>(numPerSample, rand, dec));
      }

      while (dec.hasNext()) {
        // DtracePartitioner returns an empty string for a blank line or for a trailing EXIT.
        String invocation = dec.next();
        if (!invocation.isEmpty()) {
          for (MultiRandSelector<String> mrs : selectors) {
            mrs.accept(invocation);
          }
        }
      }

      // The selected ENTER, OBJECT, and CLASS records of each sample.
      List<List<String>> selections = new ArrayList<>(numReps);
      // The selected records of all the samples.
      Set<String> allSelected = new HashSet<>();
      for (MultiRandSelector<String> mrs : selectors) {
        List<String> selection = new ArrayList<>();
        for (Iterator<String> iter = mrs.valuesIter(); iter.hasNext(); ) {
          selection.add(iter.next());
        }
        selections.add(selection);
        allSelected.addAll(selection);
      }

      Map<String, String> invocations = dec.matchExits(allSelected, includeUnreturned);

      List<List<String>> result = new ArrayList<>(numReps);
      for (List<String> selection : selections) {
        List<String> sample = new ArrayList<>();
        for (String selected : selection) {
          String invocation = invocations.get(selected);
          if (invocation != null) {
            sample.add(invocation);
          }
        }
        result.add(sample);
      }
      return result;
    } catch (UncheckedIOException e) {
      // DtracePartitioner's Iterator methods cannot throw IOException.
      throw e.getCause();
    }
  }

  /**
   * Deletes the file with the given name, if it exists. A failed deletion is not fatal; it produces
   * a warning on standard output. Tolerates a name that is not a legal file path.
   *
   * @param fileName the name of the file to delete
   */
  private static void deleteQuietly(String fileName) {
    try {
      Files.deleteIfExists(Path.of(fileName));
    } catch (IOException | InvalidPathException e) {
      System.out.println("Warning: could not delete " + fileName + ": " + e.getMessage());
    }
  }

  /**
   * Returns the command line for running Daikon on a sample.
   *
   * @param dtraceName the sample .dtrace file
   * @param daikonArgs the arguments to pass to Daikon, other than the .dtrace file
   * @return the command line for running Daikon on the sample
   */
  private static String[] daikonCommandLine(String dtraceName, List<String> daikonArgs) {
    List<String> result = new ArrayList<>();
    result.add(dtraceName);
    result.add("-o");
    result.add(dtraceName + ".inv");
    result.addAll(daikonArgs);
    return result.toArray(new String[0]);
  }

  /**
   * Runs Daikon on a sample, then runs PrintInvariants on the result.
   *
   * @param dtraceName the sample .dtrace file
   * @param daikonArgs the arguments to pass to Daikon, other than the .dtrace file
   * @throws IOException if PrintInvariants cannot read or write a file
   * @throws ClassNotFoundException if PrintInvariants cannot deserialize the invariants
   */
  private static void invokeDaikon(String dtraceName, List<String> daikonArgs)
      throws IOException, ClassNotFoundException {

    System.out.println("Created file: " + dtraceName);

    daikon.Daikon.mainHelper(daikonCommandLine(dtraceName, daikonArgs));
    daikon.PrintInvariants.mainHelper(
        new String[] {"--output", dtraceName + ".txt", dtraceName + ".inv"});
  }

  /**
   * Returns the name of the file that holds the given sample of the given trace file.
   *
   * @param strFileName the name of the trace file; ends with ".dtrace"
   * @param rep the number of the sample
   * @return the name of the file that holds the sample
   */
  private static String calcOut(String strFileName, int rep) {
    // Insert the sample number into the file's name, not into the name of a directory.
    String base = strFileName.substring(0, strFileName.length() - ".dtrace".length());
    return base + rep + ".dtrace";
  }
}

// I don't think any of this is used anymore...
// Now all of the random selection comes from the
// classes in plume.

class InvocationComparator implements Comparator<String> {
  /** Requires: s1 and s2 are String representations of invocations from a tracefile. */
  @Pure
  @Override
  public int compare(String s1, String s2) {
    if (s1 == s2) {
      return 0;
    }

    // sorts first by program point
    int pptCompare =
        s1.substring(0, s1.indexOf(":::")).compareTo(s2.substring(0, s2.indexOf(":::")));
    if (pptCompare != 0) {
      return pptCompare;
    }

    // next sorts based on the other stuff
    int nonce1 = getNonce(s1);
    int nonce2 = getNonce(s2);
    int type1 = getType(s1);
    int type2 = getType(s2);
    // This makes sure nonce takes priority, ties are broken
    // so that ENTER comes before EXIT for the same program point
    return 3 * (nonce1 - nonce2) + (type1 - type2);
  }

  private int getNonce(String s1) {
    if (s1.indexOf("OBJECT") != -1 || s1.indexOf("CLASS") != -1) {
      // it's ok, no chance of overflow wrapa round
      return Integer.MAX_VALUE;
    }
    StringTokenizer st = new StringTokenizer(s1);
    st.nextToken();
    st.nextToken();
    return Integer.parseInt(st.nextToken());
  }

  private int getType(String s1) {
    // we want ENTER to come before EXIT
    if (s1.indexOf("CLASS") != -1) {
      return -1;
    }
    if (s1.indexOf("OBJECT") != -1) {
      return 0;
    }
    if (s1.indexOf("ENTER") != -1) {
      return 1;
    }
    if (s1.indexOf("EXIT") != -1) {
      return 2;
    }
    System.out.println("ERROR" + s1);
    return 0;
  }
}
