// TraceSelect.java
package daikon.tools;

import static java.nio.charset.StandardCharsets.UTF_8;

import daikon.DaikonGetopt;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Random;
import java.util.StringTokenizer;
import java.util.regex.Pattern;
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

  /**
   * Matches an argument that has the form of a TraceSelect option: a hyphen followed by capital
   * letters and underscores. No Daikon argument has this form.
   */
  private static final Pattern TRACESELECT_OPTION = Pattern.compile("-[A-Z_]{2,}");

  /** Matches a negative integer, which is not an option. */
  private static final Pattern NEGATIVE_INTEGER = Pattern.compile("-[0-9]+");

  /** The usage message for this program. */
  private static final String usage =
      StringsPlume.joinLines(
          "USAGE: TraceSelect num_reps sample_size [options] [Daikon-args]...",
          "num_reps and sample_size must be positive integers.",
          "The options are -SEED n, -NOCLEAN, -INCLUDE_UNRETURNED, and -DO_DIFFS.",
          "The Daikon-args start with the first argument that is not one of those options.",
          "Exactly one of the Daikon-args must be a .dtrace file; it is the file to sample.",
          "The Daikon-args must not include -o, which TraceSelect supplies for each sample.",
          "Example: java TraceSelect 20 10 -NOCLEAN -INCLUDE_UNRETURNED -SEED 1000 foo.dtrace"
              + " foo.decls RatPoly.decls",
          "",
          "The Daikon-args are:",
          daikon.Daikon.usage);

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
    // Handles -h and --help before num_reps.  A negative num_reps is left for parsePositiveInt to
    // report.
    if (args.length == 0 || !NEGATIVE_INTEGER.matcher(args[0]).matches()) {
      args = DaikonGetopt.argsAfterLeadingOptions(args, usage);
    }

    if (args.length < 2) {
      throw new daikon.Daikon.UserError("Too few arguments; run with -h for usage");
    }

    int numReps = parsePositiveInt("num_reps", args[0]);
    int numPerSample = parsePositiveInt("sample_size", args[1]);

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
        String seed = args[++i];
        try {
          rand = new Random(Long.parseLong(seed));
        } catch (NumberFormatException e) {
          throw new daikon.Daikon.UserError("-SEED requires an integer argument, not " + seed);
        }
      } else if (option.equals("-NOCLEAN")) {
        clean = false;
      } else if (option.equals("-INCLUDE_UNRETURNED")) {
        includeUnreturned = true;
      } else if (option.equals("-DO_DIFFS")) {
        doDiffs = true;
      } else if (TRACESELECT_OPTION.matcher(args[i]).matches()) {
        throw new daikon.Daikon.UserError(
            "Unrecognized TraceSelect option "
                + args[i]
                + "; the options are -SEED, -NOCLEAN, -INCLUDE_UNRETURNED, and -DO_DIFFS");
      } else {
        break;
      }
    }

    // The remaining arguments are the Daikon arguments, such as .dtrace and .decls files,
    // "--nohierarchy", or "--format java".  Only the .dtrace file is sampled.  Daikon treats any
    // file whose name contains ".dtrace" as a trace file, so every such file must be the one that
    // is sampled.
    String inputFile = null;
    List<String> daikonArgs = new ArrayList<>();
    for (; i < args.length; i++) {
      if (args[i].contains(".dtrace")) {
        if (!args[i].endsWith(".dtrace")) {
          throw new daikon.Daikon.UserError(
              "TraceSelect cannot sample "
                  + args[i]
                  + "; the trace file must be uncompressed and its name must end with"
                  + " \".dtrace\"");
        }
        if (inputFile != null) {
          throw new daikon.Daikon.UserError("Only 1 dtrace file for input allowed");
        }
        inputFile = args[i];
      } else {
        daikonArgs.add(args[i]);
      }
    }

    // Check the Daikon arguments before doing any sampling, so that a bad argument is reported
    // immediately.  This also handles -h and --help.  When there is a trace file, the arguments
    // are the same as those of a sample's Daikon run, except for the .dtrace file, which does not
    // exist yet.
    if (inputFile != null || !daikonArgs.isEmpty()) {
      checkDaikonArgs(
          (inputFile == null)
              ? daikonArgs.toArray(new String[0])
              : daikonCommandLine(inputFile, daikonArgs));
    }
    if (inputFile == null) {
      throw new daikon.Daikon.UserError(
          "No .dtrace file name specified (the trace file must be uncompressed and its name must"
              + " end with \".dtrace\")");
    }

    // The .inv file for each sample, from the first sample to the last.
    List<String> sampleNames = new ArrayList<>();

    System.out.println("*******Processing********");

    try {
      for (int rep = 1; rep <= numReps; rep++) {

        List<String> al;
        try {
          al = selectSample(inputFile, numPerSample, rand, includeUnreturned);
        } catch (IOException e) {
          throw new daikon.Daikon.UserError(
              e, "Cannot read trace file " + inputFile + ": " + e.getMessage());
        }

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
                "Cannot write sample file " + filePrefix + ": " + e.getMessage());
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
   * Randomly selects invocations from a trace file.
   *
   * @param inputFile the trace file to sample
   * @param numPerSample the number of invocations to select for each program point
   * @param rand the source of randomness
   * @param includeUnreturned if true, also select invocations that entered a method but did not
   *     exit it normally
   * @return the selected invocations, each followed by its matching exit, in no particular order
   * @throws IOException if the trace file cannot be read
   */
  public static List<String> selectSample(
      String inputFile, int numPerSample, Random rand, boolean includeUnreturned)
      throws IOException {
    try (DtracePartitioner dec = new DtracePartitioner(inputFile)) {
      MultiRandSelector<String> mrs = new MultiRandSelector<>(numPerSample, rand, dec);

      while (dec.hasNext()) {
        // DtracePartitioner returns an empty string for a blank line or for a trailing EXIT.
        String invocation = dec.next();
        if (!invocation.isEmpty()) {
          mrs.accept(invocation);
        }
      }

      List<String> al = new ArrayList<>();
      for (Iterator<String> iter = mrs.valuesIter(); iter.hasNext(); ) {
        al.add(iter.next());
      }

      return dec.patchValues(al, includeUnreturned);
    }
  }

  /**
   * Parses a command-line argument that must be a positive integer.
   *
   * @param name the name of the argument, for use in error messages
   * @param arg the command-line argument
   * @return the integer value of the argument
   * @throws daikon.Daikon.UserError if the argument is not a positive integer
   */
  private static int parsePositiveInt(String name, String arg) {
    int result;
    try {
      result = Integer.parseInt(arg);
    } catch (NumberFormatException e) {
      result = 0;
    }
    if (result <= 0) {
      throw new daikon.Daikon.UserError(
          name + " must be a positive integer, not " + arg + "; run with -h for usage");
    }
    return result;
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
   * Checks Daikon command-line arguments by passing them to {@link daikon.Daikon#read_options}.
   * Handles -h and --help.
   *
   * <p>This does not call {@link daikon.Daikon#cleanup}, so that it does not discard global state
   * that the caller may depend on. {@code read_options} sets static fields for the options in
   * {@code args}; {@link daikon.Daikon#mainHelper} resets them before it runs. An exception is
   * {@code -o}, which {@code read_options} rejects if {@link daikon.Daikon#inv_file} is already
   * set. This method clears that field during the check and then restores it.
   *
   * @param args the Daikon command-line arguments
   * @throws daikon.Daikon.NormalTermination after printing the usage message, if an argument
   *     requests it
   * @throws daikon.Daikon.UserError if an argument is bad
   */
  private static void checkDaikonArgs(String[] args) {
    File savedInvFile = daikon.Daikon.inv_file;
    daikon.Daikon.inv_file = null;
    try {
      daikon.Daikon.read_options(args, usage);
    } finally {
      daikon.Daikon.inv_file = savedInvFile;
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
    // Reset the Daikon options, such as --format, so that PrintInvariants uses its defaults.
    daikon.Daikon.cleanup();
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
