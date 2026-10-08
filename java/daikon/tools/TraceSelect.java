// TraceSelect.java
package daikon.tools;

import daikon.DaikonGetopt;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UncheckedIOException;
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
import org.checkerframework.dataflow.qual.Pure;
import org.plumelib.util.FilesPlume;
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
          "USAGE: TraceSelect num_reps sample_size [options] [Daikon-args]...",
          "num_reps and sample_size must be positive integers.",
          "The options are -SEED n, -NOCLEAN, -INCLUDE_UNRETURNED, and -DO_DIFFS.",
          "The Daikon-args start with the first argument that is not one of those options.",
          "Exactly one of the Daikon-args must be a .dtrace file; it is the file to sample.",
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
    // Handles -h and --help before num_reps.
    args = DaikonGetopt.argsAfterLeadingOptions(args, usage);

    if (args.length < 2) {
      throw new daikon.Daikon.UserError("Too few arguments." + daikon.Daikon.lineSep + usage);
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
      } else {
        break;
      }
    }

    // The remaining arguments are the Daikon arguments, such as .dtrace and .decls files,
    // "--nohierarchy", or "--format java".  Only the .dtrace file is sampled.
    String inputFile = null;
    List<String> daikonArgs = new ArrayList<>();
    for (; i < args.length; i++) {
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
      throw new daikon.Daikon.UserError(
          "No .dtrace file name specified (the trace file must be uncompressed and its name must"
              + " end with \".dtrace\")");
    }

    // Check the Daikon arguments before doing any sampling, so that a bad argument (including a
    // misspelled TraceSelect option) is reported immediately.  This also handles -h and --help.
    List<String> argsToCheck = new ArrayList<>(daikonArgs);
    argsToCheck.add(inputFile);
    daikon.Daikon.read_options(argsToCheck.toArray(new String[0]), usage);

    // The arguments to daikon.diff.MultiDiff: "-p" followed by the .inv file for each sample.
    String[] sampleNames = new String[numReps + 1];
    sampleNames[0] = "-p";

    System.out.println("*******Processing********");

    try {
      for (int rep = numReps; rep > 0; rep--) {

        List<String> al = new ArrayList<>();
        try (DtracePartitioner dec = new DtracePartitioner(inputFile)) {
          MultiRandSelector<String> mrs = new MultiRandSelector<>(numPerSample, rand, dec);

          while (dec.hasNext()) {
            mrs.accept(dec.next());
          }

          for (Iterator<String> iter = mrs.valuesIter(); iter.hasNext(); ) {
            al.add(iter.next());
          }

          al = dec.patchValues(al, includeUnreturned);
        }

        String filePrefix = calcOut(inputFile, rep);

        sampleNames[rep] = filePrefix + ".inv";

        try {
          try (PrintWriter pwOut = new PrintWriter(FilesPlume.newBufferedFileWriter(filePrefix))) {
            for (String toPrint : al) {
              pwOut.println(toPrint);
            }
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
        // spinfo format
        daikon.diff.MultiDiff.mainHelper(sampleNames);
      }

    } catch (IOException e) {
      throw new UncheckedIOException(e);
    } catch (ReflectiveOperationException e) {
      throw new daikon.Daikon.BugInDaikon(e);
    } finally {
      // Clean up the mess!  A failed cleanup is not fatal.
      // Start at index 1: sampleNames[0] is the "-p" sentinel, not a file.
      if (clean) {
        for (int j = 1; j < sampleNames.length; j++) {
          if (sampleNames[j] != null) {
            deleteQuietly(sampleNames[j]);
          }
        }
      }
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
          name + " must be a positive integer, not " + arg + daikon.Daikon.lineSep + usage);
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
   * Runs Daikon on a sample, then runs PrintInvariants on the result.
   *
   * @param dtraceName the sample .dtrace file
   * @param daikonArgs the arguments to pass to Daikon, other than the .dtrace file
   * @throws IOException if PrintInvariants cannot be run
   */
  private static void invokeDaikon(String dtraceName, List<String> daikonArgs) throws IOException {

    System.out.println("Created file: " + dtraceName);

    List<String> daikonArgsList = new ArrayList<>();
    daikonArgsList.add(dtraceName);
    daikonArgsList.add("-o");
    daikonArgsList.add(dtraceName + ".inv");
    daikonArgsList.addAll(daikonArgs);

    daikon.Daikon.mainHelper(daikonArgsList.toArray(new String[0]));
    // Run: java daikon.PrintInvariants dtraceName.inv > dtraceName.txt
    ProcessBuilder pb = new ProcessBuilder("java", "daikon.PrintInvariants", dtraceName + ".inv");
    pb.redirectOutput(new File(dtraceName + ".txt"));
    // In Java 26, `Process` implements `AutoCloseable`, so use try-with-resources.
    @SuppressWarnings({
      "resourceleak:required.method.not.called",
      "resourceleak:unneeded.suppression"
    })
    Process p = pb.start();
    try {
      p.waitFor();
    } catch (InterruptedException e) {
      // do nothing
    }
  }

  /**
   * Returns the name of the file that holds the given sample of the given trace file.
   *
   * @param strFileName the name of the trace file
   * @param rep the number of the sample
   * @return the name of the file that holds the sample
   */
  private static String calcOut(String strFileName, int rep) {
    StringBuilder product = new StringBuilder();
    int index = strFileName.indexOf('.');
    if (index >= 0) {
      product.append(strFileName.substring(0, index));
      product.append(rep);
      if (index != strFileName.length()) {
        product.append(strFileName.substring(index));
      }
    } else {
      product.append(strFileName).append("2");
    }
    return product.toString();
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
