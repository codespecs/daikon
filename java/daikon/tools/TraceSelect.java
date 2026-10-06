// TraceSelect.java
package daikon.tools;

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
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
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

  public static boolean CLEAN = true;
  public static boolean INCLUDE_UNRETURNED = false;
  public static boolean DO_DIFFS = false;

  private static int num_reps;

  private static @MonotonicNonNull String fileName = null;

  // // stores the invocations in Strings
  // private static ArrayList invokeBuffer;

  private static int numPerSample;

  // always set to non-null by mainHelper
  private static @MonotonicNonNull Random randObj;

  /** The arguments to pass to Daikon, other than the .dtrace file. */
  private static List<String> daikonArgs = new ArrayList<>();

  // This allows us to simply call MultiDiff
  // with the same files we just created.
  // Always set to non-null by mainHelper
  private static String @MonotonicNonNull [] sampleNames;

  /** The usage message for this program. */
  private static final String usage =
      StringsPlume.joinLines(
          "USAGE: TraceSelect num_reps sample_size [options] [Daikon-args]...",
          "The options are -SEED n, -NOCLEAN, -INCLUDE_UNRETURNED, and -DO_DIFFS.",
          "The Daikon-args start with the first argument that is not one of those options.",
          "Example: java TraceSelect 20 10 -NOCLEAN -INCLUDE_UNRETURNED -SEED 1000 foo.dtrace"
              + " foo2.dtrace foo.decls RatPoly.decls foo3.dtrace");

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
    // Reset the state from any previous call.
    CLEAN = true;
    INCLUDE_UNRETURNED = false;
    DO_DIFFS = false;
    daikonArgs = new ArrayList<>();
    String inputFile = null;
    Random rand = null;

    if (args.length < 2) {
      throw new daikon.Daikon.UserError("Too few arguments." + daikon.Daikon.lineSep + usage);
    }

    try {
      num_reps = Integer.parseInt(args[0]);
      numPerSample = Integer.parseInt(args[1]);
    } catch (NumberFormatException e) {
      throw new daikon.Daikon.UserError(
          "num_reps and sample_size must be integers." + daikon.Daikon.lineSep + usage);
    }

    // process optional switches
    // also deduce index of arg for Daikon
    boolean knowArgStart = false;
    for (int i = 2; i < args.length; i++) {
      // allows seed setting
      if (args[i].toUpperCase(Locale.ENGLISH).equals("-SEED")) {
        if (i + 1 >= args.length) {
          throw new daikon.Daikon.UserError("-SEED requires an argument");
        }
        String seed = args[++i];
        try {
          rand = new Random(Long.parseLong(seed));
        } catch (NumberFormatException e) {
          throw new daikon.Daikon.UserError("-SEED requires an integer argument, not " + seed);
        }
      }

      // NOCLEAN argument will leave the trace samples even after
      // the invariants from these samples have been generated
      else if (args[i].toUpperCase(Locale.ENGLISH).equals("-NOCLEAN")) {
        CLEAN = false;
      }

      // INCLUDE_UNRETURNED option will allow selecting method invocations
      // that entered the method successfully but did not exit normally;
      // either from a thrown Exception or abnormal termination.
      else if (args[i].toUpperCase(Locale.ENGLISH).equals("-INCLUDE_UNRETURNED")) {
        INCLUDE_UNRETURNED = true;
      }

      // DO_DIFFS will create a spinfo file for generating
      // conditional invariants and implications by running
      // daikon.diff.Diff over each of the samples and finding
      // properties that appear in some but not all of the
      // samples.
      else if (args[i].toUpperCase(Locale.ENGLISH).equals("-DO_DIFFS")) {
        DO_DIFFS = true;
      }

      // The Daikon arguments start with the first argument that is not a TraceSelect switch,
      // such as a .dtrace or .decls file, or "--nohierarchy" or "--format java".

      // -h and --help before the Daikon arguments print the usage message.
      else if (!knowArgStart
          && (args[i].equals("-h") || args[i].equals("--" + daikon.Daikon.help_SWITCH))) {
        System.out.println(usage);
        throw new daikon.Daikon.NormalTermination();
      }

      // For now, only the first dtrace file will be sampled
      else if (args[i].endsWith(".dtrace")) {
        if (inputFile == null) {
          inputFile = args[i];
        } else {
          throw new daikon.Daikon.UserError("Only 1 dtrace file for input allowed");
        }

        knowArgStart = true;
      } else {
        knowArgStart = true;
        daikonArgs.add(args[i]);
      }
    }

    // if no seed provided, use default Random() constructor
    randObj = (rand != null) ? rand : new Random();

    sampleNames = new String[num_reps + 1];
    sampleNames[0] = "-p";

    if (inputFile == null) {
      throw new daikon.Daikon.UserError(
          "No .dtrace file name specified (the trace file must be uncompressed and its name must"
              + " end with \".dtrace\")");
    }
    fileName = inputFile;

    try {

      // invokeBuffer = new ArrayList();
      // fileName = args[1];

      System.out.println("*******Processing********");

      // Have to call the DtraceNonceDoctor
      // to avoid the broken Dtrace from
      // using a command-line 'cat' that
      // results in repeat nonces
      /*
        String[] doctorArgs = new String[1];
        doctorArgs[0] = fileName;
        DtraceNonceDoctor.main (doctorArgs );
        Runtime.getRuntime().exec ("mv " + doctorArgs[0] + "_fixed " +
        doctorArgs[0]);
      */

      while (num_reps > 0) {

        List<String> al = new ArrayList<>();
        try (DtracePartitioner dec = new DtracePartitioner(fileName)) {
          MultiRandSelector<String> mrs = new MultiRandSelector<>(numPerSample, dec);

          while (dec.hasNext()) {
            mrs.accept(dec.next());
          }

          for (Iterator<String> i = mrs.valuesIter(); i.hasNext(); ) {
            al.add(i.next());
          }

          List<String> al_tmp = dec.patchValues(al, INCLUDE_UNRETURNED);
          al = al_tmp;
        }

        String filePrefix = calcOut(fileName);

        // must do num_reps - 1 because of "off by one"
        // but now add a '-p' in the front so it's all good
        sampleNames[num_reps] = filePrefix + ".inv";

        try (PrintWriter pwOut = new PrintWriter(FilesPlume.newBufferedFileWriter(filePrefix))) {
          for (String toPrint : al) {
            pwOut.println(toPrint);
          }
          pwOut.flush();
        }

        invokeDaikon(filePrefix);

        // Clean up the mess.  A failed cleanup is not fatal.
        if (CLEAN) {
          deleteQuietly(filePrefix);
        }

        num_reps--;
      }

      if (DO_DIFFS) {
        // histograms
        //  daikon.diff.Diff.main (sampleNames);

        // spinfo format
        daikon.diff.MultiDiff.main(sampleNames);
      }

      // Clean up the mess!  A failed cleanup is not fatal.
      // Start at index 1: sampleNames[0] is the "-p" sentinel, not a file.
      if (CLEAN) {
        for (int j = 1; j < sampleNames.length; j++) {
          deleteQuietly(sampleNames[j]);
        }
      }

    } catch (Exception e) {
      throw new Error(e);
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

  private static void invokeDaikon(String dtraceName) throws IOException {

    System.out.println("Created file: " + dtraceName);

    // this part adds on the rest of the decls files
    ArrayList<String> daikonArgsList = new ArrayList<>();
    daikonArgsList.add(dtraceName);
    daikonArgsList.add("-o");
    daikonArgsList.add(dtraceName + ".inv");

    // all the Daikon args except for the original single dtrace file
    daikonArgsList.addAll(daikonArgs);

    // create an array to store the Strings in daikonArgsList
    String[] daikonArgs = daikonArgsList.toArray(new String[0]);

    // initializes daikon again or else an exception is thrown
    reinitializeDaikon();
    daikon.Daikon.main(daikonArgs);
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

  private static void reinitializeDaikon() {
    daikon.Daikon.inv_file = null;
  }

  private static String calcOut(String strFileName) {
    StringBuilder product = new StringBuilder();
    int index = strFileName.indexOf('.');
    if (index >= 0) {
      product.append(strFileName.substring(0, index));
      product.append(num_reps);
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
