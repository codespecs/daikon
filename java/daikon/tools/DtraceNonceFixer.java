// DtraceNonceFixer.java

package daikon.tools;

import daikon.DaikonGetopt;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.StringTokenizer;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.plumelib.util.FilesPlume;
import org.plumelib.util.StringsPlume;

/**
 * This tool fixes a Dtrace file whose invocation nonces became inaccurate as a result of a {@code
 * cat} command combining multiple dtrace files. Every dtrace file besides the first will have the
 * invocation nonces increased by the "correct" amount, determined in the following way:
 *
 * <p>Keep track of all the nonces you see and maintain a record of the highest nonce observed. The
 * next time you see a '0' valued nonce that is not part of an EXIT program point, then you know you
 * have reached the beginning of the next dtrace file. Use that as the number to add to the
 * remaining nonces and repeat. This should only require one pass through the file.
 */
public class DtraceNonceFixer {

  /** Do not instantiate. */
  private DtraceNonceFixer() {
    throw new UnsupportedOperationException("Do not instantiate");
  }

  /** The system-specific line separator. */
  private static final String lineSep = System.lineSeparator();

  /** The usage message for this program. */
  private static String usage =
      StringsPlume.joinLines(
          "Usage: DtraceNonceFixer FILENAME [OUTFILE]",
          "Modifies dtrace file FILENAME so that the invocation nonces are consistent.",
          "The output file will be FILENAME_fixed and another output included",
          "nonces for OBJECT and CLASS invocations called FILENAME_all_fixed.",
          "If OUTFILE is supplied, the output that includes nonces for all invocations",
          "is written to OUTFILE instead, and no other output file remains.");

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
  public static void mainHelper(final String[] args) {
    String[] files = DaikonGetopt.nonOptionArgs(args, usage);
    if (files.length != 1 && files.length != 2) {
      throw new daikon.Daikon.UserError(usage);
    }

    // The base name of the output files, which determines whether they are compressed.
    String outputBase = (files.length == 2) ? files[1] : files[0];
    String outputFilename =
        outputBase.endsWith(".gz") ? (outputBase + "_fixed.gz") : (outputBase + "_fixed");

    // maxNonce - the biggest nonce ever found in the file
    int maxNonce = 0;

    // The intermediate file must be closed before it is read, so that its contents (including, for
    // a compressed file, the trailer) are complete.
    try (BufferedReader br1 = FilesPlume.newBufferedFileReader(files[0]);
        PrintWriter out1 = new PrintWriter(FilesPlume.newBufferedFileWriter(outputFilename))) {

      // correctionFactor - the amount to add to each observed nonce
      int correctionFactor = 0;
      boolean first = true;
      String nextInvo;
      while ((nextInvo = grabNextInvocation(br1)) != null) {
        int non = peekNonce(nextInvo);
        // The first legit 0 nonce will have an ENTER and EXIT
        // seeing a 0 means we have reached the next file
        if (non == 0 && nextInvo.indexOf("EXIT") == -1) {
          if (first) {
            // on the first file, keep the first nonce as 0
            first = false;
          } else {
            correctionFactor = maxNonce + 1;
          }
        }
        int newNonce = non + correctionFactor;
        maxNonce = Math.max(maxNonce, newNonce);
        if (non != -1) {
          out1.println(spawnWithNewNonce(nextInvo, newNonce));
        } else {
          out1.println(nextInvo);
        }
      }
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }

    // now go back and add the OBJECT and CLASS invocations
    String allFixedFilename;
    if (files.length == 2) {
      allFixedFilename = files[1];
    } else {
      allFixedFilename =
          outputFilename.endsWith(".gz") ? (files[0] + "_all_fixed.gz") : (files[0] + "_all_fixed");
    }

    try (BufferedReader br2 = FilesPlume.newBufferedFileReader(outputFilename);
        PrintWriter out2 = new PrintWriter(FilesPlume.newBufferedFileWriter(allFixedFilename))) {
      String nextInvo;
      while ((nextInvo = grabNextInvocation(br2)) != null) {
        int non = peekNonce(nextInvo);
        // if there is no nonce at this point it must be an OBJECT
        // or a CLASS invocation (or a sample without a nonce)
        if (non == -1 && isSample(nextInvo)) {
          out2.println(spawnWithNewNonce(nextInvo, ++maxNonce));
        } else {
          out2.println(nextInvo);
        }
      }
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }

    // The intermediate file is not needed when OUTFILE is supplied.
    if (files.length == 2) {
      try {
        Files.delete(Path.of(outputFilename));
      } catch (IOException e) {
        throw new UncheckedIOException(e);
      }
    }
  }

  /**
   * Returns a String representing an invocation with the line directly under
   * 'this_invocation_nonce' changed to 'newNone'. If the String 'this_invocation_nonce' is not
   * found, then creates a line 'this_invocation_nonce' directly below the program point name and a
   * line containing newNonce directly under that.
   */
  private static String spawnWithNewNonce(String invo, int newNonce) {

    //    System.out.println (invo);

    StringBuilder sb = new StringBuilder();
    StringTokenizer st = new StringTokenizer(invo, lineSep);

    if (!st.hasMoreTokens()) {
      return sb.toString();
    }

    // First line is the program point name
    sb.append(st.nextToken()).append(lineSep);

    // There is a chance that this is not really an invocation
    // but a EOF shutdown hook instead.
    if (!st.hasMoreTokens()) {
      return sb.toString();
    }

    // See if the second line is the nonce
    String line = st.nextToken();
    if (line.trim().equals("this_invocation_nonce")) {
      // modify the next line to include the new nonce
      sb.append(line).append(lineSep).append(newNonce).append(lineSep);
      // throw out the next token, because it will be the old nonce
      st.nextToken();
    } else {
      // otherwise create the required this_invocation_nonce line, then retain `line`, which is
      // the name of the first variable
      sb.append("this_invocation_nonce" + lineSep).append(newNonce).append(lineSep);
      sb.append(line).append(lineSep);
    }

    while (st.hasMoreTokens()) {
      sb.append(st.nextToken()).append(lineSep);
    }

    return sb.toString();
  }

  /**
   * Returns true if the given paragraph of a dtrace file is a sample, as opposed to a declaration,
   * a comment, or other information.
   *
   * @param invo a paragraph of a dtrace file
   * @return true if {@code invo} is a sample
   */
  private static boolean isSample(String invo) {
    int lineEnd = invo.indexOf(lineSep);
    String firstLine = (lineEnd == -1) ? invo : invo.substring(0, lineEnd);
    return firstLine.contains(":::") && !firstLine.startsWith("ppt ");
  }

  /**
   * Returns the nonce of the invocation 'invo', or -1 if the String 'this_invocation_nonce' is not
   * found in {@code invo}.
   */
  private static int peekNonce(String invo) {
    StringTokenizer st = new StringTokenizer(invo, lineSep);
    while (st.hasMoreTokens()) {
      String line = st.nextToken();
      if (line.trim().equals("this_invocation_nonce")) {
        return Integer.parseInt(st.nextToken().trim());
      }
    }
    return -1;
  }

  /**
   * Grabs the next invocation out of the dtrace buffer and returns a String with endline characters
   * preserved. This method will return a single blank line if the original dtrace file contained
   * consecutive blank lines. Leading whitespace, which is significant in declarations, is
   * preserved.
   *
   * @param br the reader for the dtrace file
   * @return the next invocation, or null if the end of the file has been reached
   */
  private static @Nullable String grabNextInvocation(BufferedReader br) throws IOException {
    StringBuilder sb = new StringBuilder();
    String line;
    while ((line = br.readLine()) != null) {
      line = line.stripTrailing();
      if (line.isEmpty()) {
        return sb.toString();
      }
      sb.append(line).append(lineSep);
    }
    // End of file
    return (sb.length() == 0) ? null : sb.toString();
  }
}
