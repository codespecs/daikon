// DtraceNonceFixer.java

package daikon.tools;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UncheckedIOException;
import java.util.StringTokenizer;
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

  /** The system-specific line separator. */
  private static final String lineSep = System.lineSeparator();

  /** The usage message for this program. */
  private static String usage =
      StringsPlume.joinLines(
          "Usage: DtraceNonceFixer FILENAME",
          "Modifies dtrace file FILENAME so that the invocation nonces are consistent.",
          "The output file will be FILENAME_fixed and another output included",
          "nonces for OBJECT and CLASS invocations called FILENAME_all_fixed");

  public static void main(String[] args) {
    try {
      mainHelper(args);
    } catch (daikon.Daikon.DaikonTerminationException e) {
      daikon.Daikon.handleDaikonTerminationException(e);
    }
  }

  /**
   * This does the work of {@link #main(String[])}, but it never calls System.exit, so it is
   * appropriate to be called progrmmatically.
   *
   * @param args command-line arguments, like those of {@link #main}
   */
  public static void mainHelper(final String[] args) {
    if (args.length != 1) {
      throw new daikon.Daikon.UserError(usage);
    }

    String outputFilename =
        args[0].endsWith(".gz") ? (args[0] + "_fixed.gz") : (args[0] + "_fixed");

    try (BufferedReader br1 = FilesPlume.newBufferedFileReader(args[0]);
        PrintWriter out1 = new PrintWriter(FilesPlume.newBufferedFileWriter(outputFilename))) {

      // maxNonce - the biggest nonce ever found in the file
      // correctionFactor - the amount to add to each observed nonce
      int maxNonce = 0;
      int correctionFactor = 0;
      boolean first = true;
      while (br1.ready()) {
        String nextInvo = grabNextInvocation(br1);
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
      out1.flush();

      // now go back and add the OBJECT and CLASS invocations
      String allFixedFilename =
          outputFilename.endsWith(".gz") ? (args[0] + "_all_fixed.gz") : (args[0] + "_all_fixed");

      try (BufferedReader br2 = FilesPlume.newBufferedFileReader(outputFilename);
          PrintWriter out2 = new PrintWriter(FilesPlume.newBufferedFileWriter(allFixedFilename))) {

        while (br2.ready()) {
          String nextInvo = grabNextInvocation(br2);
          int non = peekNonce(nextInvo);
          // if there is no nonce at this point it must be an OBJECT
          // or a CLASS invocation
          if (non == -1) {
            out2.println(spawnWithNewNonce(nextInvo, ++maxNonce));
          } else {
            out2.println(nextInvo);
          }
        }

        out2.flush();
      }
    } catch (IOException e) {
      throw new UncheckedIOException(e);
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
    if (line.equals("this_invocation_nonce")) {
      // modify the next line to include the new nonce
      sb.append(line).append(lineSep).append(newNonce).append(lineSep);
      // throw out the next token, because it will be the old nonce
      st.nextToken();
    } else {
      // otherwise create the required this_invocation_nonce line
      sb.append("this_invocation_nonce" + lineSep).append(newNonce).append(lineSep);
    }

    while (st.hasMoreTokens()) {
      sb.append(st.nextToken()).append(lineSep);
    }

    return sb.toString();
  }

  /**
   * Returns the nonce of the invocation 'invo', or -1 if the String 'this_invocation_nonce' is not
   * found in {@code invo}.
   */
  private static int peekNonce(String invo) {
    StringTokenizer st = new StringTokenizer(invo, lineSep);
    while (st.hasMoreTokens()) {
      String line = st.nextToken();
      if (line.equals("this_invocation_nonce")) {
        return Integer.parseInt(st.nextToken());
      }
    }
    return -1;
  }

  /**
   * Grabs the next invocation out of the dtrace buffer and returns a String with endline characters
   * preserved. This method will return a single blank line if the original dtrace file contained
   * consecutive blank lines.
   */
  private static String grabNextInvocation(BufferedReader br) throws IOException {
    StringBuilder sb = new StringBuilder();
    while (br.ready()) {
      String line = br.readLine();
      assert line != null; // because br.ready() = true
      line = line.trim();
      if (line.equals("")) {
        break;
      }
      sb.append(line).append(lineSep);
    }
    return sb.toString();
  }
}
