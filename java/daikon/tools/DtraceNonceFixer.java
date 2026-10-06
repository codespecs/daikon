// DtraceNonceFixer.java

package daikon.tools;

import static daikon.tools.nullness.NullnessUtil.castNonNull;

import daikon.FileIO;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
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
 *
 * <p>A second pass gives a nonce to each sample that lacks one. An EXIT sample gets the same nonce
 * as its ENTER sample, which is found the same way that Daikon pairs samples without nonces.
 */
public class DtraceNonceFixer {

  /** Do not instantiate. */
  private DtraceNonceFixer() {
    throw new UnsupportedOperationException("Do not instantiate");
  }

  /** The line that precedes the nonce in a sample. */
  private static final String NONCE_HEADER = "this_invocation_nonce";

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
    if (args.length != 1 && args.length != 2) {
      throw new daikon.Daikon.UserError(usage);
    }

    try {
      if (args.length == 1) {
        String suffix = args[0].endsWith(".gz") ? ".gz" : "";
        String fixedFilename = args[0] + "_fixed" + suffix;
        int maxNonce = correctNonces(args[0], fixedFilename);
        addMissingNonces(fixedFilename, args[0] + "_all_fixed" + suffix, maxNonce);
      } else {
        // The intermediate file is in OUTFILE's directory, and has a fresh name so that it
        // overwrites no existing file.
        Path outfile = Path.of(args[1]).toAbsolutePath();
        Path tmpFile =
            Files.createTempFile(
                castNonNull(outfile.getParent()), // an absolute file path has a parent
                outfile.getFileName() + "-",
                args[1].endsWith(".gz") ? "-fixed.gz" : "-fixed");
        try {
          int maxNonce = correctNonces(args[0], tmpFile.toString());
          addMissingNonces(tmpFile.toString(), args[1], maxNonce);
        } finally {
          Files.deleteIfExists(tmpFile);
        }
      }
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  /**
   * Copies {@code inFilename} to {@code outFilename}, increasing the nonces of each concatenated
   * dtrace file after the first so that they do not collide with earlier nonces. The output file is
   * closed when this method returns, so it is complete (including, for a compressed file, the
   * trailer) and may be read.
   *
   * @param inFilename the dtrace file to read
   * @param outFilename the dtrace file to write
   * @return the largest nonce in the output
   * @throws IOException if there is trouble reading or writing a file
   */
  private static int correctNonces(String inFilename, String outFilename) throws IOException {
    // maxNonce - the biggest nonce ever found in the file
    int maxNonce = 0;
    try (BufferedReader br = FilesPlume.newBufferedFileReader(inFilename);
        PrintWriter out = new PrintWriter(FilesPlume.newBufferedFileWriter(outFilename))) {

      // correctionFactor - the amount to add to each observed nonce
      int correctionFactor = 0;
      boolean first = true;
      List<String> para;
      while ((para = grabNextParagraph(br)) != null) {
        int non = peekNonce(para);
        if (non == -1) {
          printParagraph(out, para);
          continue;
        }
        // The first legit 0 nonce will have an ENTER and EXIT
        // seeing a 0 means we have reached the next file
        if (non == 0 && !para.get(pptNameIndex(para)).contains(FileIO.exit_tag)) {
          if (first) {
            // on the first file, keep the first nonce as 0
            first = false;
          } else {
            correctionFactor = maxNonce + 1;
          }
        }
        int newNonce = non + correctionFactor;
        maxNonce = Math.max(maxNonce, newNonce);
        printParagraph(out, withNonce(para, newNonce));
      }
    }
    return maxNonce;
  }

  /**
   * Copies {@code inFilename} to {@code outFilename}, giving a nonce to each sample that lacks one,
   * such as an OBJECT or CLASS sample. Each new nonce is larger than {@code maxNonce}. An EXIT or
   * THROWS sample gets the nonce of the most recent unmatched ENTER sample for the same method,
   * like the call stack that Daikon uses for samples without nonces.
   *
   * @param inFilename the dtrace file to read
   * @param outFilename the dtrace file to write
   * @param maxNonce the largest nonce in {@code inFilename}
   * @throws IOException if there is trouble reading or writing a file
   */
  private static void addMissingNonces(String inFilename, String outFilename, int maxNonce)
      throws IOException {
    // The ENTER samples that have not yet been matched by an EXIT sample, most recent first.
    Deque<Call> callStack = new ArrayDeque<>();
    try (BufferedReader br = FilesPlume.newBufferedFileReader(inFilename);
        PrintWriter out = new PrintWriter(FilesPlume.newBufferedFileWriter(outFilename))) {
      List<String> para;
      while ((para = grabNextParagraph(br)) != null) {
        if (!isSample(para) || peekNonce(para) != -1) {
          printParagraph(out, para);
          continue;
        }
        String pptName = para.get(pptNameIndex(para));
        int sepIndex = pptName.indexOf(FileIO.ppt_tag_separator);
        String method = pptName.substring(0, sepIndex);
        String point = pptName.substring(sepIndex + FileIO.ppt_tag_separator.length());
        int nonce;
        if (point.startsWith(FileIO.enter_suffix)) {
          nonce = ++maxNonce;
          callStack.push(new Call(method, nonce));
        } else if (point.startsWith(FileIO.exit_suffix) || point.startsWith(FileIO.throws_suffix)) {
          Integer enterNonce = popCall(callStack, method);
          nonce = (enterNonce != null) ? enterNonce : ++maxNonce;
        } else {
          nonce = ++maxNonce;
        }
        printParagraph(out, withNonce(para, nonce));
      }
    }
  }

  /** A method call whose ENTER sample has been seen, but whose EXIT sample has not. */
  private static final class Call {
    /** The program point name, without the part starting at ":::". */
    final String method;

    /** The nonce of the ENTER sample. */
    final int nonce;

    /**
     * Creates a new Call.
     *
     * @param method the program point name, without the part starting at ":::"
     * @param nonce the nonce of the ENTER sample
     */
    Call(String method, int nonce) {
      this.method = method;
      this.nonce = nonce;
    }
  }

  /**
   * Removes the most recent call to {@code method} from {@code callStack}, along with all more
   * recent calls (which exited exceptionally), and returns its nonce. If {@code callStack} has no
   * call to {@code method}, returns null and leaves {@code callStack} unchanged.
   *
   * @param callStack the unmatched ENTER samples, most recent first
   * @param method the method of an EXIT sample
   * @return the nonce of the matching ENTER sample, or null if there is none
   */
  private static @Nullable Integer popCall(Deque<Call> callStack, String method) {
    if (callStack.stream().noneMatch(call -> call.method.equals(method))) {
      return null;
    }
    Call call;
    do {
      call = callStack.pop();
    } while (!call.method.equals(method));
    return call.nonce;
  }

  /**
   * Returns the index of the first line of {@code para} that is not a comment. Daikon treats
   * leading comment lines as a separate record, so that line is the first line of a sample or
   * declaration.
   *
   * @param para a paragraph of a dtrace file
   * @return the index of the first non-comment line, or {@code para.size()} if there is none
   */
  private static int pptNameIndex(List<String> para) {
    int i = 0;
    while (i < para.size() && FileIO.isComment(para.get(i))) {
      i++;
    }
    return i;
  }

  /**
   * Returns true if the given paragraph of a dtrace file is a sample, as opposed to a declaration,
   * a comment, or other information.
   *
   * @param para a paragraph of a dtrace file
   * @return true if {@code para} is a sample
   */
  private static boolean isSample(List<String> para) {
    int i = pptNameIndex(para);
    if (i == para.size()) {
      return false;
    }
    String firstLine = para.get(i);
    return firstLine.contains(FileIO.ppt_tag_separator) && !firstLine.startsWith("ppt ");
  }

  /**
   * Returns the nonce of the sample {@code para}, or -1 if {@code para} is not a sample or has no
   * nonce. The nonce header, if any, directly follows the program point name.
   *
   * @param para a paragraph of a dtrace file
   * @return the nonce of {@code para}, or -1
   */
  private static int peekNonce(List<String> para) {
    if (!isSample(para)) {
      return -1;
    }
    int i = pptNameIndex(para);
    if (i + 1 >= para.size() || !para.get(i + 1).trim().equals(NONCE_HEADER)) {
      return -1;
    }
    if (i + 2 >= para.size()) {
      throw new daikon.Daikon.UserError("No nonce after " + NONCE_HEADER + " in: " + para);
    }
    String nonceString = para.get(i + 2).trim();
    try {
      return Integer.parseInt(nonceString);
    } catch (NumberFormatException e) {
      throw new daikon.Daikon.UserError("Bad nonce \"" + nonceString + "\" in: " + para);
    }
  }

  /**
   * Returns a copy of the sample {@code para} whose nonce is {@code newNonce}. If {@code para} has
   * a nonce, it is replaced; otherwise the nonce header and nonce are inserted directly below the
   * program point name.
   *
   * @param para a sample from a dtrace file
   * @param newNonce the nonce for the result
   * @return a copy of {@code para} with nonce {@code newNonce}
   */
  private static List<String> withNonce(List<String> para, int newNonce) {
    boolean hasNonce = peekNonce(para) != -1;
    List<String> result = new ArrayList<>(para);
    int i = pptNameIndex(para);
    if (hasNonce) {
      // Daikon requires the header to be exactly NONCE_HEADER, without surrounding whitespace.
      result.set(i + 1, NONCE_HEADER);
      result.set(i + 2, Integer.toString(newNonce));
    } else {
      result.add(i + 1, NONCE_HEADER);
      result.add(i + 2, Integer.toString(newNonce));
    }
    return result;
  }

  /**
   * Prints a paragraph followed by a blank line.
   *
   * @param out where to print
   * @param para the lines of the paragraph
   */
  private static void printParagraph(PrintWriter out, List<String> para) {
    for (String line : para) {
      out.println(line);
    }
    out.println();
  }

  /**
   * Returns the lines of the next paragraph of the dtrace file. This method will return an empty
   * list if the original dtrace file contained consecutive blank lines. Leading whitespace is
   * preserved, so that the output differs from the input only in nonces.
   *
   * @param br the reader for the dtrace file
   * @return the lines of the next paragraph, or null if the end of the file has been reached
   */
  private static @Nullable List<String> grabNextParagraph(BufferedReader br) throws IOException {
    List<String> result = new ArrayList<>();
    String line;
    while ((line = br.readLine()) != null) {
      line = line.stripTrailing();
      if (line.isEmpty()) {
        return result;
      }
      result.add(line);
    }
    // End of file
    return result.isEmpty() ? null : result;
  }
}
