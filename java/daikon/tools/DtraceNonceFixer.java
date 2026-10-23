// DtraceNonceFixer.java

package daikon.tools;

import static daikon.tools.nullness.NullnessUtil.castNonNull;

import daikon.DaikonGetopt;
import daikon.FileIO;
import daikon.PptName;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UncheckedIOException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.plumelib.util.FilesPlume;
import org.plumelib.util.StringsPlume;

/**
 * This tool fixes a Dtrace file whose invocation nonces became inaccurate as a result of a {@code
 * cat} command combining multiple dtrace files. It renumbers the nonces of each concatenated file
 * after the first so that they do not collide with nonces of earlier files, and it gives a nonce to
 * each sample that lacks one. An EXIT or THROWS sample gets the same nonce as its ENTER sample,
 * which is found the same way that Daikon pairs samples without nonces.
 */
public class DtraceNonceFixer {

  /** Do not instantiate. */
  private DtraceNonceFixer() {
    throw new UnsupportedOperationException("Do not instantiate");
  }

  /** The usage message for this program. */
  private static String usage =
      StringsPlume.joinLines(
          "Usage: DtraceNonceFixer FILENAME",
          "Modifies dtrace file FILENAME so that the invocation nonces are consistent.",
          "Output file FILENAME_fixed renumbers the nonces of concatenated dtrace files",
          "so that they do not collide.  Output file FILENAME_all_fixed also gives a nonce",
          "to each sample that lacks one; an EXIT or THROWS sample gets the nonce of its",
          "ENTER sample.");

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
   * @param args command-line arguments: a single dtrace file, or {@code -h} or {@code --help}
   */
  public static void mainHelper(final String[] args) {
    String[] files = DaikonGetopt.nonOptionArgs(args, usage);
    if (files.length != 1) {
      throw new daikon.Daikon.UserError(usage);
    }

    String suffix = files[0].endsWith(".gz") ? ".gz" : "";
    String fixedFilename = files[0] + "_fixed" + suffix;
    try {
      // First pass: renumber the nonces of each concatenated file after the first.
      int maxNonce = renumberNonces(files[0], fixedFilename);
      // Second pass: give a new nonce to each sample that lacks one, such as an OBJECT or CLASS
      // sample.
      addMissingNonces(fixedFilename, files[0] + "_all_fixed" + suffix, maxNonce);
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
  private static int renumberNonces(String inFilename, String outFilename) throws IOException {
    // maxNonce - the biggest nonce ever found in the file
    int maxNonce = 0;
    try (BufferedReader br = FilesPlume.newBufferedFileReader(inFilename);
        BufferedWriter bw = FilesPlume.newBufferedFileWriter(outFilename)) {
      PrintWriter out = new PrintWriter(bw);

      // correctionFactor - the amount to add to each observed nonce
      int correctionFactor = 0;
      // seenNonce - whether any sample with a nonce has been seen
      boolean seenNonce = false;
      Paragraph para;
      while ((para = Paragraph.read(br)) != null) {
        int oldNonce = para.nonce;
        if (oldNonce == -1) {
          para.print(out);
          continue;
        }
        // A 0 nonce that is not an EXIT or THROWS, after other nonces, means we have reached the
        // next file. The first file's nonces are unchanged, even if that file does not start at 0.
        if (oldNonce == 0 && seenNonce && !isExitOrThrows(new PptName(para.pptNameLine()))) {
          correctionFactor = addNonces(maxNonce, 1);
        }
        seenNonce = true;
        int newNonce = addNonces(oldNonce, correctionFactor);
        para.printWithNonce(out, newNonce);
        maxNonce = Math.max(maxNonce, newNonce);
      }
      throwIfError(out, outFilename);
    }
    return maxNonce;
  }

  /**
   * Copies {@code inFilename} to {@code outFilename}, giving a nonce to each sample that lacks one,
   * such as an OBJECT or CLASS sample. Each new nonce is larger than {@code maxNonce}. An EXIT or
   * THROWS sample gets the nonce of the most recent unmatched ENTER sample for the same method,
   * like the call stack that Daikon uses for samples without nonces. As in Daikon, an ENTER sample
   * that has a nonce in the input is not paired with an EXIT or THROWS sample that lacks one.
   *
   * @param inFilename the dtrace file to read
   * @param outFilename the dtrace file to write
   * @param maxNonce the largest nonce in {@code inFilename}
   * @throws IOException if there is trouble reading or writing a file
   */
  private static void addMissingNonces(String inFilename, String outFilename, int maxNonce)
      throws IOException {
    CallStack callStack = new CallStack();
    // The parsed program point names, so that each distinct name is parsed only once.
    Map<String, PptName> pptNames = new HashMap<>();
    try (BufferedReader br = FilesPlume.newBufferedFileReader(inFilename);
        BufferedWriter bw = FilesPlume.newBufferedFileWriter(outFilename)) {
      PrintWriter out = new PrintWriter(bw);
      Paragraph para;
      while ((para = Paragraph.read(br)) != null) {
        if (!para.isSample() || para.nonce != -1) {
          para.print(out);
          continue;
        }
        PptName pptName = pptNames.computeIfAbsent(para.pptNameLine(), PptName::new);
        // This is the same test that Daikon uses in FileIO.compute_orig_variables.
        boolean isEnter = pptName.name().endsWith(FileIO.enter_tag);
        boolean isExit = isExitOrThrows(pptName);
        Integer enterNonce = isExit ? callStack.matchMethod(methodName(pptName)) : null;
        int nonce;
        if (enterNonce != null) {
          nonce = enterNonce;
        } else {
          maxNonce = addNonces(maxNonce, 1);
          nonce = maxNonce;
          if (isEnter) {
            callStack.push(methodName(pptName), nonce);
          }
        }
        para.printWithNonce(out, nonce);
      }
      throwIfError(out, outFilename);
    }
  }

  /**
   * Flushes {@code out} and throws an exception if {@code out} has encountered an error. A
   * PrintWriter records errors rather than throwing IOException. Therefore, a caller should make
   * the PrintWriter's underlying writer, not the PrintWriter, the try-with-resources resource, so
   * that closing it (which, for a compressed file, writes the trailer) throws IOException on
   * failure.
   *
   * @param out the PrintWriter to check
   * @param filename the file that {@code out} writes, for the error message
   * @throws IOException if {@code out} has encountered an error
   */
  private static void throwIfError(PrintWriter out, String filename) throws IOException {
    if (out.checkError()) {
      throw new IOException("Error writing " + filename);
    }
  }

  /**
   * Returns {@code a + b}.
   *
   * @param a a nonce or an amount to add to a nonce
   * @param b a nonce or an amount to add to a nonce
   * @return {@code a + b}
   * @throws daikon.Daikon.UserError if {@code a + b} exceeds the range of an int
   */
  private static int addNonces(int a, int b) {
    try {
      return Math.addExact(a, b);
    } catch (ArithmeticException e) {
      throw new daikon.Daikon.UserError("Nonce exceeds " + Integer.MAX_VALUE);
    }
  }

  /**
   * Returns true if the given program point is an EXIT or THROWS point, which Daikon pairs with an
   * ENTER point.
   *
   * @param pptName a program point name
   * @return true if {@code pptName} is an EXIT or THROWS point
   */
  private static boolean isExitOrThrows(PptName pptName) {
    return pptName.isExitPoint() || pptName.isThrowsPoint();
  }

  /**
   * Returns the given program point name, without the part starting at ":::".
   *
   * @param pptName a program point name
   * @return the method part of {@code pptName}
   */
  private static String methodName(PptName pptName) {
    // The PptName(String) constructor always sets the name without the point.
    return castNonNull(pptName.getNameWithoutPoint());
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
   * The ENTER samples that have not yet been matched by an EXIT sample, and that will not be
   * matched by nonce. Each operation takes amortized constant time.
   */
  private static final class CallStack {
    /** The calls, most recent first. */
    private final Deque<Call> stack = new ArrayDeque<>();

    /** The number of calls in {@link #stack}, for each method. */
    private final Map<String, Integer> counts = new HashMap<>();

    /**
     * Adds a call.
     *
     * @param method the program point name of the ENTER sample, without the part starting at ":::"
     * @param nonce the nonce of the ENTER sample
     */
    void push(String method, int nonce) {
      stack.push(new Call(method, nonce));
      counts.merge(method, 1, Integer::sum);
    }

    /**
     * Removes the most recent call to {@code method}, along with all more recent calls (which
     * exited exceptionally), and returns its nonce. If there is no call to {@code method}, returns
     * null and leaves this unchanged.
     *
     * @param method the method of an EXIT sample
     * @return the nonce of the matching ENTER sample, or null if there is none
     */
    @Nullable Integer matchMethod(String method) {
      if (!counts.containsKey(method)) {
        return null;
      }
      while (true) {
        Call call = stack.pop();
        counts.computeIfPresent(
            call.method,
            (m, count) -> {
              if (count == 1) {
                return null;
              }
              return count - 1;
            });
        if (call.method.equals(method)) {
          return call.nonce;
        }
      }
    }
  }

  /** A paragraph of a dtrace file: its lines, up to but not including a blank line. */
  private static final class Paragraph {
    /** The lines of the paragraph. */
    final List<String> lines;

    /**
     * The index of the program point name, if this paragraph is a sample; otherwise -1. Leading
     * lines are comments, which Daikon treats as a separate record. A paragraph that consists of
     * only a program point name is a sample, because Daikon reads it as a sample of a program point
     * that has no variables (such as the entry to a static method with no parameters).
     */
    final int pptNameIndex;

    /** The nonce, or -1 if this paragraph is not a sample or has no nonce. */
    final int nonce;

    /**
     * Creates a new Paragraph.
     *
     * @param lines the lines of the paragraph
     */
    Paragraph(List<String> lines) {
      this.lines = lines;
      int i = 0;
      while (i < lines.size() && FileIO.isComment(lines.get(i))) {
        i++;
      }
      // A paragraph is a sample if its first non-comment line contains ":::" and is not the
      // header of another kind of record.  Like Daikon (see FileIO.read_data_trace_record), check
      // for a record header first.  The first line of every other kind of record (such as
      // "DECLARE" or "VarComparability") lacks ":::", and Daikon rejects a sample whose program
      // point name lacks ":::" (see the PptName constructor).
      boolean isSample =
          i < lines.size()
              && !isRecordHeader(lines.get(i))
              && lines.get(i).contains(FileIO.ppt_tag_separator);
      this.pptNameIndex = isSample ? i : -1;
      this.nonce = isSample ? parseNonce(lines, i) : -1;
    }

    /**
     * Returns true if {@code line} starts a record, other than a sample, whose first line may
     * contain ":::".
     *
     * @param line the first non-comment line of a paragraph
     * @return true if {@code line} starts a record that is not a sample
     */
    private static boolean isRecordHeader(String line) {
      return line.startsWith("decl-version")
          || line.startsWith("input-language")
          || line.startsWith("ppt ")
          || line.startsWith("var-comparability");
    }

    /**
     * Returns the nonce of the sample whose program point name is {@code lines.get(i)}, or -1 if
     * the sample has no nonce. The nonce header, if any, directly follows the program point name.
     *
     * @param lines the lines of a paragraph that is a sample, possibly starting with comment lines
     * @param i the index in {@code lines} of the program point name
     * @return the nonce of the sample, or -1
     */
    private static int parseNonce(List<String> lines, int i) {
      // Like Daikon, require the header to be exactly NONCE_HEADER.
      if (i + 1 >= lines.size() || !lines.get(i + 1).equals(FileIO.NONCE_HEADER)) {
        return -1;
      }
      if (i + 2 >= lines.size()) {
        throw new daikon.Daikon.UserError(
            "No nonce after " + FileIO.NONCE_HEADER + " in: " + lines);
      }
      String nonceString = lines.get(i + 2).trim();
      int nonce;
      try {
        nonce = Integer.parseInt(nonceString);
      } catch (NumberFormatException e) {
        nonce = -1;
      }
      if (nonce < 0) {
        throw new daikon.Daikon.UserError("Bad nonce \"" + nonceString + "\" in: " + lines);
      }
      return nonce;
    }

    /**
     * Returns true if this paragraph is a sample, as opposed to a declaration, a comment, or other
     * information.
     *
     * @return true if this paragraph is a sample
     */
    boolean isSample() {
      return pptNameIndex != -1;
    }

    /**
     * Returns the program point name line of this sample.
     *
     * @return the program point name line of this sample
     */
    String pptNameLine() {
      return lines.get(pptNameIndex);
    }

    /**
     * Prints this paragraph followed by a blank line.
     *
     * @param out where to print
     */
    void print(PrintWriter out) {
      for (String line : lines) {
        out.println(line);
      }
      out.println();
    }

    /**
     * Prints this sample, with nonce {@code newNonce}, followed by a blank line. If this sample has
     * a nonce, it is replaced; otherwise the nonce header and nonce are inserted directly below the
     * program point name.
     *
     * @param out where to print
     * @param newNonce the nonce to print
     */
    void printWithNonce(PrintWriter out, int newNonce) {
      int postNonceIndex = (nonce == -1) ? pptNameIndex + 1 : pptNameIndex + 3;
      for (int i = 0; i <= pptNameIndex; i++) {
        out.println(lines.get(i));
      }
      out.println(FileIO.NONCE_HEADER);
      out.println(newNonce);
      for (int i = postNonceIndex; i < lines.size(); i++) {
        out.println(lines.get(i));
      }
      out.println();
    }

    /**
     * Returns the next paragraph of the dtrace file. Like Daikon, this method treats only an empty
     * line as a paragraph separator; a line that contains only whitespace is part of a paragraph.
     * This method returns an empty paragraph if the dtrace file contains consecutive empty lines.
     * Lines are not modified, so the output differs from the input only in the nonce header line,
     * in the nonces, and in a final empty line (which the output always has).
     *
     * @param br the reader for the dtrace file
     * @return the next paragraph, or null if the end of the file has been reached
     * @throws IOException if there is trouble reading the file
     */
    static @Nullable Paragraph read(BufferedReader br) throws IOException {
      List<String> result = new ArrayList<>();
      String line;
      while ((line = br.readLine()) != null) {
        if (line.isEmpty()) {
          return new Paragraph(result);
        }
        result.add(line);
      }
      // End of file
      return result.isEmpty() ? null : new Paragraph(result);
    }
  }
}
