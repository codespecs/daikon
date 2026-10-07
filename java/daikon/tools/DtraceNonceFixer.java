// DtraceNonceFixer.java

package daikon.tools;

import static daikon.tools.nullness.NullnessUtil.castNonNull;

import daikon.FileIO;
import daikon.PptName;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UncheckedIOException;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
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
 * cat} command combining multiple dtrace files. Every dtrace file besides the first will have the
 * invocation nonces increased by the "correct" amount, determined in the following way:
 *
 * <p>Keep track of all the nonces you see and maintain a record of the highest nonce observed. The
 * next time you see a '0' valued nonce that is not part of an EXIT or THROWS program point, then
 * you know you have reached the beginning of the next dtrace file. Use that as the number to add to
 * the remaining nonces and repeat. This should only require one pass through the file.
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
          "is written to OUTFILE instead, and no other output file remains.",
          "OUTFILE may be the same as FILENAME.");

  /** Parsed program point names, indexed by their full names. */
  private static final Map<String, PptName> pptNames = new HashMap<>();

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
        // The temporary files are in OUTFILE's directory, so that the final move can be atomic,
        // and have fresh names so that they overwrite no existing file.  OUTFILE is not opened
        // for writing, so if this program fails, OUTFILE (which may be the input) is unchanged.
        Path outfile = Path.of(args[1]).toAbsolutePath();
        Path dir = castNonNull(outfile.getParent()); // an absolute file path has a parent
        String prefix = outfile.getFileName() + "-";
        String suffix = args[1].endsWith(".gz") ? ".gz" : "";
        Path tmpFile = Files.createTempFile(dir, prefix, "-fixed" + suffix);
        Path tmpOutFile = Files.createTempFile(dir, prefix, "-all-fixed" + suffix);
        try {
          int maxNonce = correctNonces(args[0], tmpFile.toString());
          addMissingNonces(tmpFile.toString(), tmpOutFile.toString(), maxNonce);
          replaceFile(tmpOutFile, outfile);
        } finally {
          Files.deleteIfExists(tmpFile);
          Files.deleteIfExists(tmpOutFile);
        }
      }
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  /**
   * Moves {@code source} to {@code target}, replacing {@code target} if it exists. If possible, the
   * move is atomic, and the result has the permissions of the original {@code target}.
   *
   * @param source the file to move
   * @param target the file to replace
   * @throws IOException if there is trouble moving the file
   */
  private static void replaceFile(Path source, Path target) throws IOException {
    if (Files.exists(target)) {
      try {
        Files.setPosixFilePermissions(source, Files.getPosixFilePermissions(target));
      } catch (UnsupportedOperationException e) {
        // The file system does not support POSIX permissions.
      }
    }
    try {
      Files.move(
          source, target, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
    } catch (AtomicMoveNotSupportedException e) {
      Files.move(source, target, StandardCopyOption.REPLACE_EXISTING);
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
      Paragraph para;
      while ((para = Paragraph.read(br)) != null) {
        int non = para.nonce;
        if (non == -1) {
          para.print(out);
          continue;
        }
        // The first legit 0 nonce will have an ENTER and EXIT
        // seeing a 0 means we have reached the next file
        if (non == 0 && !isExitOrThrows(para.pptName())) {
          if (first) {
            // on the first file, keep the first nonce as 0
            first = false;
          } else {
            correctionFactor = maxNonce + 1;
          }
        }
        int newNonce = non + correctionFactor;
        maxNonce = Math.max(maxNonce, newNonce);
        para.printWithNonce(out, newNonce);
      }
    }
    return maxNonce;
  }

  /**
   * Copies {@code inFilename} to {@code outFilename}, giving a nonce to each sample that lacks one,
   * such as an OBJECT or CLASS sample. Each new nonce is larger than {@code maxNonce}. An EXIT or
   * THROWS sample gets the nonce of the most recent unmatched ENTER sample for the same method,
   * like the call stack that Daikon uses for samples without nonces. The ENTER sample may have had
   * a nonce in the input.
   *
   * @param inFilename the dtrace file to read
   * @param outFilename the dtrace file to write
   * @param maxNonce the largest nonce in {@code inFilename}
   * @throws IOException if there is trouble reading or writing a file
   */
  private static void addMissingNonces(String inFilename, String outFilename, int maxNonce)
      throws IOException {
    CallStack callStack = new CallStack();
    try (BufferedReader br = FilesPlume.newBufferedFileReader(inFilename);
        PrintWriter out = new PrintWriter(FilesPlume.newBufferedFileWriter(outFilename))) {
      Paragraph para;
      while ((para = Paragraph.read(br)) != null) {
        if (!para.isSample()) {
          para.print(out);
          continue;
        }
        String pptName = para.pptName();
        // This is the same test that Daikon uses in FileIO.compute_orig_variables.
        boolean isEnter = pptName.endsWith(FileIO.enter_tag);
        boolean isExit = !isEnter && isExitOrThrows(pptName);
        if (para.nonce != -1) {
          if (isEnter) {
            callStack.push(methodName(pptName), para.nonce);
          } else if (isExit) {
            callStack.matchNonce(para.nonce);
          }
          para.print(out);
          continue;
        }
        int nonce;
        if (isEnter) {
          nonce = ++maxNonce;
          callStack.push(methodName(pptName), nonce);
        } else if (isExit) {
          Integer enterNonce = callStack.matchMethod(methodName(pptName));
          nonce = (enterNonce != null) ? enterNonce : ++maxNonce;
        } else {
          nonce = ++maxNonce;
        }
        para.printWithNonce(out, nonce);
      }
    }
  }

  /**
   * Returns the parsed form of the given program point name.
   *
   * @param pptName a program point name
   * @return the parsed form of {@code pptName}
   */
  private static PptName parsePptName(String pptName) {
    return pptNames.computeIfAbsent(pptName, PptName::new);
  }

  /**
   * Returns true if the given program point is an EXIT or THROWS point, which Daikon pairs with an
   * ENTER point.
   *
   * @param pptName a program point name
   * @return true if {@code pptName} is an EXIT or THROWS point
   */
  private static boolean isExitOrThrows(String pptName) {
    PptName parsed = parsePptName(pptName);
    return parsed.isExitPoint() || parsed.isThrowsPoint();
  }

  /**
   * Returns the given program point name, without the part starting at ":::".
   *
   * @param pptName a program point name
   * @return the method part of {@code pptName}
   */
  private static String methodName(String pptName) {
    // non-null because the PptName was created from a name that contains ":::"
    return castNonNull(parsePptName(pptName).getNameWithoutPoint());
  }

  /** A method call whose ENTER sample has been seen, but whose EXIT sample has not. */
  private static final class Call {
    /** The program point name, without the part starting at ":::". */
    final String method;

    /** The nonce of the ENTER sample. */
    final int nonce;

    /** True if an EXIT sample has been matched with this call. */
    boolean matched = false;

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
   * The ENTER samples that have not yet been matched by an EXIT sample. Each operation takes
   * amortized constant time.
   */
  private static final class CallStack {
    /**
     * The calls, most recent first. May contain matched calls (those that were matched by nonce
     * while a more recent call was unmatched); they are skipped.
     */
    private final Deque<Call> stack = new ArrayDeque<>();

    /** The number of unmatched calls in {@link #stack}, for each method. */
    private final Map<String, Integer> unmatchedCounts = new HashMap<>();

    /** The unmatched calls in {@link #stack}, indexed by nonce. */
    private final Map<Integer, Call> unmatchedByNonce = new HashMap<>();

    /**
     * Adds a call.
     *
     * @param method the program point name of the ENTER sample, without the part starting at ":::"
     * @param nonce the nonce of the ENTER sample
     */
    void push(String method, int nonce) {
      Call call = new Call(method, nonce);
      stack.push(call);
      unmatchedCounts.merge(method, 1, Integer::sum);
      unmatchedByNonce.put(nonce, call);
    }

    /**
     * Marks the call with the given nonce, if any, as matched.
     *
     * @param nonce the nonce of an EXIT sample
     */
    void matchNonce(int nonce) {
      Call call = unmatchedByNonce.get(nonce);
      if (call != null) {
        markMatched(call);
      }
      Call top;
      while ((top = stack.peek()) != null && top.matched) {
        stack.pop();
      }
    }

    /**
     * Removes the most recent unmatched call to {@code method}, along with all more recent calls
     * (which exited exceptionally), and returns its nonce. If there is no unmatched call to {@code
     * method}, returns null and leaves this unchanged.
     *
     * @param method the method of an EXIT sample
     * @return the nonce of the matching ENTER sample, or null if there is none
     */
    @Nullable Integer matchMethod(String method) {
      if (!unmatchedCounts.containsKey(method)) {
        return null;
      }
      while (true) {
        Call call = stack.pop();
        if (call.matched) {
          continue;
        }
        markMatched(call);
        if (call.method.equals(method)) {
          return call.nonce;
        }
      }
    }

    /**
     * Marks the given unmatched call as matched, but does not remove it from {@link #stack}.
     *
     * @param call an unmatched call
     */
    private void markMatched(Call call) {
      call.matched = true;
      unmatchedCounts.computeIfPresent(call.method, (m, count) -> (count == 1) ? null : count - 1);
      unmatchedByNonce.remove(call.nonce, call);
    }
  }

  /** A paragraph of a dtrace file: its lines, up to but not including a blank line. */
  private static final class Paragraph {
    /** The lines of the paragraph. */
    final List<String> lines;

    /**
     * The index of the program point name, if this paragraph is a sample; otherwise -1. Leading
     * lines are comments, which Daikon treats as a separate record.
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
      boolean isSample =
          i < lines.size()
              && lines.get(i).contains(FileIO.ppt_tag_separator)
              && !lines.get(i).startsWith("ppt ");
      this.pptNameIndex = isSample ? i : -1;
      this.nonce = isSample ? parseNonce(lines, i) : -1;
    }

    /**
     * Returns the nonce of the sample whose program point name is at index {@code i}, or -1 if the
     * sample has no nonce. The nonce header, if any, directly follows the program point name.
     *
     * @param lines the lines of a sample
     * @param i the index of the program point name in {@code lines}
     * @return the nonce of the sample, or -1
     */
    private static int parseNonce(List<String> lines, int i) {
      if (i + 1 >= lines.size() || !lines.get(i + 1).trim().equals(NONCE_HEADER)) {
        return -1;
      }
      if (i + 2 >= lines.size()) {
        throw new daikon.Daikon.UserError("No nonce after " + NONCE_HEADER + " in: " + lines);
      }
      String nonceString = lines.get(i + 2).trim();
      try {
        return Integer.parseInt(nonceString);
      } catch (NumberFormatException e) {
        throw new daikon.Daikon.UserError("Bad nonce \"" + nonceString + "\" in: " + lines);
      }
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
     * Returns the program point name of this sample.
     *
     * @return the program point name of this sample
     */
    String pptName() {
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
      // Daikon requires the header to be exactly NONCE_HEADER, without surrounding whitespace.
      int rest = (nonce == -1) ? pptNameIndex + 1 : pptNameIndex + 3;
      for (int i = 0; i <= pptNameIndex; i++) {
        out.println(lines.get(i));
      }
      out.println(NONCE_HEADER);
      out.println(newNonce);
      for (int i = rest; i < lines.size(); i++) {
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
