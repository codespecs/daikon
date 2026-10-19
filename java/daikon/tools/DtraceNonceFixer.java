// DtraceNonceFixer.java

package daikon.tools;

import static daikon.tools.nullness.NullnessUtil.castNonNull;

import daikon.DaikonGetopt;
import daikon.FileIO;
import daikon.PptName;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UncheckedIOException;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
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
 * <p>A second pass gives a new nonce to each sample that lacks one, such as an OBJECT or CLASS
 * sample.
 */
public class DtraceNonceFixer {

  /** Do not instantiate. */
  private DtraceNonceFixer() {
    throw new UnsupportedOperationException("Do not instantiate");
  }

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

    try {
      if (files.length == 1) {
        String suffix = files[0].endsWith(".gz") ? ".gz" : "";
        String fixedFilename = files[0] + "_fixed" + suffix;
        int maxNonce = correctNonces(files[0], fixedFilename);
        addMissingNonces(fixedFilename, files[0] + "_all_fixed" + suffix, maxNonce);
      } else {
        // The temporary files are in OUTFILE's directory, so that the final move can be atomic,
        // and have fresh names so that they overwrite no existing file.  OUTFILE is not opened
        // for writing, so if this program fails, OUTFILE (which may be the input) is unchanged.
        // If OUTFILE is a symbolic link, the file that it refers to is replaced.
        Path outfile = Path.of(files[1]).toAbsolutePath();
        if (Files.exists(outfile)) {
          outfile = outfile.toRealPath();
        } else if (Files.isSymbolicLink(outfile)) {
          throw new daikon.Daikon.UserError(
              "OUTFILE is a symbolic link that does not refer to an existing file: " + files[1]);
        }
        Path dir = castNonNull(outfile.getParent()); // an absolute file path has a parent
        String prefix = outfile.getFileName() + "-";
        String suffix = files[1].endsWith(".gz") ? ".gz" : "";
        // The intermediate file is not compressed, because it is deleted.
        Path tmpFile = createFreshFile(dir, prefix, "-fixed");
        try {
          Path tmpOutFile = createFreshFile(dir, prefix, "-all-fixed" + suffix);
          try {
            int maxNonce = correctNonces(files[0], tmpFile.toString());
            addMissingNonces(tmpFile.toString(), tmpOutFile.toString(), maxNonce);
            // Delete before replacing OUTFILE, so that no failure occurs after OUTFILE changes.
            Files.delete(tmpFile);
            replaceFile(tmpOutFile, outfile);
          } catch (Throwable e) {
            deleteAfterFailure(tmpOutFile, e);
            throw e;
          }
        } catch (Throwable e) {
          deleteAfterFailure(tmpFile, e);
          throw e;
        }
      }
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  /**
   * Creates a new, empty file whose name does not conflict with any existing file. Unlike {@link
   * Files#createTempFile}, which makes the file readable only by its owner, this method gives the
   * file the default permissions (which depend on the umask).
   *
   * @param dir the directory in which to create the file
   * @param prefix the start of the file name
   * @param suffix the end of the file name
   * @return the new file
   * @throws IOException if there is trouble creating the file
   */
  private static Path createFreshFile(Path dir, String prefix, String suffix) throws IOException {
    for (int i = 0; ; i++) {
      try {
        return Files.createFile(dir.resolve(prefix + i + suffix));
      } catch (FileAlreadyExistsException e) {
        // Try the next name.
      }
    }
  }

  /**
   * Deletes {@code file}, if it exists, after a failure. A problem deleting the file is added to
   * {@code failure} as a suppressed exception, so that it does not hide {@code failure}.
   *
   * @param file the file to delete
   * @param failure the exception that the caller will throw
   */
  private static void deleteAfterFailure(Path file, Throwable failure) {
    try {
      Files.deleteIfExists(file);
    } catch (IOException e) {
      failure.addSuppressed(e);
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
        boolean isExit = isExitOrThrows(para.pptName());
        // The first legit 0 nonce will have an ENTER and EXIT
        // seeing a 0 means we have reached the next file
        if (non == 0 && !isExit) {
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
   * Copies {@code inFilename} to {@code outFilename}, giving a new nonce to each sample that lacks
   * one, such as an OBJECT or CLASS sample. Each new nonce is larger than {@code maxNonce}.
   *
   * @param inFilename the dtrace file to read
   * @param outFilename the dtrace file to write
   * @param maxNonce the largest nonce in {@code inFilename}
   * @throws IOException if there is trouble reading or writing a file
   */
  private static void addMissingNonces(String inFilename, String outFilename, int maxNonce)
      throws IOException {
    try (BufferedReader br = FilesPlume.newBufferedFileReader(inFilename);
        PrintWriter out = new PrintWriter(FilesPlume.newBufferedFileWriter(outFilename))) {
      Paragraph para;
      while ((para = Paragraph.read(br)) != null) {
        if (para.isSample() && para.nonce == -1) {
          para.printWithNonce(out, ++maxNonce);
        } else {
          para.print(out);
        }
      }
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

  /** The parsed program point names, so that each distinct name is parsed only once. */
  private static final Map<String, PptName> pptNames = new HashMap<>();

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
      // A line without ":::" does not start a sample, because Daikon rejects a program point name
      // without ":::" (see the PptName constructor).
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
     * Returns the program point name of this sample.
     *
     * @return the program point name of this sample
     */
    PptName pptName() {
      return pptNames.computeIfAbsent(lines.get(pptNameIndex), PptName::new);
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
      int rest = (nonce == -1) ? pptNameIndex + 1 : pptNameIndex + 3;
      for (int i = 0; i <= pptNameIndex; i++) {
        out.println(lines.get(i));
      }
      out.println(FileIO.NONCE_HEADER);
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
