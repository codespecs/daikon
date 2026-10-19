// DtracePartitioner.java
package daikon.tools;

import java.io.BufferedReader;
import java.io.Closeable;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import org.checkerframework.checker.calledmethods.qual.EnsuresCalledMethods;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.mustcall.qual.CreatesMustCallFor;
import org.checkerframework.checker.mustcall.qual.Owning;
import org.plumelib.util.FilesPlume;
import org.plumelib.util.Partitioner;

/**
 * This class partitions Daikon trace files so that invocations of the same program point are
 * grouped together for use with random selection.
 */
public class DtracePartitioner implements Closeable, Partitioner<String, String>, Iterator<String> {

  /** The system-specific line separator. */
  private static final String lineSep = System.lineSeparator();

  /** reading from the file as a lazy iterator */
  private @Owning BufferedReader br;

  /** the name of the Daikon trace file */
  private String filename;

  /**
   * @param filename the Daikon trace file to be partitioned
   * @throws IOException if the file cannot be opened
   */
  public DtracePartitioner(String filename) throws IOException {
    this.filename = filename;
    br = FilesPlume.newBufferedFileReader(filename);
  }

  /** Releases resources held by this. */
  @EnsuresCalledMethods(value = "br", methods = "close")
  @Override
  public void close(@GuardSatisfied DtracePartitioner this) throws IOException {
    br.close();
  }

  @Override
  public boolean hasNext(@GuardSatisfied DtracePartitioner this) {
    try {
      return br.ready();
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  /** Not implemented, because this class does not modify the underlying trace file. */
  @Override
  public void remove(@GuardSatisfied DtracePartitioner this) {
    throw new UnsupportedOperationException("Can not remove");
  }

  @Override
  public String next(@GuardSatisfied DtracePartitioner this) {
    try {
      String ret = grabNextInvocation();
      if (ret.indexOf("EXIT") != -1) {
        if (!br.ready()) {
          return "";
        }
        return next();
      } else {
        return ret;
      }
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  /**
   * Grabs the next invocation in the Daikon trace file by interpreting a blank line as the
   * invocation delimiter. Note that multiple blank lines between invocations might occur, so the
   * callee is responsible for checking if the returned String is a blank line.
   */
  private String grabNextInvocation(@GuardSatisfied DtracePartitioner this) throws IOException {
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

  /** Returns the program point name given by the input invocation. */
  @Override
  public String assignToBucket(String invocation) {
    if (invocation.indexOf(lineSep) == -1) {
      // was: return null;
      throw new Error("No lineSep: " + invocation);
    }
    return invocation.substring(0, invocation.indexOf(lineSep));
  }

  /**
   * Same as {@link #patchValues(List, boolean)} with second arg=false.
   *
   * @param enters a list of program point names
   * @return an ArrayList containing all of the elements of 'enters'. The original order is NOT
   *     guaranteed.
   */
  @CreatesMustCallFor("this")
  public List<String> patchValues(List<String> enters) {
    return patchValues(enters, false);
  }

  /**
   * Finds the exits that correspond to Enters.
   *
   * <p>Modifies: none
   *
   * @param enters a list of program point names
   * @param includeUnreturnedEnters ensures that any ENTER ppt invocations will definitely have a
   *     corresponding EXIT ppt invocation following them
   * @return an ArrayList containing all of the elements of 'enters'. The original order is NOT
   *     guaranteed.
   */
  @CreatesMustCallFor("this")
  public List<String> patchValues(List<String> enters, boolean includeUnreturnedEnters) {
    return new ArrayList<>(matchExits(enters, includeUnreturnedEnters).values());
  }

  /**
   * Finds the exits that correspond to enters. Reads the trace file once.
   *
   * @param enters ENTER, OBJECT, and CLASS records
   * @param includeUnreturnedEnters if true, the result includes the ENTER records that have no
   *     corresponding EXIT record
   * @return a map from each element of {@code enters} to its full invocation: an ENTER record
   *     followed by its EXIT record, or an OBJECT or CLASS record by itself. If {@code
   *     includeUnreturnedEnters} is true, also maps each ENTER record that has no EXIT record to
   *     itself.
   */
  @CreatesMustCallFor("this")
  public Map<String, String> matchExits(
      Collection<String> enters, boolean includeUnreturnedEnters) {
    try {
      System.out.println("Entering matchExits");
      Map<String, String> result = new HashMap<>();

      // Maps nonces --> ENTER half of invocation
      Map<Integer, String> nonceMap = new HashMap<>();
      for (String enterStr : enters) {
        // it could be an OBJECT or CLASS invocation ppt, which has no EXIT
        if (enterStr.indexOf("ENTER") == -1) {
          result.put(enterStr, enterStr);
          continue;
        }
        nonceMap.put(calcNonce(enterStr), enterStr);
      }

      br.close();
      // look for EXIT half of invocations
      br = FilesPlume.newBufferedFileReader(filename);
      while (br.ready()) {
        String nextInvo = grabNextInvocation();
        if (nextInvo.indexOf("EXIT") == -1) {
          continue;
        }
        String enterInvo = nonceMap.get(calcNonce(nextInvo));
        if (enterInvo != null) {
          result.put(enterInvo, enterInvo + lineSep + nextInvo);
        }
      }

      // add in the invocations that were never resolved because no
      // matching EXIT invocation exists.
      if (includeUnreturnedEnters) {
        for (String enterInvo : nonceMap.values()) {
          result.putIfAbsent(enterInvo, enterInvo);
        }
      }
      return result;

    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  private int calcNonce(String invocation) {
    StringTokenizer st = new StringTokenizer(invocation, lineSep);
    while (st.hasMoreTokens()) {
      String line = st.nextToken();
      if (line.equals("this_invocation_nonce")) {
        return Integer.parseInt(st.nextToken());
      }
    }
    throw new RuntimeException("This invocation didn't contain a nonce: " + invocation);
  }
}
