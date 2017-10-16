package daikon.tools;

import daikon.*;
import java.util.*;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
*/

/**
 * A class that gives an example of how to use a FileIO.Processor object to read a trace file.
 * Invoke it like:
 *
 * <pre>
 *   java daikon.tools.ReadTrace file1 file2 ...
 * </pre>
 *
 * A concrete example invocation:
 *
 * <pre>
 *   java -cp $DAIKONDIR/daikon.jar daikon.tools.ReadTrace /scratch/$USER/tests/daikon-tests/StackAr/StackAr.dtrace.gz
 * </pre>
 *
 * You probably won't run this program. Instead, you will copy parts of its source code in the
 * process of writing your own program that reads a dtrace file.
 */
public class ReadTrace {

  /** @param args data trace file names */
  public static void main(String[] args) {
    CollectDataProcessor processor = new CollectDataProcessor();
    PptMap ppts = new PptMap();
    try {
      FileIO.read_data_trace_files(Arrays.asList(args), ppts, processor, false);
    } catch (Exception e) {
      throw new Error(e);
    }

    // At this point, the processor has been called on each sample in turn.
    // Now, we can do whatever we like with the data.

    for (PptTopLevel ppt : processor.samples.keySet()) {
      for (ValueTuple vt : processor.samples.get(ppt)) {
        System.out.printf("%nSample for %s :%n", ppt.name());
        for (VarInfo vi : ppt.var_infos) {
          System.out.printf("%s = %s%n", vi.name(), vt.getValueOrNull(vi));
        }
      }
    }
  }

  /**
   * Populates the {@code samples} map with all the data read from the file. This is only reasonable
   * for small trace files, since all the data will be retained in memory!
   */
  public static class CollectDataProcessor extends FileIO.Processor {

    public Map<PptTopLevel, List<ValueTuple>> samples =
        new LinkedHashMap<PptTopLevel, List<ValueTuple>>();

    /** Process the sample, by adding it to the {@code samples} map. */
    /*@RequiresNonNull("FileIO.data_trace_state")*/
    @Override
    public void process_sample(
        PptMap all_ppts, PptTopLevel ppt, ValueTuple vt, /*@Nullable*/ Integer nonce) {

      // Add orig and derived variables to the ValueTuple
      assert vt.vals != null
          : "@AssumeAssertion(nullness): bug: Checker Framework bug:  vals is a non-null array, but is reported as nullable";
      FileIO.compute_orig_variables(ppt, vt.vals, vt.mods, nonce);
      FileIO.compute_derived_variables(ppt, vt.vals, vt.mods);

      // Intern the sample, to save space, since we are storing them all.
      vt = new ValueTuple(vt.vals, vt.mods);

      // Add the sample to the map
      if (!samples.containsKey(ppt)) {
        samples.put(ppt, new ArrayList<ValueTuple>());
      }
      samples.get(ppt).add(vt);
    }
  }
}
