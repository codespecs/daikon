package daikon;

import utilMDE.Option;
import utilMDE.Options;
import utilMDE.SimpleLog;

import java.util.*;
import java.io.*;

public class StackOffsets {

  public static class OffsetInfo {
    int offset;
    int cnt;
    public OffsetInfo (int offset) {
      this.offset = offset;
      cnt = 1;
    }
  }

  public static class StackInfo {

    PptTopLevel ppt;
    Stack<Integer> entry_esp = new Stack<Integer>();
    int entry_cnt = 0;
    Map<VarInfo,OffsetInfo> offset_map
      = new LinkedHashMap<VarInfo,OffsetInfo>();

    public StackInfo (PptTopLevel ppt) {
      this.ppt = ppt;
    }

    public void sample (PptTopLevel func_ppt, PptTopLevel ppt, ValueTuple vt) {

      String enter_esp_name = String.format ("bv:0x%x:esp", ppt.bb_offset());

      // Loop through each variable
      for (VarInfo vi : ppt.var_infos) {

        // Skip non stack pointer variables
        if (!vi.name().endsWith (":esp"))
          continue;

        // If this is the function entry esp
        if ((ppt == func_ppt) && (enter_esp_name.equals (vi.name()))) {
          entry_cnt++;
          // if (entry_esp.size() > 0)
            // System.out.printf ("Entry stack for %s is size %d\n",
            //                    func_ppt.name(), entry_esp.size());
          Integer entry = int_val (vi, vt);
          assert entry != null :  "function entry esp should be be nonsense";
          entry_esp.push (entry);
          // System.out.printf ("Enter function %s\n", ppt.name);
        } else {
          if (entry_esp.size() == 0) {
            System.out.printf ("No function entry for ppt %s (func %s) "
                               + "(line %d)\n", ppt.name(), ppt.function_id,
                               FileIO.get_linenum());
            return;
          }
          OffsetInfo past_offset = offset_map.get(vi);
          Integer cur_offset = int_val(vi, vt);
          if (cur_offset == null) {
            System.out.printf ("unexpected nonsense val for ppt %s (func %s) "
                               + "(line %d)\n", ppt.name(), ppt.function_id,
                               FileIO.get_linenum());
          } else if (past_offset == null) {
            cur_offset -= entry_esp.peek();
            offset_map.put (vi, new OffsetInfo (cur_offset));
          } else {
            cur_offset -= entry_esp.peek();
            if (cur_offset != past_offset.offset) {
              System.out.printf ("Mismatch at %s: %d - %d [%d]\n", vi.name(),
                             cur_offset, past_offset.offset, past_offset.cnt);
            } else {
              past_offset.cnt++;
            }
          }

        }
      }

      // pop off the entry if this is a return
      if (ppt.flags.contains (PptTopLevel.PptFlags.RETURN)) {
        // System.out.printf ("Return from %s (function %s)\n", ppt.name(),
        //                   ppt.function_id);
        entry_esp.pop();
      }

    }

    public static /*@Nullable*/ Integer int_val (VarInfo vi, ValueTuple vt) {
      Object o = vt.getValue(vi);
      if (o == null)
        return null;
      return new Integer (((Long) o).intValue());
    }

    public void dump() {

      System.out.printf ("Ppt %s [stack=%d/%d]\n", ppt.name(),
                         entry_esp.size(), entry_cnt);
      for (VarInfo vi : offset_map.keySet()) {
        OffsetInfo oi = offset_map.get (vi);
        System.out.printf ("  var %s: %d [%d]\n", vi.name(), oi.offset, oi.cnt);
      }
    }

  }
  public static class Processor extends FileIO.Processor {

    Map<PptTopLevel,StackInfo> stack_info_map
      = new LinkedHashMap<PptTopLevel,StackInfo>();

    public void process_sample (PptMap all_ppts, PptTopLevel ppt, ValueTuple vt,
                                /*@Nullable*/ Integer nonce) {

      // System.out.printf ("Processing sample for ppt %s [%d]\n", ppt.name(),
      //                   nonce);

      // Find the StackInfo for this function
      String function_id = ppt.function_id;
      PptTopLevel func_ppt = all_ppts.get(function_id);
      StackInfo si = stack_info_map.get (func_ppt);
      if (si == null) {
        si = new StackInfo (func_ppt);
        stack_info_map.put (func_ppt, si);
      }
      si.sample (func_ppt, ppt, vt);
    }

  }

  public static PptMap all_ppts = new PptMap();

  public static Processor processor = new Processor();

  public static void main (String args[]) throws IOException {

    String dtrace_file = args[0];

    FileIO.read_data_trace_file (dtrace_file, all_ppts, processor, false, true);

    // build function information
    Map<PptTopLevel,List<PptTopLevel>> func_map
      = new LinkedHashMap<PptTopLevel,List<PptTopLevel>>();
    for (Iterator<PptTopLevel> ii = all_ppts.pptIterator(); ii.hasNext(); ) {
      PptTopLevel ppt = ii.next();
      PptTopLevel func = all_ppts.get(ppt.function_id);
      List<PptTopLevel> bbs = func_map.get (func);
      if (bbs == null) {
        bbs = new ArrayList<PptTopLevel>();
        func_map.put (func, bbs);
      }
      bbs.add (ppt);
    }

    // Dump function info
    for (PptTopLevel ppt : func_map.keySet()) {
      System.out.printf ("function %s\n", ppt.name());
      List<PptTopLevel> bbs = func_map.get (ppt);
      PptTopLevel prev = null;
      for (PptTopLevel bb : bbs) {
        System.out.printf ("  %04X+%02d: ", bb.bb_offset() & 0xFFFF,
                           bb.bb_length);
        assert (prev == null) ||
          ((prev.bb_offset() + prev.bb_length) == bb.bb_offset())
          : prev.name() + "  not adjacent to " + bb.name();
        if (bb.ppt_successors != null) {
          for (String succ : bb.ppt_successors) {
            PptTopLevel ppt_succ = all_ppts.get (succ);
            System.out.printf ("%04X ", ppt_succ.bb_offset() & 0xFFFF);
            assert ppt_succ.function_id.equals (ppt.function_id);
          }
        }
        if (bb.flags.contains(PptTopLevel.PptFlags.RETURN))
          System.out.printf ("[ret]");
        System.out.printf ("\n");
      }
    }

    // Dump stack info for each function
    for (PptTopLevel ppt : processor.stack_info_map.keySet()) {
      StackInfo si = processor.stack_info_map.get (ppt);
      si.dump();
    }
  }

}
