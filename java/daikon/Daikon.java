// Main routine for Daikon invariant detector

package daikon;

import daikon.inv.*;
import utilMDE.*;
import java.util.*;

public class Daikon {

  /**
   * Two arguments:  comma-separated list of declaration files,
   * comma-separated list of data trace files.
   */
  public static void main(String[] args) {

    if (args.length != 2)
      throw new Error("Got " + args.length + " aguments, expected two:  comma-separated lists of decl files, trace files.");

    PptMap all_ppts = new PptMap();

    Vector decl_files = UtilMDE.tokens(args[0], ",");
    FileIO.read_declaration_files(decl_files, all_ppts, null);

//     System.out.println("Program points:");
//     for (Iterator itor = all_ppts.keySet().iterator() ; itor.hasNext() ; ) {
//       System.out.println(itor.next());
//     }

    Vector dtrace_files = UtilMDE.tokens(args[1], ",");
    FileIO.read_data_trace_files(dtrace_files, all_ppts, null);




    // Now examine the invariants.
    System.out.println("Examining the invariants.");

    // See print_invariants_ppt for a better approach to this; I need to
    // incorporate all that into some function somewhere.

    for (Iterator itor = new TreeSet(all_ppts.keySet()).iterator() ; itor.hasNext() ; ) {
      String ppt_name = (String) itor.next();
      PptTopLevel ppt_tl = (PptTopLevel) all_ppts.get(ppt_name);
      ppt_tl.print_invariants_maybe();
    }

    System.out.println("Exiting");

  }

}
