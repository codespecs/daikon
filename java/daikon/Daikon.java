// Main routine for Daikon invariant detector

package daikon;

import daikon.inv.*;
import daikon.split.*;

import utilMDE.*;

import java.util.*;

public class Daikon {

  static final boolean disable_splitting = false;

  // The two arguments to daikon.Daikon are a comma-separated list of
  // declaration files, and a comma-separated list of data trace files.
  //
  // So you should be able to do
  //
  //   java daikon.Daikon a.decls,b.decls,c.decls  g.dtrace
  //
  // I realize this isn't the ideal user interface; I can change it if you
  // like.  (For instance, it would probably be more convenient for you if you
  // just supplied all the files on the command line and I inferred which was a
  // declaration file and which was a trace file, right?)

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

    for (Iterator itor = new TreeSet(all_ppts.keySet()).iterator() ; itor.hasNext() ; ) {
      String ppt_name = (String) itor.next();
      PptTopLevel ppt = (PptTopLevel) all_ppts.get(ppt_name);
      ppt.initial_processing();
      if (! disable_splitting) {
        Splitter[] pconds = ppt.getSplitters();
        ppt.addConditions(pconds);
      }
      ppt.print_invariants_maybe();
    }

    // Old implementation that didn't interleave invariant inference and
    // reporting.
    // for (Iterator itor = all_ppts.values().iterator() ; itor.hasNext() ; ) {
    //   PptTopLevel ppt = (PptTopLevel) itor.next();
    //   ppt.initial_processing();
    // }
    // // Now examine the invariants.
    // System.out.println("Examining the invariants.");
    // for (Iterator itor = new TreeSet(all_ppts.keySet()).iterator() ; itor.hasNext() ; ) {
    //   String ppt_name = (String) itor.next();
    //   PptTopLevel ppt_tl = (PptTopLevel) all_ppts.get(ppt_name);
    //   ppt_tl.print_invariants_maybe();
    // }

    System.out.println("Exiting");

  }

}
