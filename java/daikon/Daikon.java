// Main routine for Daikon invariant detector

package daikon;

import daikon.inv.*;
import daikon.split.*;

import utilMDE.*;

import java.util.*;
import java.io.*;

public class Daikon {

  // public static final boolean disable_splitting = false;
  public static final boolean disable_splitting = true;

  public static final boolean disable_ternary_invariants = false;
  // public static final boolean disable_ternary_invariants = true;

  // Change this at your peril; high costs in time and space for "false".
  public static final boolean check_program_types = true;
  // public static final boolean check_program_types = false;


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

    PptMap all_ppts = new PptMap();

    int num_decl_files = 0;
    int num_dtrace_files = 0;
    try {
      for (int i=0; i<args.length; i++) {
        String file = args[i];
        if (file.endsWith(".decls")) {
          FileIO.read_declaration_file(file, all_ppts, null);
          num_decl_files++;
        } else if (file.endsWith(".dtrace")) {
          FileIO.read_data_trace_file(file, all_ppts, null);
          num_dtrace_files++;
        } else {
          throw new Error("Unrecognized file suffix: " + file);
        }
      }
    } catch (IOException e) {
      e.printStackTrace();
      throw new Error(e.toString());
    }
    // Jikes complains when I put this all in one big string.
    System.out.print("Read " + num_decl_files + " declaration file" +
                       ((num_decl_files == 1) ? "" : "s"));
    System.out.print(", " + num_dtrace_files + " dtrace file");
    System.out.println(((num_dtrace_files == 1) ? "" : "s") + ".");

    // Old version that took only two arguments
    // if (args.length != 2)
    //   throw new Error("Got " + args.length + " aguments, expected two:  comma-separated lists of decl files, trace files.");
    // Vector decl_files = UtilMDE.tokens(args[0], ",");
    // FileIO.read_declaration_files(decl_files, all_ppts, null);
    // // System.out.println("Program points:");
    // // for (Iterator itor = all_ppts.keySet().iterator() ; itor.hasNext() ; ) {
    // //   System.out.println(itor.next());
    // // }
    // Vector dtrace_files = UtilMDE.tokens(args[1], ",");
    // FileIO.read_data_trace_files(dtrace_files, all_ppts, null);


    for (Iterator itor = new TreeSet(all_ppts.keySet()).iterator() ; itor.hasNext() ; ) {
      String ppt_name = (String) itor.next();
      PptTopLevel ppt = (PptTopLevel) all_ppts.get(ppt_name);
      if (ppt.num_samples() > 0) {
        ppt.initial_processing();
        if (! disable_splitting) {
          Splitter[] pconds = ppt.getSplitters();
          if (Global.debugPptSplit)
            System.out.println("Got " + pconds.length + " splitters for " + ppt.name);
          ppt.addConditions(pconds);
        }
        ppt.print_invariants_maybe();
        ppt.clear_view_caches();
      }
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
