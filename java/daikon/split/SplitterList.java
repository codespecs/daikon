package daikon.split;

import daikon.*;

import java.util.*;

import utilMDE.*;

// Yes, this is a horrible way to hard-code this.  I'm sorry.

public abstract class SplitterList {

  static HashMap ppt_splitters = new HashMap();

  public static void put(String pptname, Splitter[] splits) {
    // System.out.println("SplitterList.put(" + pptname + ")");
    Assert.assert(! ppt_splitters.containsKey(pptname));
    ppt_splitters.put(pptname, splits);
  }

  public static Splitter[] get(String pptname) {
    return (Splitter[]) ppt_splitters.get(pptname);
  }

/// This is now done elsewhere (eg, in GriesLisp).
//   static {
//     ppt_splitters = new HashMap();
//     HashMap sp = ppt_splitters; // short name for convenience
//
//   }
}
