// Kinda weak name.

package daikon;

import java.util.*;

/** Maps from a name (a String) to a Ppt. */
class PptMap extends HashMap {

  // // Is this of any interest?  Will I ever call it?
  // // This used to take a "String filename" initial argument.
  // void merge(PptMap other, int other_samples) {
  //   Set other_entries = other.entrySet();
  //   for (Iterator itor = other_entries.iterator() ; itor.hasNext() ;) {
  //     Map.Entry entry = (Map.Entry) itor.next();
  //     String ppt_name = (String) entry.getKey();
  //     // Do I really want to be using Ppt rather than IPpt here??
  //     PptTopLevel other_ppt_info = (PptTopLevel) entry.getValue();
  //     PptTopLevel this_ppt_info = (PptTopLevel) get(ppt_name);
  //     // Would it be acceptable, if (this_ppt_info == null), to
  //     // just do "ppt_map.put(ppt_name, other_ppt_info);" (without
  //     // then having to call merge?  Or do I want to make a copy (or make a
  //     // new object and then call merge) so that other_ppt_info doesn't
  //     // unexpectedly change?  For now, avoid the sharing, to be safe.
  //     if (this_ppt_info == null) {
  //       // This constructor is commented out; uncomment if I want to use it.
  //       this_ppt_info = new PptTopLevel(other_ppt_info);
  //       put(ppt_name, this_ppt_info);
  //     }
  //     this_ppt_info.merge(other_ppt_info);
  //   }
  // }

}

