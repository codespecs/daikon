package daikon;

import java.io.*;
import java.util.*;
import javax.swing.tree.*;
import utilMDE.*;

/** Maps from a name (a String) to a PptTopLevel. */
public class PptMap
  implements Serializable
{
  private final Map nameToPpt = new HashMap();

  public void add(PptTopLevel ppt)
  {
    nameToPpt.put(ppt.name, ppt);
  }

  public PptTopLevel get(String name)
  {
    return (PptTopLevel) nameToPpt.get(name);
  }

  public PptTopLevel get(PptName name)
  {
    return get(name.toString());
  }

  public boolean containsName(String name)
  {
    return nameToPpt.containsKey(name);
  }

  public Collection asCollection()
  {
    return Collections.unmodifiableCollection(nameToPpt.values());
  }

  public Collection nameStringSet()
  {
    return Collections.unmodifiableSet(nameToPpt.keySet());
  }

  public Iterator iterator()
  {
    return asCollection().iterator();
  }

  public DefaultMutableTreeNode diff(PptMap other) {
    DefaultMutableTreeNode result = new DefaultMutableTreeNode();

    Iterator itor1 = new TreeSet(this.nameStringSet()).iterator();
    Iterator itor2 = new TreeSet(other.nameStringSet()).iterator();
    /*
    for (Iterator opi = new OrderedPairIterator(itor1, itor2);
	 opi.hasNext(); ) {
      Pair pair = (Pair) opi.next();
      if (pair.b == null) {
        result.add("Program point " + pair.a +
		   " only in first set of invariants");
      } else if (pair.a == null) {
        result.add("Program point " + pair.b +
		   " only in second set of invariants");
      } else {
        String ppt_name = (String) pair.a;
        Assert.assert(ppt_name.equals(pair.b));
	// mjh - How do we know the ppt in the pptmap is PptTopLevel?
	// Not specified in PptMap.
        PptTopLevel thisPpt = this.get(ppt_name);
        PptTopLevel otherPpt = other.get(ppt_name);
        result.addAll(thisPpt.diff(otherPpt));
      }
    }
    */
    return result;
  }


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

