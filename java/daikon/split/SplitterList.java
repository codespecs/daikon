package daikon.split;

import daikon.*;

import java.util.*;

import utilMDE.*;

// SplitterLisp maps from a program point name to an array of Splitter
// objects that should be used when splitting that program point.
// It's a shame to have to hard-code for each program point name.

public abstract class SplitterList {

  // maps from string to Splitter[]
  static HashMap ppt_splitters = new HashMap();

  public static void put(String pptname, Splitter[] splits) {
    // System.out.println("SplitterList.put(" + pptname + ")");
    if (pptname.equals("") && ppt_splitters.containsKey(pptname)) {
      Splitter[] old = (Splitter[]) ppt_splitters.get(pptname);
      Splitter[] new_splits = new Splitter[old.length + splits.length];
      System.arraycopy(old, 0, new_splits, 0, old.length);
      System.arraycopy(splits, 0, new_splits, old.length, splits.length);
      ppt_splitters.put(pptname, new_splits);
    } else {
      Assert.assert(! ppt_splitters.containsKey(pptname));
      // Assert.assert(! ppt_splitters.containsKey(pptname),
      //               "SplitterList already contains " + pptname
      //               + " which maps to\n " + ArraysMDE.toString(get_raw(pptname))
      //               + "\n which is " + formatSplitters(get_raw(pptname)));
      ppt_splitters.put(pptname, splits);
    }
  }

  // This is only used by the debugging output in SplitterList.put().
  public static String formatSplitters(Splitter[] splits) {
    if (splits == null)
      return "null";
    StringBuffer sb = new StringBuffer();
    sb.append("[");
    for (int i=0; i<splits.length; i++) {
      if (i != 0)
        sb.append(", ");
      sb.append("\"");
      sb.append(splits[i].condition());
      sb.append("\"");
    }
    sb.append("]");
    return sb.toString();
  }

  public static Splitter[] get_raw(String pptname) {
    return (Splitter[]) ppt_splitters.get(pptname);
  }

  // This routine tries the name first, then the base of the name, then the
  // class, then the empty string.  For instance, if the program point name is
  // "Foo.bar(IZ)V:::EXIT2", then it tries, in order:
  //   "Foo.bar(IZ)V:::EXIT2"
  //   "Foo.bar(IZ)V"
  //   "Foo.bar"
  //   "Foo"
  //   ""

  public static Splitter[] get(String name) {
    String name_ = name;        // debugging
    Splitter[] result;
    result = get_raw(name);
    if (Global.debugPptSplit)
      System.out.println("SplitterList.get found "
                         + ((result == null) ? "no" : "" + result.length)
                         + " splitters for " + name);
    if (result != null)
      return result;
    {
      int tag_index = name.indexOf(FileIO.ppt_tag_separator);
      if (tag_index != -1) {
        name = name.substring(0, tag_index);
        result  = get_raw(name);
        if (Global.debugPptSplit)
          System.out.println("SplitterList.get found "
                             + ((result == null) ? "no" : "" + result.length)
                             + " splitters for " + name);
        if (result != null)
          return result;
      }
    }
    int lparen_index = name.indexOf('(');
    {
      if (lparen_index != -1) {
        name = name.substring(0, lparen_index);
        result  = get_raw(name);
        if (Global.debugPptSplit)
          System.out.println("SplitterList.get found "
                             + ((result == null) ? "no" : "" + result.length)
                             + " splitters for " + name);
        if (result != null)
          return result;
      }
    }
    {
      // The class name runs up to the last dot before any open parenthesis.
      int dot_limit = (lparen_index == -1) ? name.length() : lparen_index;
      int dot_index = name.lastIndexOf('.', dot_limit - 1);
      if (dot_index != -1) {
        name = name.substring(0, dot_index);
        result = get_raw(name);
        if (Global.debugPptSplit)
          System.out.println("SplitterList.get found "
                             + ((result == null) ? "no" : "" + result.length)
                             + " splitters for " + name);
        if (result != null)
          return result;
      }
    }

    // Empty string means always applicable.
    result = get_raw("");
    if (Global.debugPptSplit)
      System.out.println("SplitterList.get found "
                         + ((result == null) ? "no" : "" + result.length)
                         + " splitters for " + name);
    if (result != null)
      return result;


    return null;
  }

}
