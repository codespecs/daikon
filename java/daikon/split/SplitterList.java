package daikon.split;

import daikon.*;

import org.apache.oro.text.regex.*;
import java.util.*;

import utilMDE.*;

// SplitterLisp maps from a program point name to an array of Splitter
// objects that should be used when splitting that program point.
// It's a shame to have to hard-code for each program point name.

public abstract class SplitterList {

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  public static boolean dkconfig_all_splitters = false;

  // maps from string to Splitter[]
  private static final HashMap ppt_splitters = new HashMap();
  private static final PatternMatcher re_matcher = new Perl5Matcher();
  private static final PatternCompiler re_compiler = new Perl5Compiler();

  public static void put(String pptname, Splitter[] splits) {
    if (pptname.equals(".*") && ppt_splitters.containsKey(pptname)) {
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
  
  /*
    // This returns a list of all the splitters thar are applicable to the
    // program point named "name".  The list is constructed by looking up
    // various parts of "name" in the SplitterList hashtable.
    
    // This routine tries the name first, then the base of the name, then the
    // class, then the empty string.  For instance, if the program point name is
    // "Foo.bar(IZ)V:::EXIT2", then it tries, in order:
    //   "Foo.bar(IZ)V:::EXIT2"
    //   "Foo.bar(IZ)V"
    //   "Foo.bar"
    //   "Foo"
    //   ""
    
    public static Splitter[] get(String pptName) {
    String pptName_ = pptName;        // debugging
    Splitter[] result;
    Vector splitterArrays = new Vector();
    Vector splitters = new Vector();
    
    result = get_raw(pptName);
    if (result != null)
    splitterArrays.addElement(result);
    
    {
    int tag_index = pptName.indexOf(FileIO.ppt_tag_separator);
    if (tag_index != -1) {
    pptName = pptName.substring(0, tag_index);
    result = get_raw(pptName);
    if (result != null)
    splitterArrays.addElement(result);
    }
    }
    
    int lparen_index = pptName.indexOf('(');
    {
    if (lparen_index != -1) {
    pptName = pptName.substring(0, lparen_index);
    result = get_raw(pptName);
    if (result != null)
    splitterArrays.addElement(result);
    }
    }
    {
    // The class pptName runs up to the last dot before any open parenthesis.
    int dot_limit = (lparen_index == -1) ? pptName.length() : lparen_index;
    int dot_index = pptName.lastIndexOf('.', dot_limit - 1);
    if (dot_index != -1) {
    pptName = pptName.substring(0, dot_index);
    result = get_raw(pptName);
    if (result != null)
    splitterArrays.addElement(result);
    }
    }
    
    // Empty string means always applicable.
    result = get_raw("");
    if (result != null)
    splitterArrays.addElement(result);
    
    if (splitterArrays.size() == 0) {
    if (Global.debugPptSplit) {
    System.out.println("SplitterList.get found no splitters for " + pptName);
    return null;
    }
    }else{
    Splitter[] tempsplitters; 
    int counter = 0;
    for(int i = 0; i < splitterArrays.size(); i++) {
    tempsplitters = (Splitter[])splitterArrays.elementAt(i);
    for(int j = 0; j < tempsplitters.length; j++) {
    splitters.addElement(tempsplitters[j]);
    counter++;
    }
    }
    if (Global.debugPptSplit)
    System.out.println("SplitterList.get found " + counter + " splitters for " + pptName);
    }
    return (Splitter[])splitters.toArray(new Splitter[0]);
    }
  */
  //////////////////////
  
  /*
   * return the splitters associated with this program point name only.
   * @param pptName 
   * @return an array of splitters
   */
  public static Splitter[] get(String pptName) {
    Iterator itor = ppt_splitters.keySet().iterator();
    Vector splitterArrays = new Vector();
    Vector splitters = new Vector();
    
    while(itor.hasNext()) {
      String name = (String)itor.next(); //a daikon PptName, assumed 
      //to begin with  "ClassName.functionName"
      try{
	Pattern pat = re_compiler.compile(name);
	Pattern opat = re_compiler.compile("OBJECT");
	if (re_matcher.contains(pptName, pat)) {
	  Splitter[] result = get_raw(name);
	  if (result != null) {
	    splitterArrays.addElement(result);
	  }
	}else if (re_matcher.contains(pptName, opat) && re_matcher.contains(name, opat)) {
	  Iterator all = ppt_splitters.values().iterator();
	  while(all.hasNext()) {
	    splitterArrays.addElement((Splitter[])all.next());
	  }
	}
      }catch(Exception e) {
	if (Global.debugSplit.isDebugEnabled()) {
	  Global.debugSplit.debug("Error matching regex for " + pptName + "\n" + e.toString());
	}
      }
    }
    
    if (splitterArrays.size() == 0) {
      if (Global.debugSplit.isDebugEnabled()) {
	Global.debugSplit.debug("SplitterList.get found no splitters for " + pptName);
	return null;
      }
    }else{
      Splitter[] tempsplitters; 
      int counter = 0;
      for(int i = 0; i < splitterArrays.size(); i++) {
	tempsplitters = (Splitter[])splitterArrays.elementAt(i);
	for(int j = 0; j < tempsplitters.length; j++) {
	  splitters.addElement(tempsplitters[j]);
	  counter++;
	}
      }
      if (Global.debugSplit.isDebugEnabled())
	Global.debugSplit.debug("SplitterList.get found " + counter + " splitters for " + pptName);
    }    
    return (Splitter[])splitters.toArray(new Splitter[0]);
  }
 
  /* 
   * return all the splitters in this program, 
   * @return an array of splitters
   */
  public static Splitter[] get_all( ) {
    Vector splitters = new Vector();
    Iterator all_splitters = ppt_splitters.values().iterator();
    while(all_splitters.hasNext()) {
      Splitter[] splitter_array = (Splitter[])all_splitters.next();
      for(int i = 0; i < splitter_array.length; i++) {
	Splitter tempsplitter = splitter_array[i];
	int j = 0; boolean duplicate = false;
	//Weed out splitters with the same condition
	if (!splitters.isEmpty()) {
	  for(j = 0; j < splitters.size(); j++) {
	    if ((tempsplitter.condition().trim()).equals( ((Splitter)splitters.elementAt(j)).condition().trim())) {
	      //System.err.println(" duplicate " + tempsplitter.condition() + " \n");
	      duplicate = true;
	      break;
	    }
	  }
	}
	if (!duplicate) {
	  splitters.addElement(tempsplitter);
	}
      }
    }    
    return (Splitter[])splitters.toArray(new Splitter[0]);
  }

}





