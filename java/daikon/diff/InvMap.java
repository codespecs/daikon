package daikon.diff;

import java.io.*;
import java.util.*;
import daikon.*;
import daikon.inv.*;

/**
 * Maps ppts to lists of invariants.  Has an iterator to return the
 * ppts in the order they were inserted.
 **/
public class InvMap implements Serializable {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020301L;
  
  private Map pptToInvs = new HashMap();
  private List ppts = new ArrayList();
    
  public InvMap() { }

  public void put(PptTopLevel ppt, List invs) {
    ppts.add(ppt);
    pptToInvs.put(ppt, invs);
  }

  public List get(PptTopLevel ppt) {
    return (List) pptToInvs.get(ppt);
  }

  /**
   * Returns an iterator over the ppts, in the order they were added
   * to the map.
   **/
  public Iterator iterator() {
    return ppts.iterator();
  }

  public String toString() {
    String result = new String();
    for (Iterator i = iterator(); i.hasNext(); ) {
      PptTopLevel ppt = (PptTopLevel) i.next();
      result += ppt.name + Global.lineSep;
      List invs = get(ppt);
      for (Iterator i2 = invs.iterator(); i2.hasNext(); ) {
        Invariant inv = (Invariant) i2.next();
        result += "  " + inv.format() + Global.lineSep;
      }
    }
    return result;
  }
}
