package daikon.inv.unary.string;

import daikon.*;
import utilMDE.*;

import java.util.*;

public final class SingleStringFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static Vector instantiate(PptSlice ppt, int pass) {

    VarInfo var = ppt.var_infos[0];
    Assert.assert(var.rep_type == ProglangType.STRING);

    Vector result = new Vector();
    if (pass == 1) {
      result.add(OneOfString.instantiate(ppt));
    } else if (pass == 2) {
      // nothing currently instantiated here
    }
    return result;
  }

  private SingleStringFactory() {
  }

}
