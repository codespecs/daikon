package daikon.inv.binary.sequenceString;

import daikon.*;

import java.util.*;

public final class SequenceStringFactory {

  public final static boolean debugSequenceStringFactory = false;
  // public final static boolean debugSequenceStringFactory = true;

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static Vector instantiate(PptSlice ppt) {
    if (debugSequenceStringFactory) {
      System.out.println("SequenceStringFactory instantiate " + ppt.name());
    }

    boolean seq_first;

    VarInfo seqvar;
    VarInfo sclvar;
    {
      VarInfo vi0 = ppt.var_infos[0];
      VarInfo vi1 = ppt.var_infos[1];
      if ((vi0.rep_type == ProglangType.STRING_ARRAY)
          && (vi1.rep_type == ProglangType.STRING)) {
        seq_first = true;
        seqvar = ppt.var_infos[0];
        sclvar = ppt.var_infos[1];
      } else if ((vi0.rep_type == ProglangType.STRING)
                 && (vi1.rep_type == ProglangType.STRING_ARRAY)) {
        seq_first = false;
        seqvar = ppt.var_infos[1];
        sclvar = ppt.var_infos[0];
      } else {
        throw new Error("Bad types");
      }
    }

    if (! seqvar.eltsCompatible(sclvar))
      return null;

    Vector result = new Vector();
    // I could check that the length of the sequence isn't always 0.
    result.add(Member.instantiate(ppt, seq_first));
    return result;
  }

  private SequenceStringFactory() {
  }

}
