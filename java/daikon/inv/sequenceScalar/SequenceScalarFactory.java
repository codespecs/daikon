package daikon.inv.sequenceScalar;

import daikon.*;

import java.util.*;

public class SequenceScalarFactory {


  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static Vector instantiate(PptSlice ppt, int pass) {
    boolean seq_first;

    {
      VarInfo vi0 = ppt.var_infos[0];
      VarInfo vi1 = ppt.var_infos[1];
      if ((vi0.rep_type == ProglangType.INT_ARRAY)
          && (vi1.rep_type == ProglangType.INT)) {
        seq_first = true;
      } else if ((vi0.rep_type == ProglangType.INT)
                 && (vi1.rep_type == ProglangType.INT_ARRAY)) {
        seq_first = false;
      } else {
        throw new Error("Bad types");
      }
    }

    VarInfo seqvar = seq_first ? ppt.var_infos[0] : ppt.var_infos[1];
    VarInfo sclvar = seq_first ? ppt.var_infos[1] : ppt.var_infos[0];

    if (Daikon.check_program_types
        && (! seqvar.type.elementType().equals(sclvar.type)))
      return null;

    Vector result = new Vector();
    if (pass == 2) {
      // I could check that the length of the sequence isn't always 0.
      result.add(Member.instantiate(ppt, seq_first));
    }
    return result;
  }

  private SequenceScalarFactory() {
  }

}
