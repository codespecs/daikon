package daikon.inv.binary.sequenceScalar;

import daikon.*;

import java.util.*;

public final class SequenceScalarFactory {

  public final static boolean debugSequenceScalarFactory = false;
  // public final static boolean debugSequenceScalarFactory = true;

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static Vector instantiate(PptSlice ppt, int pass) {
    if (debugSequenceScalarFactory) {
      System.out.println("SequenceScalarFactory instantiate (pass " + pass + ") " + ppt.name);
    }

    boolean seq_first;

    VarInfo seqvar;
    VarInfo sclvar;
    {
      VarInfo vi0 = ppt.var_infos[0];
      VarInfo vi1 = ppt.var_infos[1];
      if ((vi0.rep_type == ProglangType.INT_ARRAY)
          && (vi1.rep_type == ProglangType.INT)) {
        seq_first = true;
        seqvar = ppt.var_infos[0];
        sclvar = ppt.var_infos[1];
      } else if ((vi0.rep_type == ProglangType.INT)
                 && (vi1.rep_type == ProglangType.INT_ARRAY)) {
        seq_first = false;
        seqvar = ppt.var_infos[1];
        sclvar = ppt.var_infos[0];
      } else {
        throw new Error("Bad types");
      }
    }

    if (Daikon.check_program_types) {
      if (! seqvar.type.elementType().castable(sclvar.type)) {
        if (debugSequenceScalarFactory) {
          System.out.println("Non-castable types: " + sclvar.name + " of type " + sclvar.type.format() + " vs. element of " + seqvar.name + " whose type is " + seqvar.type.format());
        }
        return null;
      }
    }
    if (! Daikon.ignore_comparability) {
      if (! VarComparability.comparable(VarInfoName.parse("seqvar.name.elementName"), seqvar.comparability.elementType(),
                                        sclvar.name, sclvar.comparability)) {
        return null;
      }
    }

    Vector result = new Vector();
    if (pass == 1) {
      // nothing to do
    } else {
      // I could check that the length of the sequence isn't always 0.
      result.add(Member.instantiate(ppt, seq_first));
      result.add(SeqIntComparison.instantiate(ppt, seq_first));
    }
    return result;
  }

  private SequenceScalarFactory() {
  }

}
