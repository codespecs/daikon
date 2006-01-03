package daikon.derive.unary;

import daikon.*;

import java.util.Iterator;

import java.util.logging.Logger;
import java.util.logging.Level;

public final class SequenceLengthFactory extends UnaryDerivationFactory {


  /**
   * Debug output.
   *
   *
   **/

  public static final Logger debug =
    Logger.getLogger("daikon.derive.unary.SequenceLengthFactory");

  public UnaryDerivation[] instantiate(VarInfo vi) {
    if (!SequenceLength.dkconfig_enabled) {
      return null;
    }

    // if (vi.rep_type != ProglangType.INT_ARRAY)
    //   return null;
    if (! vi.rep_type.isArray())
      return null;

    // Omit length of .class sequences, since they always have a
    // corresponding equal-length sequence (sans-class).
    if (vi.name.hasNodeOfType(VarInfoName.TypeOf.class)) {
      return null;
    }

    if (!vi.aux.getFlag(VarInfoAux.HAS_SIZE)) {
      // Don't derive if auxiliary info says size of this collection
      // has no meaning
      return null;
    }

    // Omit length of fields applied over sequences, since they always
    // have a corresponding equal-length sequence (sans-field).
    {
      // If $Field appears before $Elements, omit.
      for (VarInfoName node : (new VarInfoName.InorderFlattener(vi.name)).nodes()) {
        if (node instanceof VarInfoName.Field) {
          return null;
        }
        if (node instanceof VarInfoName.Elements) {
          break;
        }
      }
    }

    if (! SequenceLength.applicable(vi)) {
      Global.tautological_suppressed_derived_variables++;
      return null;
    }

    if (debug.isLoggable(Level.FINE)) {
      debug.fine ("Instantiating for " + vi.name + " in " + vi.ppt);
    }

    if (vi.aux.getFlag(VarInfoAux.NULL_TERMINATING)) {
      return new UnaryDerivation[] { new SequenceLength(vi, 0),
                                     new SequenceLength(vi, -1) };
    } else {
      // If it can't terminate with nulls, then all members are important,
      // so we only need to do shift for 0
      return new UnaryDerivation[] { new SequenceLength(vi, 0)};
    }
  }

}
