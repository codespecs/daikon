package daikon.derive.unary;

import daikon.*;

import java.util.Iterator;

public final class SequenceLengthFactory extends UnaryDerivationFactory {

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

    // Omit length of fields applied over sequences, since they always
    // have a corresponding equal-length sequence (sans-field).
    {
      // If $Field appears before $Elements, omit.
      Iterator nodes = (new VarInfoName.InorderFlattener(vi.name)).nodes().iterator();
      while (nodes.hasNext()) {
	VarInfoName node = (VarInfoName) nodes.next();
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

    return new UnaryDerivation[] { new SequenceLength(vi, 0),
                                   new SequenceLength(vi, -1) };
  }

}
