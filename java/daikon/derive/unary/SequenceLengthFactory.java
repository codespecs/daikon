package daikon.derive.unary;

import daikon.*;
import java.util.logging.Level;
import java.util.logging.Logger;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
*/

public final class SequenceLengthFactory extends UnaryDerivationFactory {

  /** Debug output. */
  public static final Logger debug = Logger.getLogger("daikon.derive.unary.SequenceLengthFactory");

  @Override
  public UnaryDerivation /*@Nullable*/ [] instantiate(VarInfo vi) {
    if (!SequenceLength.dkconfig_enabled) {
      return null;
    }

    if (!vi.is_direct_array()) return null;

    if (!vi.aux.hasSize()) {
      // Don't derive if auxiliary info says size of this collection
      // has no meaning
      return null;
    }

    if (!SequenceLength.applicable(vi)) {
      Global.tautological_suppressed_derived_variables++;
      return null;
    }

    if (debug.isLoggable(Level.FINE)) {
      debug.fine("Instantiating for " + vi.name() + " in " + vi.ppt);
    }

    if (vi.aux.nullTerminating()) {
      return new UnaryDerivation[] {new SequenceLength(vi, 0), new SequenceLength(vi, -1)};
    } else {
      // If it can't terminate with nulls, then all members are important,
      // so we only need to do shift for 0
      return new UnaryDerivation[] {new SequenceLength(vi, 0)};
    }
  }
}
