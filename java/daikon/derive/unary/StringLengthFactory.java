package daikon.derive.unary;

import daikon.Global;
import daikon.VarInfo;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.checkerframework.checker.nullness.qual.Nullable;

public final class StringLengthFactory extends UnaryDerivationFactory {

  /** Debug output. */
  public static final Logger debug = Logger.getLogger("daikon.derive.unary.StringLengthFactory");

  @Override
  public UnaryDerivation @Nullable [] instantiate(VarInfo vi) {
    if (!StringLength.dkconfig_enabled) {
      return null;
    }

    if (!vi.file_rep_type.isString()) {
      return null;
    }

    if (!StringLength.applicable(vi)) {
      Global.tautological_suppressed_derived_variables++;
      return null;
    }

    if (debug.isLoggable(Level.FINE)) {
      debug.fine("Instantiating for " + vi.name() + " in " + vi.ppt);
    }

    return new UnaryDerivation[] {new StringLength(vi)};
  }
}
