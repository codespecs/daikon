package daikon.inv.filter;

import daikon.inv.*;
import daikon.PrintInvariants;
import java.util.logging.Level;

/**
 * Filter for not printing invariants suppressed during checking.
 * JHP: remove
 **/
public class SuppressionFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress invariants that aren't checked during run [DEPRECATED]";
  }

  boolean shouldDiscardInvariant( Invariant inv ) {
    return (false);
  }
}
