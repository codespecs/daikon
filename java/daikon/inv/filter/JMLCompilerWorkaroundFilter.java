package daikon.inv.filter;

import daikon.inv.*;
import daikon.inv.filter.*;
import daikon.inv.Invariant.OutputFormat;

public class JMLCompilerWorkaroundFilter extends InvariantFilter {
  public static final String DESCRIPTION = "Suppresses invariants that cause " +
    "the jmlc tool to fail they are used in JML annotations";
  public static boolean createNextFilterOn = false;

  public JMLCompilerWorkaroundFilter() {
    super();
    if (!createNextFilterOn)
      turnOff();
  }

  public String getDescription() {
    return DESCRIPTION;
  }

  boolean shouldDiscardInvariant(Invariant inv) {
    if (inv.format_using(OutputFormat.JML).indexOf("\\type") != -1)
      return true;
    return false;
  }
}
