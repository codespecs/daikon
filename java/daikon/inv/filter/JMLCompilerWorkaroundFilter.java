package daikon.inv.filter;

import daikon.*;
import daikon.inv.*;

public class JMLCompilerWorkaroundFilter extends InvariantFilter {
  public static final String DESCRIPTION
    = "Suppresses invariants that cause the jmlc tool to fail "
    + "when they are used in JML annotations";
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
    VarInfo[] vis = inv.ppt.var_infos;
    for (int i=0; i<vis.length; i++) {
      if (vis[i].name.hasNodeOfType(VarInfoName.TypeOf.class)) {
        return true;
      }
    }
    return false;
  }
}
