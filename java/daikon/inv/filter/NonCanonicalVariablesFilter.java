package daikon.inv.filter;

import daikon.inv.*;

public class NonCanonicalVariablesFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress invariants containing non-canonical variables [deprecated]";
  }

  //  We should discard this invariant only if it has non-canonical variables AND it is
  //  not an equality Comparison invariant.  We need to keep equality Comparison
  //  invariants so that later on, Equality invariants will be made out of them.
  boolean shouldDiscardInvariant( Invariant invariant ) {
    return false;
    /* [INCR]
    if (invariant instanceof Implication) {
      answer = ((Implication)invariant).consequent().hasNonCanonicalVariable();
      vis = ((Implication)invariant).consequent().ppt.var_infos;
    } else {
      answer = (invariant.hasNonCanonicalVariable() && !IsEqualityComparison.it.accept(invariant));
      vis = invariant.ppt.var_infos;
    }

    if (answer) {
      invariant.discardCode = DiscardCode.non_canonical_var;
      for (int i=0; i < vis.length; i++) {
        if (!vis[i].isCanonical()) {
          invariant.discardString = vis[i].name.name()+" has canonical form "+vis[i].equal_to.name.name();
          break;
        }
      }
    }
    return answer;
    */ // [INCR]
  }
}
