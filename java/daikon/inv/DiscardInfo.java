package daikon.inv;

import daikon.*;

/*>>>
import org.checkerframework.checker.signature.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * A class used for holding a DiscardCode and a string that contains more detailed information about
 * why an Invariant was discarded, as well as the classname and what would be returned by the
 * Invariant's format() method.
 */
public final class DiscardInfo {

  /**
   * The DiscardCode describing this DiscardInfo. It should never be null; if an invariant isn't
   * being discarded, use null as its DiscardInfo.
   */
  private DiscardCode discardCode;

  /** The detailed reason for discard */
  private String discardString;

  /**
   * The String that would have resulted from calling format() on the Invariant being discarded.
   * This does not have to be maintained if the Invariant isn't discarded.
   */
  private String discardFormat;

  /** Invariant for which the DiscardInfo applies */
  public Invariant inv;

  // Rarely used, so no need to precompute. -MDE
  // /**
  //  * The className of the Invariant being discarded
  //  */
  // private String className;

  // public DiscardInfo(String className, String discardFormat, DiscardCode discardCode, String discardString) {
  //   this.discardCode = discardCode;
  //   this.discardString = discardString;
  //   this.discardFormat = discardFormat;
  //   this.className = className;
  // }

  public DiscardInfo(Invariant inv, DiscardCode discardCode, String discardString) {
    assert inv.ppt != null;
    // this(inv.getClass().getName(), inv.format(), discardCode, discardString);
    this.discardCode = discardCode;
    this.discardString = discardString;
    this.discardFormat = inv.format();
    // this.className = inv.getClass().getName();
    this.inv = inv;
    inv.log("%s", discardString);
  }

  public String discardFormat() {
    return this.discardFormat;
  }

  public DiscardCode discardCode() {
    return this.discardCode;
  }

  public String discardString() {
    return this.discardString;
  }

  public /*@ClassGetName*/ String className() {
    return this.inv.getClass().getName();
  }

  /*@SideEffectFree*/
  public String format() {
    return (discardFormat + Global.lineSep + discardCode + Global.lineSep + discardString);
  }

  /** Adds the specified string as an additional reason. */
  public void add_implied(String reason) {
    discardString += " and " + reason;
  }

  /**
   * Adds an equality string to the discardString for each variable in in vis which is different
   * from the leader.
   */
  public void add_implied_vis(VarInfo[] vis) {
    for (int i = 0; i < vis.length; i++) {
      if (inv.ppt.var_infos[i] != vis[i]) {
        discardString += " and " + inv.ppt.var_infos[i] + "==" + vis[i];
      }
    }
  }
}
