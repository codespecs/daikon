package daikon.derive.ternary;

import daikon.*;
import daikon.derive.*;

/**
 * Factory to produce TernaryDerivations.
 **/

public abstract class TernaryDerivationFactory implements DerivationFactory {

  /**
   * Create a set of derivations from three base variables.  If the
   * base variables aren't worth deriving from, returns null.
   * @param vi1
   * @param vi2
   * @param vi3 the three base variables.
   * @return a set of derivations based on three base variables.  We
   * allow more than one because the base variables may have multiple
   * derived variables, per type of derivation.  Can also be null if the
   * variables have nothing to derive from.
   **/
  public abstract TernaryDerivation[] instantiate(VarInfo vi1, VarInfo vi2,
                                                  VarInfo vi3);

}
