package daikon.derive.binary;

import daikon.*;
import daikon.derive.*;

public abstract class BinaryDerivationFactory implements DerivationFactory {

  // I could conceivably get rid of "applicable" and make "instantiate"
  // return an empty array if it's not applicable.

  public abstract BinaryDerivation[] instantiate(VarInfo vi1, VarInfo vi2);

  public abstract boolean applicable(VarInfo vi1, VarInfo vi2);
}
