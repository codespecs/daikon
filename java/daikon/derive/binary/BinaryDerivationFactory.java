package daikon.derive.binary;

import daikon.*;
import daikon.derive.*;

public abstract class BinaryDerivationFactory implements DerivationFactory {

  public abstract BinaryDerivation[] instantiate(VarInfo vi1, VarInfo vi2);

}
