package daikon.derive.unary;

import daikon.*;
import daikon.derive.*;

public abstract class UnaryDerivationFactory implements DerivationFactory {

  public abstract UnaryDerivation @Nullable [] instantiate(VarInfo vi);
}
