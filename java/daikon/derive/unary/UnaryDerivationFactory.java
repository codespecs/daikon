package daikon.derive.unary;

import daikon.*;
import daikon.derive.*;

public abstract class UnaryDerivationFactory implements DerivationFactory {

  public abstract UnaryDerivation[] instantiate(VarInfo vi);

  public abstract boolean applicable(VarInfo vi);
}
