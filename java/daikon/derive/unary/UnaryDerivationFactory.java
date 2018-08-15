package daikon.derive.unary;

import daikon.*;
import daikon.derive.*;
import org.checkerframework.checker.nullness.qual.Nullable;

public abstract class UnaryDerivationFactory implements DerivationFactory {

  public abstract UnaryDerivation @Nullable [] instantiate(VarInfo vi);
}
