package daikon.derive.unary;

import daikon.VarInfo;
import daikon.derive.DerivationFactory;
import org.checkerframework.checker.nullness.qual.Nullable;

public abstract class UnaryDerivationFactory implements DerivationFactory {

  public abstract UnaryDerivation @Nullable [] instantiate(VarInfo vi);
}
