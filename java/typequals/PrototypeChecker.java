package typequals;

import checkers.basetype.BaseTypeChecker;
import checkers.quals.*;

@TypeQualifiers({ Prototype.class, NonPrototype.class,
                  PrototypeOrNot.class, PrototypeBottom.class })
public final class PrototypeChecker extends BaseTypeChecker { }
