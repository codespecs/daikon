package typequals;

import checkers.basetype.BaseTypeChecker;
import checkers.quals.*;

/**
 * The prototype checker is relevant only to Invariant and its subclasses.
 * Other classes should never be annotated, and
 * the checker should never issue a warning about other classes.
 * <p>
 *
 * A word about defaults:  It is never necessary to write @NonPrototype,
 * because every use of every type defaults to NonPrototype (and in fact
 * this default applies to every class declaration as well).  The
 * declaration of Invariant (and its subclasses) are explicitly marked
 * as @PrototypeOrNot, so that it is possible to annotate them
 * with @NonPrototype or @PrototypeOrNot.
 **/

@TypeQualifiers({ Prototype.class, NonPrototype.class,
                  PrototypeOrNot.class, PrototypeBottom.class })
public final class PrototypeChecker extends BaseTypeChecker { }
