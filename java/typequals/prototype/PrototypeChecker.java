package typequals.prototype;

import org.checkerframework.common.basetype.BaseTypeChecker;

/**
 * The prototype checker is relevant only to Invariant and its subclasses. Other classes should
 * never be annotated, and the checker should never issue a warning about other classes.
 *
 * <p>A word about defaults: It is never necessary to write @NonPrototype, because every use of
 * every type defaults to NonPrototype (and in fact this default applies to every class declaration
 * as well). The declaration of Invariant (and its subclasses) are explicitly marked as @Prototype,
 * so that it is possible for instances to be marked either with @Prototype or @NonPrototype.
 */
public final class PrototypeChecker extends BaseTypeChecker {}
