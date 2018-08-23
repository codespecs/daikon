package typequals.prototype.qual;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;
import org.checkerframework.framework.qual.SubtypeOf;

/**
 * The Prototype and NonPrototype qualifiers apply only to Invariant (and its subclasses). A
 * prototype invariant is one that is not instantiated over a particular set of variables, will
 * never be applied to values, etc. A prototype can have any operation performed on it that does not
 * depend on properties of instance fields, and the prototype's {@link daikon.inv.Invariant#ppt}
 * field is null. Prototypes are often used as factory objects from which to instantiate
 * non-prototypes, but can be used for other purposes as well.
 *
 * <p>The {@code Prototype} qualifier means an invariant that is <em>either</em> a prototype or not.
 * There is not a way to say that a particular reference is definitely a prototype. A method that
 * can be called on a prototype does not access instance fields. It cannot be a static method for
 * two reasons:
 *
 * <ul>
 *   <li>The method does access the {@code swap} field in some classes. This is a design flaw; the
 *       classes that currently use a {@code swap} field should probably be rewritten to be two
 *       distinct classes. (Currently, some of Daikon's invariants use the {@code swap} field
 *       approach and some use the approach of multiple classes. Making the codebase uniform would
 *       be another benefit.)
 *   <li>Static method cannot override one another, so the methods need to be instance methods
 *       regardless.
 * </ul>
 */
@Target({ElementType.TYPE_USE, ElementType.TYPE_PARAMETER})
@SubtypeOf({})
public @interface Prototype {}
