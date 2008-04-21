package checkers.quals;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import checkers.javari.JavariChecker;
import checkers.metaquals.TypeQualifier;
import static java.lang.annotation.ElementType.*;

/**
 * Indicates that a field is assignable, even if it is inside a {@link ReadOnly}
 * instance.
 * 
 * <p>
 * 
 * This annotation is part of the Javari language.
 * 
 * @see JavariChecker
 * @manual #javari Javari Checker
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({FIELD})
@TypeQualifier // (for now)
public @interface Assignable {

}
