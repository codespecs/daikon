package checkers.quals;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import static java.lang.annotation.ElementType.*;

/**
 * Indicates that, for the variable on which this annotation appears,
 * the object to which this variable refers will not be modified via
 * this reference, except its fields explicitably marked as
 * Mutable.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({FIELD, LOCAL_VARIABLE, METHOD, PARAMETER, TYPE})
public @interface ReadOnly {

}
