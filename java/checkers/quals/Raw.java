package checkers.quals;

import static java.lang.annotation.ElementType.*;

import java.lang.annotation.*;

import checkers.metaquals.*;
import checkers.nonnull.NonNullChecker;

/**
 * A method receiver annotation that indicates that non-null fields might be
 * null within the body of the method, e.g., if {@code this} is {@code Raw},
 * {@code this.field} might be null even if {@code field} was declared to be
 * {@link NonNull}.
 * 
 * <p>
 * 
 * This annotation is associated with the {@link NonNullChecker}.
 * 
 * @see NonNull
 * @see NonNullChecker
 * @manual #nonnull NonNull Checker
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({FIELD, LOCAL_VARIABLE, METHOD, PARAMETER, TYPE})
@TypeQualifier
@SubtypeOf( { Nullable.class } )
public @interface Raw {
    //Class<?> upTo() default Object.class;
}
