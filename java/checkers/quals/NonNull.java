package checkers.quals;

import static java.lang.annotation.ElementType.*;

import java.lang.annotation.*;

import javax.lang.model.type.TypeKind;

import checkers.metaquals.*;
import checkers.nonnull.NonNullChecker;
import checkers.types.AnnotatedTypeMirror.AnnotatedPrimitiveType;

import com.sun.source.tree.Tree;

/**
 * Indicates that a variable should never have a null value.
 * 
 * <p>
 * 
 * This annotation is associated with the {@link NonNullChecker}.
 * 
 * @see Nullable
 * @see NonNullChecker
 * @manual #nonnull NonNull Checker
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({FIELD, LOCAL_VARIABLE, METHOD, PARAMETER, TYPE})
@TypeQualifier
@SubtypeOf( { Raw.class } )
@ImplicitFor(
    types={TypeKind.PACKAGE},
    typeClasses={AnnotatedPrimitiveType.class},
    trees={
        Tree.Kind.NEW_CLASS, 
        Tree.Kind.NEW_ARRAY,
        // All literals except NULL_LITERAL:
        Tree.Kind.BOOLEAN_LITERAL,
        Tree.Kind.CHAR_LITERAL, 
        Tree.Kind.DOUBLE_LITERAL,
        Tree.Kind.FLOAT_LITERAL, 
        Tree.Kind.INT_LITERAL, 
        Tree.Kind.LONG_LITERAL,
        Tree.Kind.STRING_LITERAL
    })
public @interface NonNull {

}
