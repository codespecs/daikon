package typequals;

import java.lang.annotation.*;
import checkers.quals.*;

/** Not an index into a ValueTuple nor into a list of VarInfo objects. */
@Target({ElementType.TYPE_USE, ElementType.TYPE_PARAMETER})
@TypeQualifier
@SubtypeOf(VIndexTop.class)
@DefaultQualifierInHierarchy
public @interface VIndexUnqualified {}
