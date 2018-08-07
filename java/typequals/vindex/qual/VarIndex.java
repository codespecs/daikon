package typequals.vindex.qual;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;
import org.checkerframework.framework.qual.SubtypeOf;

/** An index into a list of VarInfo objects. */
@Target({ElementType.TYPE_USE, ElementType.TYPE_PARAMETER})
@SubtypeOf(VIndexTop.class)
public @interface VarIndex {}
