package typequals.vindex.qual;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;
import org.checkerframework.framework.qual.ImplicitFor;
import org.checkerframework.framework.qual.LiteralKind;
import org.checkerframework.framework.qual.SubtypeOf;

@Target({ElementType.TYPE_USE, ElementType.TYPE_PARAMETER})
@SubtypeOf({ValueIndex.class, VarIndex.class, VIndexUnqualified.class})
@ImplicitFor(literals = {LiteralKind.NULL, LiteralKind.INT})
public @interface VIndexBottom {}
