package typequals;

import checkers.quals.*;
import com.sun.source.tree.Tree;

@TypeQualifier
@SubtypeOf({Prototype.class, NonPrototype.class})
@ImplicitFor(trees={Tree.Kind.NULL_LITERAL})
public @interface PrototypeBottom {}
