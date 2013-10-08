package typequals;

import java.util.Set;

import javacutils.AnnotationUtils;
import javacutils.InternalUtils;
import javacutils.TypesUtils;

import javax.lang.model.element.AnnotationMirror;

import checkers.basetype.BaseTypeChecker;
import checkers.types.AnnotatedTypeMirror;
import checkers.types.SubtypingAnnotatedTypeFactory;
import checkers.types.TreeAnnotator;

import com.sun.source.tree.BinaryTree;
import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.tree.Tree;

/**
 * Adds a type qualifier from the VIndex type system to the type of tree,
 * in the following cases:
 * [TODO]
 */
public class VIndexAnnotatedTypeFactory extends SubtypingAnnotatedTypeFactory<VIndexChecker> {

    private final AnnotationMirror VINDEXTOP;

    public VIndexAnnotatedTypeFactory(VIndexChecker checker,
            CompilationUnitTree root) {
        super(checker, root);
        VINDEXTOP = AnnotationUtils.fromClass(elements, VIndexTop.class);
        this.postInit();
    }

    @Override
    public TreeAnnotator createTreeAnnotator(VIndexChecker checker) {
        return new VIndexTreeAnnotator(checker);
    }

    private class VIndexTreeAnnotator extends TreeAnnotator {

        public VIndexTreeAnnotator(BaseTypeChecker<?> checker) {
            super(checker, VIndexAnnotatedTypeFactory.this);
        }

        /**
         * Case 2: concatenation of VIndex or PolyVIndex String/char literals.
         * Also handles concatenation of partial regular expressions.
         */
        @Override
        public Void visitBinary(BinaryTree tree, AnnotatedTypeMirror type) {
            if (!type.isAnnotatedInHierarchy(VINDEXTOP)
                && tree.getKind() == Tree.Kind.PLUS
                && TypesUtils.isDeclaredOfName(InternalUtils.typeOf(tree), "int")) {
                AnnotatedTypeMirror lExpr = getAnnotatedType(tree.getLeftOperand());
                AnnotatedTypeMirror rExpr = getAnnotatedType(tree.getRightOperand());
                Set<? extends AnnotationMirror> lubs = qualHierarchy.leastUpperBounds(lExpr.getAnnotations(), rExpr.getAnnotations());
                type.replaceAnnotations(lubs);
            }
            return null; // super.visitBinary(tree, type);
        }

    }
}
