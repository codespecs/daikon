package typequals.vindex;

import com.sun.source.tree.BinaryTree;
import com.sun.source.tree.Tree;
import java.util.Set;
import javax.lang.model.element.AnnotationMirror;
import org.checkerframework.common.basetype.BaseAnnotatedTypeFactory;
import org.checkerframework.common.basetype.BaseTypeChecker;
import org.checkerframework.framework.type.AnnotatedTypeMirror;
import org.checkerframework.framework.type.treeannotator.TreeAnnotator;
import org.checkerframework.javacutil.AnnotationBuilder;
import org.checkerframework.javacutil.TreeUtils;
import org.checkerframework.javacutil.TypesUtils;

/**
 * Adds a type qualifier from the VIndex type system to the type of tree, in the following cases:
 * [TODO]
 */
public class VIndexAnnotatedTypeFactory extends BaseAnnotatedTypeFactory {

  private final AnnotationMirror VINDEXTOP;

  public VIndexAnnotatedTypeFactory(BaseTypeChecker checker) {
    super(checker);
    VINDEXTOP = AnnotationBuilder.fromClass(elements, VIndexTop.class);
    this.postInit();
  }

  @Override
  public TreeAnnotator createTreeAnnotator() {
    return new VIndexTreeAnnotator(this);
  }

  private class VIndexTreeAnnotator extends TreeAnnotator {

    public VIndexTreeAnnotator(VIndexAnnotatedTypeFactory atypeFactory) {
      super(atypeFactory);
    }

    /**
     * Case 2: concatenation of VIndex or PolyVIndex String/char literals. Also handles
     * concatenation of partial regular expressions.
     */
    @Override
    public Void visitBinary(BinaryTree tree, AnnotatedTypeMirror type) {
      if (!type.isAnnotatedInHierarchy(VINDEXTOP)
          && tree.getKind() == Tree.Kind.PLUS
          && TypesUtils.isDeclaredOfName(TreeUtils.typeOf(tree), "int")) {
        AnnotatedTypeMirror lExpr = getAnnotatedType(tree.getLeftOperand());
        AnnotatedTypeMirror rExpr = getAnnotatedType(tree.getRightOperand());
        Set<? extends AnnotationMirror> lubs =
            qualHierarchy.leastUpperBounds(lExpr.getAnnotations(), rExpr.getAnnotations());
        type.replaceAnnotations(lubs);
      }
      return null; // super.visitBinary(tree, type);
    }
  }
}
