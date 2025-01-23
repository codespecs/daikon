package daikon.tools.jtb;

import java.io.StringWriter;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import jtb.syntaxtree.*;
import jtb.visitor.*;
import org.checkerframework.checker.nullness.qual.KeyFor;

/** Replaces uses of generic type parameters with versions that do not use generics. */
public class ClassOrInterfaceTypeDecorateVisitor extends DepthFirstVisitor {

  // A map from token images to "ungenerified" versions of the classes
  // or interfaces that the given identifiers extend.
  HashMap<String, Deque<ClassOrInterfaceType>> shadowingMap = new HashMap<>();

  // For debugging purposes.
  @SuppressWarnings("UnusedMethod") // debugging code is currently commented out
  private void printShadowingMap() {
    System.out.println("Shadowing map:");
    for (Map.Entry<@KeyFor("shadowingMap") String, Deque<ClassOrInterfaceType>> e :
        shadowingMap.entrySet()) {
      System.out.print("  " + e.getKey() + " stack: ");
      for (ClassOrInterfaceType t : e.getValue()) {
        StringWriter w = new StringWriter();
        t.accept(new TreeFormatter());
        t.accept(new TreeDumper(w));
        System.out.print(w.toString().trim() + " ");
      }
      System.out.println();
    }
  }

  // f0 -> [ TypeParameters() ]
  // f1 -> ResultType()
  // f2 -> MethodDeclarator()
  // f3 -> [ "throws" NameList() ]
  // f4 -> ( Block() | ";" )
  @Override
  public void visit(MethodDeclaration n) {

    // A shallow clone, which is what we want.
    HashMap<String, Deque<ClassOrInterfaceType>> oldShadowingMap = copy(shadowingMap);

    if (n.f0.present()) {
      augmentShadowingMap((TypeParameters) n.f0.node);
    }
    n.f1.accept(this);
    n.f2.accept(this);
    n.f3.accept(this);
    n.f4.accept(this);

    // Restore shadowing map because we're going out of scope from
    // the TypeParameters declared in this method.
    shadowingMap = oldShadowingMap;
    // printShadowingMap();
  }

  // f0 -> [ TypeParameters() ]
  // f1 -> <IDENTIFIER>
  // f2 -> FormalParameters()
  // f3 -> [ "throws" NameList() ]
  // f4 -> "{"
  // f5 -> [ ExplicitConstructorInvocation() ]
  // f6 -> ( BlockStatement() )*
  // f7 -> "}"
  @Override
  public void visit(ConstructorDeclaration n) {

    // A shallow clone, which is what we want.
    HashMap<String, Deque<ClassOrInterfaceType>> oldShadowingMap = copy(shadowingMap);

    if (n.f0.present()) {
      augmentShadowingMap((TypeParameters) n.f0.node);
    }
    n.f1.accept(this);
    n.f2.accept(this);
    n.f3.accept(this);
    n.f4.accept(this);
    n.f5.accept(this);
    n.f6.accept(this);
    n.f7.accept(this);

    // Restore shadowing map because we're going out of scope from
    // the TypeParameters declared in this method.
    shadowingMap = oldShadowingMap;
    // printShadowingMap();
  }

  // f0 -> ( "class" | "interface" )
  // f1 -> <IDENTIFIER>
  // f2 -> [ TypeParameters() ]
  // f3 -> [ ExtendsList(isInterface) ]
  // f4 -> [ ImplementsList(isInterface) ]
  // f5 -> ClassOrInterfaceBody(isInterface)
  @Override
  public void visit(ClassOrInterfaceDeclaration n) {

    // A shallow clone, which is what we want.
    HashMap<String, Deque<ClassOrInterfaceType>> oldShadowingMap = copy(shadowingMap);

    n.f0.accept(this);
    n.f1.accept(this);

    if (n.f2.present()) {
      augmentShadowingMap((TypeParameters) n.f2.node);
    }

    n.f2.accept(this);
    n.f3.accept(this);
    n.f4.accept(this);
    n.f5.accept(this);

    // Restore shadowing map because we're going out of scope from
    // the TypeParameters declared in this method.
    shadowingMap = oldShadowingMap;
    // printShadowingMap();
  }

  public void augmentShadowingMap(TypeParameters n) {
    // Grammar production:
    // f0 -> "<"
    // f1 -> TypeParameter()
    // f2 -> ( "," TypeParameter() )*
    // f3 -> ">"
    final List<TypeParameter> params = new ArrayList<>();
    n.accept(
        new DepthFirstVisitor() {
          @Override
          public void visit(TypeParameter n) {
            params.add(n);
          }
        });

    for (TypeParameter t : params) {
      augmentShadowingMap(t);
    }
  }

  // f0 -> <IDENTIFIER>
  // f1 -> [ TypeBound() ]
  public void augmentShadowingMap(TypeParameter n) {
    n.f0.accept(this);
    n.f1.accept(this);
    TypeBound b = (TypeBound) n.f1.node;

    if (n.f1.present()) {

      // Grammar production for TypeBound:
      // f0 -> "extends"
      // f1 -> ClassOrInterfaceType()
      // f2 -> ( "&" ClassOrInterfaceType() )*

      // TODO figure out how/whether to handle f2 (currently it's just ignored).

      assert b.f1.unGenerifiedVersionOfThis != null;

      Deque<ClassOrInterfaceType> s =
          shadowingMap.computeIfAbsent(
              n.f0.tokenImage, __ -> new ArrayDeque<ClassOrInterfaceType>());
      s.push(b.f1.unGenerifiedVersionOfThis);

    } else {

      // No explicit bound means that bound is java.lang.Object.

      Deque<ClassOrInterfaceType> s =
          shadowingMap.computeIfAbsent(
              n.f0.tokenImage, __ -> new ArrayDeque<ClassOrInterfaceType>());

      ClassOrInterfaceType objectType =
          (ClassOrInterfaceType) Ast.create("ClassOrInterfaceType", "Object");
      s.push(objectType);
    }

    // printShadowingMap();
  }

  // ClassOrInterfaceType:
  // f0 -> <IDENTIFIER>
  // f1 -> [ TypeArguments() ]
  // f2 -> ( "." <IDENTIFIER> [ TypeArguments() ] )*
  @Override
  public void visit(ClassOrInterfaceType t) {
    t.f0.accept(this);
    t.f1.accept(this); // NO NEED TO DO THIS?
    t.f2.accept(this);

    // Make a copy of the ClassOrInterfaceType.
    StringWriter w = new StringWriter();
    // t.accept(new TreeFormatter());
    t.accept(new TreeDumper(w));
    ClassOrInterfaceType n =
        (ClassOrInterfaceType) Ast.create("ClassOrInterfaceType", w.toString());

    ungenerify(n);

    t.unGenerifiedVersionOfThis = n;
  }

  // ClassOrInterfaceType:
  // f0 -> <IDENTIFIER>
  // f1 -> [ TypeArguments() ]
  // f2 -> ( "." <IDENTIFIER> [ TypeArguments() ] )*
  @SuppressWarnings("JdkObsolete") // JTB uses Vector
  private void ungenerify(ClassOrInterfaceType n) {

    // Drop all type arguments.
    n.f1 = new NodeOptional(); // This removes optional node, if present.
    List<Node> nodeSequenceList = n.f2.nodes;
    for (int i = 0; i < nodeSequenceList.size(); i++) {
      NodeSequence oldSequence = (NodeSequence) nodeSequenceList.get(i);
      NodeSequence newSequence = new NodeSequence(3);
      newSequence.addNode(oldSequence.elementAt(0)); // "."
      newSequence.addNode(oldSequence.elementAt(1)); // <IDENTIFIER>
      newSequence.addNode(new NodeOptional()); // get rid of type arguments
      n.f2.nodes.set(i, newSequence);
    }

    // 2. Only the first <IDENTIFIER> may possibly be associated
    //    with a type argument. If we find it in typeParametersInScope,
    //    we replace t with [...]
    for (Map.Entry<@KeyFor("shadowingMap") String, Deque<ClassOrInterfaceType>> entry :
        shadowingMap.entrySet()) {
      if (entry.getKey().equals(n.f0.tokenImage)) {
        ClassOrInterfaceType c = entry.getValue().getFirst();
        // System.out.println("c:" + Ast.format(c));
        List<Node> cSequence = c.f2.nodes;
        // System.out.print("cSequence:");
        // for (Node n2 : cSequence) {
        //   System.out.print(Ast.format(n2) + " ");
        // }
        // Prepend all-but-first identifiers to the list of identifiers in f2.
        // Prepending in reverse order ensures the right prepending order.
        for (int i = cSequence.size() - 1; i >= 0; i--) {
          nodeSequenceList.add(0, cSequence.get(i));
        }
        // Set f0 to the first identifier.
        n.f0 = c.f0;
      }
    }

    {
      // StringWriter sw = new StringWriter();
      // n.accept(new TreeFormatter());
      // t.accept(new TreeDumper(sw));
      // System.out.print("t::::");
      // System.out.println(sw.toString().trim());
    }
    {
      // StringWriter sw = new StringWriter();
      // n.accept(new TreeFormatter());
      // n.accept(new TreeDumper(sw));
      // System.out.print("n::::");
      // System.out.println(sw.toString().trim());
    }
  }

  /**
   * Makes a copy of the stacks and of the map. The ClassOrInterfaceType objects are not copied.
   *
   * @param m a map
   * @return a copy of the map
   */
  @SuppressWarnings("NonApiType") // https://errorprone.info/bugpattern/NonApiType
  private static HashMap<String, Deque<ClassOrInterfaceType>> copy(
      HashMap<String, Deque<ClassOrInterfaceType>> m) {

    HashMap<String, Deque<ClassOrInterfaceType>> newMap = new HashMap<>();

    for (Map.Entry<@KeyFor("m") String, Deque<ClassOrInterfaceType>> e : m.entrySet()) {
      String key = e.getKey();
      Deque<ClassOrInterfaceType> oldStack = e.getValue();
      Deque<ClassOrInterfaceType> newStack =
          new ArrayDeque<ClassOrInterfaceType>(oldStack); // clone
      newMap.put(key, newStack);
    }

    return newMap;
  }
}
