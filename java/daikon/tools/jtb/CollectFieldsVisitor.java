package daikon.tools.jtb;

import java.util.*;
import jtb.syntaxtree.*;
import jtb.visitor.*;
import utilMDE.Assert;

// // Method "fieldDeclarations" returns a list of all FieldDeclarations
// // declared in this class or in nested/inner classes.
//
// This visitor previously included nested classes, but that didn't
// seem like the right thing to do; we should treat each class
// individually.

class CollectFieldsVisitor extends DepthFirstVisitor {

  private Vector<FieldDeclaration> fieldDecls = new Vector<FieldDeclaration>();
  private int cachedSize = -1;
  private FieldDeclaration[] fieldDeclsArray;
  private String[] allNamesArray;
  private String[] ownedNamesArray;
  private String[] finalNamesArray;

  private void updateCache() {
    if (cachedSize != fieldDecls.size()) {
      fieldDeclsArray = fieldDecls.toArray(new FieldDeclaration[0]);
      Vector<String> allNames = new Vector<String>();
      Vector<String> ownedNames = new Vector<String>();
      Vector<String> finalNames = new Vector<String>();
      for (int i=0; i<fieldDeclsArray.length; i++) {
        FieldDeclaration fd = fieldDeclsArray[i];
        boolean isFinal = hasModifier(fd, "final");
        Type fdtype = fd.f0;
	// See specification in Annotate.java for which fields are owned.
        // boolean isOwned = ! isPrimitive(fdtype);
        boolean isOwned = Ast.isArray(fdtype);
        {
          String name = name(fd.f1);
          allNames.add(name);
          if (isFinal)
            finalNames.add(name);
          if (isOwned)
            ownedNames.add(name);
        }
        NodeListOptional fds = fd.f2;
        if (fds.present()) {
          for (int j=0; j<fds.size(); j++) {
            // System.out.println("" + j + ": " + fds.elementAt(j));
            NodeSequence ns = (NodeSequence) fds.elementAt(j);
            if (ns.size() != 2) {
              System.out.println("Bad length " + ns.size() + " for NodeSequence");
            }
            String name = name((VariableDeclarator) ns.elementAt(1));
            allNames.add(name);
            if (isFinal)
              finalNames.add(name);
            if (isOwned)
              ownedNames.add(name);
          }
        }
      }
      allNamesArray = allNames.toArray(new String[0]);
      ownedNamesArray = ownedNames.toArray(new String[0]);
      finalNamesArray = finalNames.toArray(new String[0]);
      cachedSize = fieldDecls.size();
    }
  }

  private String name(VariableDeclarator n) {
    return n.f0.f0.tokenImage;
  }

  private boolean hasModifier(FieldDeclaration n, String mod) {
    return Ast.contains(n.f0, mod);
  }


  // Returns a list of all FieldDeclarations declared in this class or in
  // nested/inner classes.
  public FieldDeclaration[] fieldDeclarations() {
    updateCache();
    return fieldDeclsArray;
  }

  // Returns a list of all fields.
  public String[] allFieldNames() {
    updateCache();
    return allNamesArray;
  }

  // Returns a list of names of all fields with owner annotations.
  public String[] ownedFieldNames() {
    updateCache();
    return ownedNamesArray;
  }

  // Returns a list of all final fields.
  public String[] finalFieldNames() {
    updateCache();
    return finalNamesArray;
  }

  // Don't continue into nested classes, but do
  // explore them if they are the root.
  private boolean in_class = false;
  public void visit(ClassOrInterfaceDeclaration n) {
    //Assert.assertTrue(! in_class);
    in_class = true;
    super.visit(n);
    in_class = false;
  }
//   public void visit(NestedClassDeclaration n) {
//     if (! in_class) {
//       in_class = true;
//       super.visit(n);
//       in_class = false;
//     }
//   }

  /**
   * f0 -> ( "public" | "protected" | "private" | "static" | "final" | "transient" | "volatile" )*
   * f1 -> Type()
   * f2 -> VariableDeclarator()
   * f3 -> ( "," VariableDeclarator() )*
   * f4 -> ";"
   */
  public void visit(FieldDeclaration n) {
    fieldDecls.add(n);

    super.visit(n);             // call "accept(this)" on each field
  }

}
