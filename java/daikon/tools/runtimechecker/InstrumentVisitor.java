package daikon.tools.runtimechecker;

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Vector;
import java.util.*;

import jtb.syntaxtree.BlockStatement;
import jtb.syntaxtree.ClassBody;
import jtb.syntaxtree.ClassBodyDeclaration;
import jtb.syntaxtree.ConstructorDeclaration;
import jtb.syntaxtree.FieldDeclaration;
import jtb.syntaxtree.FormalParameter;
import jtb.syntaxtree.MethodDeclaration;
import jtb.syntaxtree.NameList;
import jtb.syntaxtree.NestedClassDeclaration;
import jtb.syntaxtree.Node;
import jtb.syntaxtree.NodeChoice;
import jtb.syntaxtree.NodeListOptional;
import jtb.syntaxtree.NodeOptional;
import jtb.syntaxtree.NodeSequence;
import jtb.syntaxtree.NodeToken;
import jtb.syntaxtree.UnmodifiedClassDeclaration;
import jtb.visitor.DepthFirstVisitor;
import jtb.visitor.TreeDumper;
import jtb.visitor.TreeFormatter;
import utilMDE.Assert;
import utilMDE.UtilMDE;
import daikon.PptMap;
import daikon.PptTopLevel;
import daikon.inv.Invariant;
import daikon.inv.OutputFormat;
import daikon.tools.jtb.Ast;

/**
 * Visitor pattern that instruments a Java source file to check invariant
 * violations at runtime.
 */
public class InstrumentVisitor extends DepthFirstVisitor {

    // If true, instrumented will make all fields of the class
    // visible. The reason for doing this is so that invariants over
    // potentially inaccessible object fields can be evaluated.
    boolean makeAllFieldsVisible = false;

    // The map containing the invariants.
    private final PptMap pptmap;

    // [[ TODO: I'm using xmlString() because it will definitely give
    // different values for different Properties. But if Properties
    // are in fact unique and immutable, and if I ensure that hashcode
    // returns unique values for unique Properties, then I should be
    // able to use hashcode. So: make sure that the statements above
    // are all correct, and then use hashcode. ]]
    private Map/*<String, String>*/ xmlStringToIndex = new HashMap/*<String,String>*/();

    private int varNumCounter = 0;

    public InstrumentVisitor(PptMap pptmap) {
        this.pptmap = pptmap;

        for (Iterator i = pptmap.pptIterator() ; i.hasNext() ; ) {
            PptTopLevel ppt = (PptTopLevel)i.next();

            if (ppt.ppt_name.isExitPoint()
		&& !ppt.ppt_name.isCombinedExitPoint()) {
		continue;
	    }

            List/*Invariant*/ invList = filterInvariants(daikon.tools.jtb.Ast.getInvariants(ppt, pptmap));
            for (Iterator invI = invList.iterator() ; invI.hasNext() ; ) {
                Invariant inv = (Invariant)invI.next();

		xmlStringToIndex.put(xmlString(inv), Integer.toString(varNumCounter));
		varNumCounter++;
	    }
	}
    }

    public void visit(FieldDeclaration fd) {
        super.visit(fd);

	// f0 -> ( "public" | "protected" | "private" | "static" | "final" | "transient" | "volatile" )*
	Vector modifiers = fd.f0.nodes;

	Vector/*NodeChoice*/ newModifiers = new Vector();
	for (int i = 0 ; i < modifiers.size() ; i++) {
	    NodeChoice nc = (NodeChoice)modifiers.get(i);
	    NodeToken token = (NodeToken)nc.choice;
	    if (!token.tokenImage.equals("public")
		&& !token.tokenImage.equals("protected")
		&& !token.tokenImage.equals("private")) {
		newModifiers.add(token);
	    }
	}

	newModifiers.add(new NodeToken("public"));
	fd.f0.nodes = newModifiers;
    }

    public void visit(ClassBody clazz) {
        super.visit(clazz);

        // add method to check object and class invariants.
        UnmodifiedClassDeclaration ucd = (UnmodifiedClassDeclaration) clazz
                .getParent();
        String classname = Ast.getClassName(ucd);
        ClassBodyDeclaration objInvDecl = checkObjectInvariants_instrumentDeclaration(classname);
        Ast.addDeclaration(clazz, objInvDecl);

        boolean isNested = false;
        boolean isStatic = false;
        Node dParent = ucd.getParent();
        if (dParent instanceof NestedClassDeclaration) {
            isNested = true;
            isStatic = Ast.isStatic((NestedClassDeclaration) dParent);
        }

        if (!isNested || (isStatic)) {
            ClassBodyDeclaration classInvDecl = checkClassInvariantsInstrumentDeclaration(classname);
            Ast.addDeclaration(clazz, classInvDecl);
            Ast.addDeclaration(clazz, getInvariantsDecl());
            Ast.addDeclaration(clazz, isInstrumentedDecl());
	    Ast.addDeclaration(clazz, staticPropertyDecl());
	    Ast.addDeclaration(clazz, staticPropertyInit());
        }
    }

    /**
     *
     */
    public void visit(ConstructorDeclaration ctor) {
        super.visit(ctor);

        // Find declared throwables.
        List/* String */declaredThrowables = getDeclaredThrowables(ctor.f3);

        Vector/* PptTopLevel */matching_ppts = Ast.getMatches(pptmap, ctor);

        StringBuffer code = new StringBuffer();

        NodeListOptional ctorBody = ctor.f6;

        // Create the code for a new BlockStatement that will be the new
        // body of the constructor.
        code.append("{");

        // Check class invariants.
        code.append("checkClassInvariantsInstrument(daikon.tools.runtimechecker.Violation.Time.onEntry);");

        // Check preconditions.
        checkPreconditions(code, matching_ppts, pptmap);

        // Check preconditions.
        for (Iterator i = matching_ppts.iterator(); i.hasNext();) {
            PptTopLevel ppt = (PptTopLevel) i.next();
            if (ppt.ppt_name.isEnterPoint()) {
                List/* Invariant */preconditions = filterInvariants(Ast
                        .getInvariants(ppt, pptmap));
                appendInvariantChecks(preconditions, code, "daikon.tools.runtimechecker.Violation.Time.onEntry");
            }
        }

        // Call original constructor code, wrapped in a try clause so that
        // we can evaluate object/class invariants before exiting, even
        // if an exception is thrown.
        code.append("boolean methodThrewSomething_instrument = false;");

        code.append("try {");

        // Insert original constructor code.
        // [[ TODO: should I use the daikon dumper that Mike found? ]]
        StringWriter stringWriter = new StringWriter();
        TreeDumper dumper = new TreeDumper(stringWriter);
        dumper.visit(ctorBody);
        code.append(stringWriter.toString());

        // Handle any exceptions, check postconditions, object and class
        // invariants.
        exitChecks(code, matching_ppts, pptmap, declaredThrowables, false);

        code.append("}");

        // Replace constructor body with instrumented code.
        BlockStatement newCtorBody = (BlockStatement) Ast.create(
                "BlockStatement", code.toString());
        newCtorBody.accept(new TreeFormatter(2, 0));
        ctor.f6 = new NodeListOptional(newCtorBody);
    }

    // Methods that we have created
    private final Set generated_methods = new HashSet();

    /**
     *
     */
    public void visit(MethodDeclaration method) {
        super.visit(method);

        if (generated_methods.contains(method)) {
            return;
        }

        // Determine if method is static.
        boolean isStatic = false;
        for (Enumeration e = method.f0.elements(); e.hasMoreElements();) {
            NodeChoice nodechoice = (NodeChoice) e.nextElement();
            NodeToken modifier = (NodeToken) nodechoice.choice;
            if (modifier.tokenImage.equals("static")) {
                isStatic = true;
            }
        }

        // Find declared throwables.
        List/* String */declaredThrowables = getDeclaredThrowables(method.f3);

        Vector/* PptTopLevel */matching_ppts = Ast.getMatches(pptmap, method);
        String name = Ast.getName(method);
        String returnType = Ast.getReturnType(method);
        String maybeReturn = (returnType.equals("void") ? "" : "return");
        Vector parameters = new Vector();
        for (Iterator i = Ast.getParameters(method).iterator(); i.hasNext();) {
            FormalParameter param = (FormalParameter) i.next();
            parameters.add(Ast.getName(param));
        }

        StringBuffer code = new StringBuffer();
        code.append("{");

        // Check object invariants.
        if (!isStatic) {
            code.append("checkObjectInvariants_instrument(daikon.tools.runtimechecker.Violation.Time.onEntry);");
        }

        // Check class invariants.
        code.append("checkClassInvariantsInstrument(daikon.tools.runtimechecker.Violation.Time.onEntry);");

        checkPreconditions(code, matching_ppts, pptmap);

        // Check preconditions.
        for (Iterator i = matching_ppts.iterator(); i.hasNext();) {
            PptTopLevel ppt = (PptTopLevel) i.next();
            if (ppt.ppt_name.isEnterPoint()) {
                List/* Invariant */preconditions = filterInvariants(Ast
                        .getInvariants(ppt, pptmap));
                appendInvariantChecks(preconditions, code, "daikon.tools.runtimechecker.Violation.Time.onEntry");
            }
        }

        // Call original method, wrapped in a try clause so that we
        // can evaluate object/class invariants before exiting.
        code.append("boolean methodThrewSomething_instrument = false;");
        if (!returnType.equals("void")) {
            code.append(returnType + " retval_instrument = ");

            // Assign some initial value to retval_instrument, otherwise
            // compiler
            // issues a "might not have been initialized" error.
            if (returnType.equals("boolean")) {
                code.append("false");
            } else if (returnType.equals("char")) {
                code.append("'a'");
            } else if (returnType.equals("byte") || returnType.equals("double")
                    || returnType.equals("float") || returnType.equals("int")
                    || returnType.equals("long") || returnType.equals("short")) {
                code.append("0");
            } else {
                code.append("null");
            }

            code.append(";");
        }

        code.append("try {");

        if (!returnType.equals("void")) {
            code.append("retval_instrument = ");
        }

        code.append("internal$" + name + "(" + UtilMDE.join(parameters, ", ")
                + ");");

        exitChecks(code, matching_ppts, pptmap, declaredThrowables, isStatic);

        // Return value.
        if (!returnType.equals("void")) {
            code.append("return retval_instrument;");
        }

        code.append("}");

        // Add method to AST.
        String new_method = code.toString();
        MethodDeclaration wrapper = (MethodDeclaration) Ast.copy(
                "MethodDeclaration", method);
        Ast.setBody(wrapper, new_method);
        wrapper.accept(new TreeFormatter(2, 0));
        ClassBody c = (ClassBody) Ast.getParent(ClassBody.class, method);
        ClassBodyDeclaration d = (ClassBodyDeclaration) Ast.create(
                "ClassBodyDeclaration", Ast.print(wrapper));
        Ast.addDeclaration(c, d);
        MethodDeclaration generated_method = (MethodDeclaration) d.f0.choice;
        generated_methods.add(generated_method);

        // Rename the original method, and make it private.
        Ast.setName(method, "internal$" + name);
        Ast.setAccess(method, "private");
    }

    // vioTime can be the name of a variable (which should be in scope)
    // or a string, but if it's a string, then you should write it as
    // something like: \"<ENTER>\"
    private void appendInvariantChecks(List/* Invariant */invs,
            StringBuffer code, String vioTime) {
        for (Iterator i = invs.iterator(); i.hasNext();) {
            Invariant inv = (Invariant) i.next();

            String javarep = inv.format_using(OutputFormat.JAVA);

            String xmlString = xmlString(inv);

            // [[ TODO : explain this. ]]
            // [[ TODO : move this to filterInvariants method. ]]
            if (xmlString.indexOf("orig(") != -1) {
                continue;
            }

            if (javarep.indexOf("unimplemented") != -1) {
                // [[ TODO: print a warning that some invariants were
                // unimplemented. ]]
                continue;
            }

            if (javarep.indexOf("\\result") != -1) {
                javarep = javarep.replaceAll("\\\\result", "retval_instrument");
            }

            code.append("try {" + daikon.Global.lineSep + "");
            code.append("if (!(" + daikon.Global.lineSep + "");
            code.append(javarep);
            code.append(")) {");
            code.append("daikon.tools.runtimechecker.Runtime.violationsAdd(daikon.tools.runtimechecker.Violation.get(daikonProperties[");
	    code.append(xmlStringToIndex.get(xmlString) + "], " + vioTime);
            code.append("));");
            code.append("}");
            code.append("} catch (RuntimeException t_instrument) {"
                    + daikon.Global.lineSep + "");
            code
                    .append("  daikon.tools.runtimechecker.Runtime.internalInvariantEvaluationErrors.add(t_instrument);");
            //code.append("  throw t_instrument;");
            code
                    .append("} catch (Error t_instrument) {" + daikon.Global.lineSep
                            + "");
            code
                    .append("  daikon.tools.runtimechecker.Runtime.internalInvariantEvaluationErrors.add(t_instrument);");
            //code.append("  throw t_instrument;");
            code.append("}" + daikon.Global.lineSep + "");
        }
    }


    /**
     * @return
     */
    private ClassBodyDeclaration isInstrumentedDecl() {
        StringBuffer code = new StringBuffer();
        code.append("public static boolean isDaikonInstrumented() { return true; }");
        return (ClassBodyDeclaration) Ast.create("ClassBodyDeclaration", code
                .toString());
    }

    /**
     * @return
     */
    private ClassBodyDeclaration getInvariantsDecl() {
        StringBuffer code = new StringBuffer();
        code.append("public static java.util.Set getDaikonInvariants() {");
        code.append("  return new java.util.HashSet(java.util.Arrays.asList(daikonProperties));");
        code.append("}");
        return (ClassBodyDeclaration) Ast.create("ClassBodyDeclaration", code
                .toString());
    }

    /**
     * @return
     */
    private ClassBodyDeclaration staticPropertyDecl() {
        StringBuffer code = new StringBuffer();

	code.append("private static daikon.tools.runtimechecker.Property[] daikonProperties;");
        return (ClassBodyDeclaration) Ast.create("ClassBodyDeclaration", code
						 .toString());
    }

    private ClassBodyDeclaration staticPropertyInit() {
        StringBuffer code = new StringBuffer();

	code.append("static {\n");
	code.append("try {\n");
	code.append("daikonProperties = new daikon.tools.runtimechecker.Property[" + varNumCounter  + "];\n");

        for (Iterator i = xmlStringToIndex.entrySet().iterator() ; i.hasNext() ; ) {
            Map.Entry e = (Map.Entry)i.next();
            code.append("daikonProperties[" + e.getValue() + "] = ");
            code.append("daikon.tools.runtimechecker.Property.get(");
            code.append("\"");
            code.append(e.getKey());
            code.append("\"");
            code.append(");\n");
	}

        code.append("} catch (Exception e) {");
        code.append(" System.err.println(\"malformed invariant. This is probably a bug in the daikon.tools.runtimechecker tool; please submit a bug report.\");");
        code.append(" e.printStackTrace();");
        code.append(" System.exit(1);\n");
        code.append("}");

        code.append("} // end static");

        return (ClassBodyDeclaration) Ast.create("ClassBodyDeclaration", code
						 .toString());
    }



    private ClassBodyDeclaration checkObjectInvariants_instrumentDeclaration(
            String classname) {
        StringBuffer code = new StringBuffer();
        code
                .append("private void checkObjectInvariants_instrument(daikon.tools.runtimechecker.Violation.Time time) {");
        String objectPptname = classname + ":::OBJECT";
        PptTopLevel objectPpt = pptmap.get(objectPptname);
        if (objectPpt != null) {
            List/* Invariant */objectInvariants = filterInvariants(Ast
                    .getInvariants(objectPpt, pptmap));
            appendInvariantChecks(objectInvariants, code, "time");
        }
        code.append("}" + daikon.Global.lineSep + "");
        return (ClassBodyDeclaration) Ast.create("ClassBodyDeclaration", code
                .toString());
    }

    private ClassBodyDeclaration checkClassInvariantsInstrumentDeclaration(
            String classname) {
        StringBuffer code = new StringBuffer();
        code
                .append("private static void checkClassInvariantsInstrument(daikon.tools.runtimechecker.Violation.Time time) {");
        String classPptname = classname + ":::CLASS";
        PptTopLevel classPpt = pptmap.get(classPptname);
        if (classPpt != null) {
            List/* Invariant */classInvariants = filterInvariants(Ast
                    .getInvariants(classPpt, pptmap));
            appendInvariantChecks(classInvariants, code, "time");
        }
        code.append("}" + daikon.Global.lineSep + "");
        return (ClassBodyDeclaration) Ast.create("ClassBodyDeclaration", code
                .toString());
    }

    private static List/* Invariant */filterInvariants(
            List/* Invariant */invariants) {
        List/* Invariant */survivors = new ArrayList();
        for (Iterator i = invariants.iterator(); i.hasNext();) {
            Invariant inv = (Invariant) i.next();



            String invString = inv.format_using(OutputFormat.JAVA);
            // These are carry-overs from Annotate. Some, like "~", should
            // actually be implemented, not removed.
            if ((invString.indexOf("warning: ") != -1)
                    || (invString.indexOf('~') != -1)
                    || (invString.indexOf("\\new") != -1)
                    || (invString.indexOf(".toString ") != -1)
                    || (invString.indexOf(".getClass()") != -1)
                    || (invString.indexOf("Quant.typeArray") != -1)
                    || (invString.indexOf("warning: method") != -1)
                    || (invString.indexOf("inexpressible") != -1)
                    || (invString.indexOf("unimplemented") != -1)) {
                continue;
            }
            survivors.add(inv);
        }
        return survivors;
    }

    private static List/* String */getDeclaredThrowables(NodeOptional nodeOpt) {
        List/* String */declaredThrowables = new ArrayList();
        if (nodeOpt.present()) {
            NodeSequence seq = (NodeSequence) nodeOpt.node;
            // There should only be two elements: "throws" and NameList
            Assert.assertTrue(seq.size() == 2);
            NameList nameList = (NameList) seq.elementAt(1);

            StringWriter stringWriter = new StringWriter();
            TreeDumper dumper = new TreeDumper(stringWriter);
            dumper.visit(nameList);

            String[] declaredThrowablesArray = stringWriter.toString().trim()
                    .split(",");
            for (int i = 0; i < declaredThrowablesArray.length; i++) {
                declaredThrowables.add(declaredThrowablesArray[i].trim());
            }
            //       System.out.println("@@@");
            //       for (int i = 0 ; i < declaredThrowables.size() ; i++) {
            //         System.out.println("xxx" + declaredThrowables.get(i) + "xxx");
            //       }
        }
        return declaredThrowables;
    }

    // [[ TODO: This method can return the wrong sequence of catch clauses,
    //    because it has no concept of throwable subclasses, and it just
    //    orders catch clauses in the order in which they appear in
    //    declaredThrowable. This can cause compilation to fail. ]]
    private void exitChecks(StringBuffer code,
            Vector/* PptTopLevel */matching_ppts, PptMap pptmap,
	   List/*String*/ declaredThrowables, boolean isStatic) {

	List/*String*/ declaredThrowablesLocal = new ArrayList(declaredThrowables);
	declaredThrowablesLocal.remove("java.lang.RuntimeException");
	declaredThrowablesLocal.remove("RuntimeException");
	declaredThrowablesLocal.remove("java.lang.Error");
	declaredThrowablesLocal.remove("Error");

        // [[ TODO: Figure out what could go wrong here (e.g. what if
        // method declaration says "throws Throwable") and prepare for
        // it. ]]
        for (Iterator i = declaredThrowablesLocal.iterator(); i.hasNext();) {
            String declaredThrowable = (String) i.next();
            code.append("} catch (" + declaredThrowable + " t_instrument) {");
            code.append("  methodThrewSomething_instrument = true;");
            code.append("  throw t_instrument;");
        }

	code.append("} catch (java.lang.RuntimeException t_instrument) {");
	code.append("  methodThrewSomething_instrument = true;");
	code.append("  throw t_instrument;");
	code.append("} catch (java.lang.Error t_instrument) {");
	code.append("  methodThrewSomething_instrument = true;");
	code.append("  throw t_instrument;");

        code.append("} finally {");

        // If method didn't throw an exception, it completed
        // normally--check method postconditions (If the method didn't
        // complete normally, it makes no sense to check postconditions,
        // because Daikon only reports normal-exit postconditions.)
        code.append(" if (!methodThrewSomething_instrument) {");

        // Check postconditions.
        for (Iterator i = matching_ppts.iterator(); i.hasNext();) {
            PptTopLevel ppt = (PptTopLevel) i.next();
            if (ppt.ppt_name.isExitPoint()
		&& ppt.ppt_name.isCombinedExitPoint()) {
                List/* Invariant */postconditions = filterInvariants(Ast
								     .getInvariants(ppt, pptmap));
                appendInvariantChecks(postconditions, code, "daikon.tools.runtimechecker.Violation.Time.onExit");
            }
        }
        code.append("}");

        // Check object invariants.
        if (!isStatic) {
            code.append("checkObjectInvariants_instrument(daikon.tools.runtimechecker.Violation.Time.onExit);");
        }
        // Check class invariants.
        code.append("checkClassInvariantsInstrument(daikon.tools.runtimechecker.Violation.Time.onExit);");

        code.append("}"); // this closes the finally clause
    }

    private void checkPreconditions(StringBuffer code,
            Vector/* PptTopLevel */matching_ppts, PptMap pptmap) {
        for (Iterator i = matching_ppts.iterator(); i.hasNext();) {
            PptTopLevel ppt = (PptTopLevel) i.next();
            if (ppt.ppt_name.isEnterPoint()) {
                List/* Invariant */preconditions = filterInvariants(Ast
                        .getInvariants(ppt, pptmap));
                appendInvariantChecks(preconditions, code, "daikon.tools.runtimechecker.Violation.Time.onEntry");
            }
        }
    }

    private static String xmlString(Invariant inv) {
        StringBuffer code = new StringBuffer();

        String daikonrep = inv.format_using(OutputFormat.DAIKON);
        if (daikonrep.indexOf("\"") != -1 || daikonrep.indexOf("\\") != -1) {
            // Now comes some real ugliness: [[ ... ]] It's easier to do
            // this transformation on a character list than by pattern
            // matching against a String.
            char[] chars = daikonrep.toCharArray();
            List/* Character */charList = new ArrayList();
            for (int j = 0; j < chars.length; j++) {
                char c = chars[j];
                if ((c == '\"') || (c == '\\')) {
                    charList.add(new Character('\\'));
                }
                charList.add(new Character(c));
            }
            char[] backslashedChars = new char[charList.size()];
            for (int j = 0; j < charList.size(); j++) {
                backslashedChars[j] = ((Character) charList.get(j))
                    .charValue();
            }
            daikonrep = new String(backslashedChars);
        }


        code.append("<INVINFO>");
        code.append("<" + inv.ppt.parent.ppt_name.getPoint() + ">");
        code.append("<DAIKON>" + daikonrep + "</DAIKON>");
        code.append("<DAIKONCLASS>" + inv.getClass().toString()
                    + "</DAIKONCLASS>");
        code.append("<METHOD>" + inv.ppt.parent.ppt_name.getSignature()
                    + "</METHOD>");
        code.append("</INVINFO>");
        return code.toString();
    }

}
