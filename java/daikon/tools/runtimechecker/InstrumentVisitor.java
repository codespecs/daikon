package daikon.tools.runtimechecker;

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.List;
import java.util.*;

import jtb.syntaxtree.*;
import jtb.visitor.DepthFirstVisitor;
import jtb.visitor.TreeDumper;
import jtb.visitor.TreeFormatter;
import utilMDE.Assert;
import utilMDE.UtilMDE;
import daikon.PptMap;
import daikon.PptTopLevel;
import daikon.inv.Invariant;
import daikon.inv.OutputFormat;
import daikon.inv.ternary.threeScalar.FunctionBinary;
import daikon.tools.jtb.*;

/**
 * Visitor that instruments a Java source file (i.e. adds code at
 * certain places) to check invariant violations at runtime.
 */
public class InstrumentVisitor extends DepthFirstVisitor {

    // If true, the instrumenter will make all fields of the class
    // visible. The reason for doing this is so that invariants over
    // potentially inaccessible object fields can be evaluated.
    public static boolean makeAllFieldsPublic = false;

    // The map containing the invariants.
    private final PptMap pptmap;

    // [[ TODO: I'm using xmlString() because it will definitely give
    // different values for different Properties. But if Properties
    // are in fact unique and immutable, and if I ensure that hashcode
    // returns unique values for unique Properties, then I should be
    // able to use hashcode. So: make sure that the statements above
    // are all correct, and then use hashcode. ]]
    private Map<String, String> xmlStringToIndex = new HashMap<String,String>();

    private int varNumCounter = 0;

    private PptNameMatcher pptMatcher;

    /**
     * Create a visitor that will insert code to check the invariants
     * contained in pptmap.
     */
    public InstrumentVisitor(PptMap pptmap, TypeDeclaration root) {
        this.pptmap = pptmap;
        this.pptMatcher = new PptNameMatcher(root);

        for (Iterator i = pptmap.pptIterator() ; i.hasNext() ; ) {
            PptTopLevel ppt = (PptTopLevel)i.next();

            if (ppt.ppt_name.isExitPoint()
		&& !ppt.ppt_name.isCombinedExitPoint()) {
		continue;
	    }

            List<Invariant> invList = filterInvariants(daikon.tools.jtb.Ast.getInvariants(ppt, pptmap));
            for (Iterator invI = invList.iterator() ; invI.hasNext() ; ) {
                Invariant inv = (Invariant)invI.next();

		xmlStringToIndex.put(xmlString(inv), Integer.toString(varNumCounter));
		varNumCounter++;
	    }
	}
    }

    /**
     * If makeAllFieldsPublic == true, then it makes this field
     * declaration public.
     */
    public void visit(FieldDeclaration fd) {

        // Fix any line/col inconsistencies first
        fd.accept(new TreeFormatter());

        super.visit(fd);
        /**
         * Grammar production for ClassOrInterfaceBodyDeclaration:
         * f0 -> Initializer()
         *       | Modifiers() ( ClassOrInterfaceDeclaration(modifiers) | EnumDeclaration(modifiers)
         *                       | ConstructorDeclaration() | FieldDeclaration(modifiers)
         *                       | MethodDeclaration(modifiers) )
         *       | ";"
         */

        if (makeAllFieldsPublic) {
            NodeSequence seq = (NodeSequence)fd.getParent().getParent();
            Modifiers modifiers = (Modifiers)seq.elementAt(0);
            List modifierList = modifiers.f0.nodes;


            List<Node> newModifiers = new ArrayList<Node>();
            for (int i = 0 ; i < modifierList.size() ; i++) {
                NodeChoice nc = (NodeChoice)modifierList.get(i);
                NodeToken token = (NodeToken)nc.choice;
                if (!token.tokenImage.equals("public")
                    && !token.tokenImage.equals("protected")
                    && !token.tokenImage.equals("private")) {
                    newModifiers.add(token);
                }
            }

            newModifiers.add(new NodeToken("public"));
            modifiers.f0.nodes = new Vector(newModifiers);
        }
    }

    /**
     * Adds the following new methods:
     *
     * checkClassInvariantsInstrument(daikon.tools.runtimechecker.Violation.Time time)
     *   Checks the class invariants.
     * checkObjectInvariants_instrument(daikon.tools.runtimechecker.Violation.Time time)
     *   Check the object invariants
     * isDaikonInstrumented()
     *   returns true (you can imagine calling this method to see if the class has been
     *   instrumented).
     * getDaikonInvariants()
     *   Returns th array of properties being checked.
     *
     * Adds the following field:
     *
     * daikon.tools.runtimechecker.Property[] daikonProperties
     *   The properties being checked.
     *
     * Add code that initializes the properties array.
     */
    public void visit(ClassOrInterfaceBody clazz) {

        // Fix any line/col inconsistencies first
        clazz.accept(new TreeFormatter());

        super.visit(clazz);

        if (Ast.isInterface(clazz)) {
            return;
        }

        // add method to check object and class invariants.
        ClassOrInterfaceDeclaration ucd = (ClassOrInterfaceDeclaration) clazz
                .getParent();
        String classname = Ast.getClassName(ucd);
        ClassOrInterfaceBodyDeclaration objInvDecl = checkObjectInvariants_instrumentDeclaration(classname);
        Ast.addDeclaration(clazz, objInvDecl);

        boolean isNested = false;
        boolean isStatic = false;
        Node dParent = ucd.getParent();
        if (!Ast.isInner(ucd) || Ast.isStatic(ucd)) {
            ClassOrInterfaceBodyDeclaration classInvDecl = checkClassInvariantsInstrumentDeclaration(classname);
            Ast.addDeclaration(clazz, classInvDecl);
            Ast.addDeclaration(clazz, getInvariantsDecl());
            Ast.addDeclaration(clazz, isInstrumentedDecl());
	    Ast.addDeclaration(clazz, staticPropertyDecl());
	    Ast.addDeclaration(clazz, staticPropertyInit());
        }
    }

    /**
     * Adds code to check class invariants and preconditions on entry
     * (but not object invariants, because there's no object yet!).
     *
     * Adds code to check postcontiions, class and object invariants
     * on exit.
     */
    public void visit(ConstructorDeclaration ctor) {

        // Fix any line/col inconsistencies first
        ctor.accept(new TreeFormatter());

        super.visit(ctor);

        // Find declared throwables.
        List<String> declaredThrowables = getDeclaredThrowables(ctor.f3);

        List<PptTopLevel> matching_ppts = pptMatcher.getMatches(pptmap, ctor);

        StringBuffer code = new StringBuffer();

        NodeListOptional ctorBody = ctor.f6;

        // Create the code for a new BlockStatement that will be the new
        // body of the constructor.
        code.append("{");

        // Count this program point entry.
        code.append("daikon.tools.runtimechecker.Runtime.numPptEntries++;");

        // Check class invariants.
        code.append("checkClassInvariantsInstrument(daikon.tools.runtimechecker.Violation.Time.onEntry);");

        // Check preconditions.
        checkPreconditions(code, matching_ppts, pptmap);

        // Check preconditions.
        for (Iterator i = matching_ppts.iterator(); i.hasNext();) {
            PptTopLevel ppt = (PptTopLevel) i.next();
            if (ppt.ppt_name.isEnterPoint()) {
                List<Invariant> preconditions = filterInvariants(Ast
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
        //newCtorBody.accept(new TreeFormatter(2, 0));
        ctor.f6 = new NodeListOptional(newCtorBody);
    }

    // Methods that we have created
    private final Set generated_methods = new HashSet();

    /**
     *
     */
    public void visit(MethodDeclaration method) {

        // Fix any line/col inconsistencies first
        method.accept(new TreeFormatter());

        super.visit(method);

        ClassOrInterfaceDeclaration clsdecl =
            (ClassOrInterfaceDeclaration)Ast.getParent(ClassOrInterfaceDeclaration.class, method);

        Assert.assertTrue(clsdecl != null);

        if (Ast.isInterface(clsdecl)) {
            return;
        }

        method.accept(new TreeFormatter());

//         System.out.println("@@@0");
//         method.accept(new TreeDumper());
//         System.out.println("@@@1");

        if (generated_methods.contains(method)) {
            return;
        }

        // Determine if method is static.
        boolean isStatic = Ast.isStatic(method);

        // Find declared throwables.
        List<String> declaredThrowables = getDeclaredThrowables(method.f3);

        List/* PptTopLevel */matching_ppts = pptMatcher.getMatches(pptmap, method);

        String name = Ast.getName(method);
        String returnType = Ast.getReturnType(method);
        String maybeReturn = (returnType.equals("void") ? "" : "return");
        List parameters = new ArrayList();
        for (Iterator i = Ast.getParameters(method).iterator(); i.hasNext();) {
            FormalParameter param = (FormalParameter) i.next();
            parameters.add(Ast.getName(param));
        }

        StringBuffer code = new StringBuffer();

        code.append("{");

        // Count this program point entry.
        code.append("daikon.tools.runtimechecker.Runtime.numPptEntries++;");

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
                List<Invariant> preconditions = filterInvariants(Ast
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

        Block block = (Block) Ast.create("Block", new_method);

        wrapper.f4.choice = block;
        wrapper.accept(new TreeFormatter());

        Modifiers modifiersOrig = Ast.getModifiers(method);
        Modifiers modifiers = (Modifiers)Ast.copy("Modifiers", modifiersOrig);
        modifiers.accept(new TreeFormatter());

        ClassOrInterfaceBody c =
            (ClassOrInterfaceBody) Ast.getParent(ClassOrInterfaceBody.class, method);

        StringBuffer modifiers_declaration_stringbuffer  = new StringBuffer();
        modifiers_declaration_stringbuffer.append(Ast.print(modifiers));
        modifiers_declaration_stringbuffer.append(" ");
        modifiers_declaration_stringbuffer.append(Ast.print(wrapper));

        ClassOrInterfaceBodyDeclaration d = (ClassOrInterfaceBodyDeclaration) Ast.create(
                "ClassOrInterfaceBodyDeclaration",
                new Class[] { Boolean.TYPE },
                new Object[] { new Boolean(false) },  // isInterface == false
                modifiers_declaration_stringbuffer.toString());
        Ast.addDeclaration(c, d);
        NodeSequence ns = (NodeSequence) d.f0.choice;
        NodeChoice nc = (NodeChoice) ns.elementAt(1);
        MethodDeclaration generated_method = (MethodDeclaration) nc.choice;
        generated_methods.add(generated_method);

        // Rename the original method, and make it private.
        Ast.setName(method, "internal$" + name);
        Ast.setAccess(method, "private");
    }

    // vioTime can be the name of a variable (which should be in scope)
    // or a string, but if it's a string, then you should write it as
    // something like: \"<ENTER>\"
    private void appendInvariantChecks(List<Invariant> invs,
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
            code.append("daikon.tools.runtimechecker.Runtime.numEvaluations++;");
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
    private ClassOrInterfaceBodyDeclaration isInstrumentedDecl() {
        StringBuffer code = new StringBuffer();
        code.append("public static boolean isDaikonInstrumented() { return true; }");
        return (ClassOrInterfaceBodyDeclaration) Ast.create("ClassOrInterfaceBodyDeclaration",
                                                            new Class[] { Boolean.TYPE },
                                                            new Object[] { new Boolean(false) },  // isInterface == false

                                                            code
                                                            .toString());
    }

    /**
     * @return
     */
    private ClassOrInterfaceBodyDeclaration getInvariantsDecl() {
        StringBuffer code = new StringBuffer();
        code.append("public static java.util.Set getDaikonInvariants() {");
        code.append("  return new java.util.HashSet(java.util.Arrays.asList(daikonProperties));");
        code.append("}");
        return (ClassOrInterfaceBodyDeclaration) Ast.create("ClassOrInterfaceBodyDeclaration",
                                                            new Class[] { Boolean.TYPE },
                                                            new Object[] { new Boolean(false) },  // isInterface == false

                                                            code
                                                            .toString());
    }

    /**
     * @return
     */
    private ClassOrInterfaceBodyDeclaration staticPropertyDecl() {
        StringBuffer code = new StringBuffer();

	code.append("private static daikon.tools.runtimechecker.Property[] daikonProperties;");
        return (ClassOrInterfaceBodyDeclaration) Ast.create("ClassOrInterfaceBodyDeclaration",
                                                            new Class[] { Boolean.TYPE },
                                                            new Object[] { new Boolean(false) },  // isInterface == false

                                                            code
                                                            .toString());
    }

    private ClassOrInterfaceBodyDeclaration staticPropertyInit() {
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

        return (ClassOrInterfaceBodyDeclaration) Ast.create("ClassOrInterfaceBodyDeclaration",
                                                            new Class[] { Boolean.TYPE },
                                                            new Object[] { new Boolean(false) },  // isInterface == false

                                                            code
                                                            .toString());
    }



    private ClassOrInterfaceBodyDeclaration checkObjectInvariants_instrumentDeclaration(
            String classname) {
        StringBuffer code = new StringBuffer();
        code
                .append("private void checkObjectInvariants_instrument(daikon.tools.runtimechecker.Violation.Time time) {");
        String objectPptname = classname + ":::OBJECT";
        PptTopLevel objectPpt = pptmap.get(objectPptname);
        if (objectPpt != null) {
            List<Invariant> objectInvariants = filterInvariants(Ast
                    .getInvariants(objectPpt, pptmap));
            appendInvariantChecks(objectInvariants, code, "time");
        }
        code.append("}" + daikon.Global.lineSep + "");
        return (ClassOrInterfaceBodyDeclaration) Ast.create("ClassOrInterfaceBodyDeclaration",
                                                            new Class[] { Boolean.TYPE },
                                                            new Object[] { new Boolean(false) },  // isInterface == false
                                                            code.toString());
    }

    private ClassOrInterfaceBodyDeclaration checkClassInvariantsInstrumentDeclaration(
            String classname) {
        StringBuffer code = new StringBuffer();
        code
                .append("private static void checkClassInvariantsInstrument(daikon.tools.runtimechecker.Violation.Time time) {");
        String classPptname = classname + ":::CLASS";
        PptTopLevel classPpt = pptmap.get(classPptname);
        if (classPpt != null) {
            List<Invariant> classInvariants = filterInvariants(Ast
                    .getInvariants(classPpt, pptmap));
            appendInvariantChecks(classInvariants, code, "time");
        }
        code.append("}" + daikon.Global.lineSep + "");
        return (ClassOrInterfaceBodyDeclaration) Ast.create("ClassOrInterfaceBodyDeclaration",
                                                            new Class[] { Boolean.TYPE },
                                                            new Object[] { new Boolean(false) },  // isInterface == false
                                                            code.toString());
    }

    /**
     * Return a subset of the argument list, removing invariants
     * that do not have a properly implemented Java format.
     **/
    private static List<Invariant> filterInvariants(
            List<Invariant> invariants) {
        List<Invariant> survivors = new ArrayList<Invariant>();
        for (Iterator i = invariants.iterator(); i.hasNext();) {
            Invariant inv = (Invariant) i.next();

            if (! inv.isValidExpression(OutputFormat.JAVA)) {
                continue;
            }
            // Left and right shifts are formatted properly by Daikon,
            // but as of 2/6/2005, a JTB bug prevents them from being
            // inserted into code.
            if (inv instanceof FunctionBinary) {
                FunctionBinary fb = (FunctionBinary)inv;
                if (fb.isLshift() || fb.isRshiftSigned() || fb.isRshiftUnsigned()) {
                    // System.err.println("Warning: shift operation skipped: " + inv.format_using(OutputFormat.JAVA));
                    continue;
                }
            }

            survivors.add(inv);
        }
        return survivors;
    }

    private static List<String> getDeclaredThrowables(NodeOptional nodeOpt) {
        List<String> declaredThrowables = new ArrayList();
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
            List<PptTopLevel> matching_ppts, PptMap pptmap,
	   List<String> declaredThrowables, boolean isStatic) {

	List<String> declaredThrowablesLocal = new ArrayList(declaredThrowables);
	declaredThrowablesLocal.remove("java.lang.RuntimeException");
	declaredThrowablesLocal.remove("RuntimeException");
	declaredThrowablesLocal.remove("java.lang.Error");
	declaredThrowablesLocal.remove("Error");


        // Count this program point exit.
        code.append("daikon.tools.runtimechecker.Runtime.numNormalPptExits++;");

        // [[ TODO: Figure out what could go wrong here (e.g. what if
        // method declaration says "throws Throwable") and prepare for
        // it. ]]
        for (Iterator i = declaredThrowablesLocal.iterator(); i.hasNext();) {
            String declaredThrowable = (String) i.next();
            code.append("} catch (" + declaredThrowable + " t_instrument) {");
            // Count this program point exit.
            code.append("daikon.tools.runtimechecker.Runtime.numExceptionalPptExits++;");
            code.append("  methodThrewSomething_instrument = true;");
            code.append("  throw t_instrument;");
        }

	code.append("} catch (java.lang.RuntimeException t_instrument) {");
	code.append("  methodThrewSomething_instrument = true;");
        // Count this program point exit.
        code.append("daikon.tools.runtimechecker.Runtime.numExceptionalPptExits++;");
	code.append("  throw t_instrument;");
	code.append("} catch (java.lang.Error t_instrument) {");
        // Count this program point exit.
        code.append("daikon.tools.runtimechecker.Runtime.numExceptionalPptExits++;");
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
                List<Invariant> postconditions = filterInvariants(Ast
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
            List<PptTopLevel> matching_ppts, PptMap pptmap) {
        for (Iterator i = matching_ppts.iterator(); i.hasNext();) {
            PptTopLevel ppt = (PptTopLevel) i.next();
            if (ppt.ppt_name.isEnterPoint()) {
                List<Invariant> preconditions = filterInvariants(Ast
                        .getInvariants(ppt, pptmap));
                appendInvariantChecks(preconditions, code, "daikon.tools.runtimechecker.Violation.Time.onEntry");
            }
        }
    }

    private static String xmlString(Invariant inv) {
        StringBuffer code = new StringBuffer();

        String daikonrep = inv.format_using(OutputFormat.DAIKON);

        String javarep = inv.format_using(OutputFormat.JAVA);

        if (daikonrep.indexOf("\"") != -1 || daikonrep.indexOf("\\") != -1) {
            // Now comes some real ugliness: [[ ... ]] It's easier to do
            // this transformation on a character list than by pattern
            // matching against a String.
            char[] chars = daikonrep.toCharArray();
            List<Character> charList = new ArrayList();
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
        code.append("<INV>" + utilMDE.UtilMDE.escapeNonJava(javarep) + "</INV>");
        code.append("<DAIKONCLASS>" + inv.getClass().toString()
                    + "</DAIKONCLASS>");
        code.append("<METHOD>" + inv.ppt.parent.ppt_name.getSignature()
                    + "</METHOD>");
        code.append("</INVINFO>");
        return code.toString();
    }

}
