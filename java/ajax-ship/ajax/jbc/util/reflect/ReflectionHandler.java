/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util.reflect;

import ajax.Globals;
import ajax.jbc.*;
import ajax.jbc.util.*;
import java.util.*;
import ajax.util.*;
import java.io.*;

public class ReflectionHandler extends JBCNativeCodeLoader implements JBCObserver, OpcodeConstants {
/**
This is a map from JBCMethods to JBCClasses to sets of classes and methods.
*/
    private Hashtable reflectionMethodInfo = new Hashtable();
    private Hashtable reflectionMethods = null;
    private CompactSet liveMethods = new CompactSet();
    private CompactSet reflectedClasses = new CompactSet();
    private CompactSet reflectedMethods = new CompactSet();
    private CompactSet serializedClasses = new CompactSet();
    private JBCClass javaIoSerializable = null;
    private boolean receiveLivenessNotifications = false;
    
    public static final String defaultReflectionResourceName = "reflection";
    
    private static final String[] wrapperClasses = {
        "Boolean", "Byte", "Short", "Character", "Integer", "Long", "Float", "Double"
    };
    
    public ReflectionHandler(JBCWorld w) {
        super(w);
        
        w.addObserver(this);
    }
    
    private void loadMetadata(StandardClassLoader loader) {
        try {
            InputStream is = getClass().getResourceAsStream("/" + defaultReflectionResourceName);
            if (is != null) {
            addReflectionInfo(loader,
                new BufferedReader(new InputStreamReader(is)));
            } else {
                Globals.userError("Cannot read the reflection information resource (\"" + defaultReflectionResourceName + "\")");
            }
        } catch (IOException ex) {
            Globals.userError("Problem with the reflection information resource (\"" + defaultReflectionResourceName + "\");\n"
                + "(" + ex.getMessage() + ")");
        } catch (ReflectionReaderException ex) {
            Globals.userError("Problem with the reflection information resource (\"" + defaultReflectionResourceName + "\");\n"
                + "There is an error in the format of the reflection information.\n"
                + "(" + ex.getMessage() + ")");
        }
    }
    
    private static ReflectionReaderException error(StreamTokenizer stream, String s) {
        return new ReflectionReaderException(s + ", reading: " + stream.toString());
    }
    
    private static boolean isExtendedWord(StreamTokenizer t) {
        return t.ttype == t.TT_WORD || t.ttype == '\'' || t.ttype == '"';
    }
    
    private static void skipConstant(StreamTokenizer stream, String w) throws ReflectionReaderException {
        if (!isExtendedWord(stream) || !stream.sval.equals(w)) {
            throw error(stream, "Expected '" + w + "'");
        }
    }
    
    private static String readWord(StreamTokenizer stream, String description) throws ReflectionReaderException {
        if (!isExtendedWord(stream)) {
            throw error(stream, "Expected " + description);
        } else {
            return stream.sval.intern();
        }
    }
    
    private static void skipToken(StreamTokenizer stream, char ch) throws ReflectionReaderException {
        if (stream.ttype != ch) {
            throw error(stream, "Expected character '" + ch + "'");
        }
    }
    
    private Hashtable addReflectionMethod(StandardClassLoader loader,
        StreamTokenizer t, String methodName) throws ReflectionReaderException {
        try {
            JBCMethod m = JBCParserUtilities.findMethod(loader, methodName);
            Object o = reflectionMethodInfo.get(m);
            
            if (o == null) {
                o = new Hashtable();
                reflectionMethodInfo.put(m, o);
            }
            
            return (Hashtable)o;
        } catch (UnresolvedClassException ex) {
            Globals.writeLog(this, "WARNING: " + ex.getMessage());
            return null;
        } catch (MissingMethodException ex) {
            Globals.writeLog(this, "WARNING: Missing method: " + ex.getMessage());
            return null;
        } catch (AmbiguousMethodException ex) {
            throw error(t, ex.getMessage());
        }
    }
    
    private CompactSet addReflectionMethodHasCaller(StandardClassLoader loader,
        StreamTokenizer t, Hashtable methodInfo, String methodName) throws ReflectionReaderException {
        if (methodInfo == null) {
            return null;
        } else {
            try {
                JBCMethod m = JBCParserUtilities.findMethod(loader, methodName);
                Object o = methodInfo.get(m);
                    
                if (o == null) {
                    o = new CompactSet();
                    methodInfo.put(m, o);
                }
                    
                return (CompactSet)o;
            } catch (UnresolvedClassException ex) {
                Globals.writeLog(this, "WARNING: " + ex.getMessage());
                return null;
            } catch (MissingMethodException ex) {
                Globals.writeLog(this, "WARNING: Missing method: " + ex.getMessage());
                return null;
            } catch (AmbiguousMethodException ex) {
                throw error(t, ex.getMessage());
            }
        }
    }
    
    private void addReflectionClassInfo(StandardClassLoader loader,
        StreamTokenizer t, CompactSet info, String className) throws ReflectionReaderException {
        if (info != null) {
            int separator = className.indexOf('<');
            JBCClass bound = null;
            
            if (separator >= 0) {
                String boundClassName = className.substring(separator + 1);
                
                bound = loader.getClass(boundClassName);
                
                if (bound == null) {
                    Globals.writeLog(this, "WARNING: Bound class " + boundClassName + " not found");
                }
                className = className.substring(0, separator);
            }
            
            Enumeration e = (new GlobClassMask(className)).findMatchingClasses(loader).elements();
            
            while (e.hasMoreElements()) {
                JBCClass c = loader.getClassCaseInsensitive((String)e.nextElement());
                    
                if (c != null && (bound == null || c.isSubclassOf(bound))) {
                    info.add(new ReflectionData(c));
                }
            }
        }
    }
    
    private void addSerializedClassInfo(StandardClassLoader loader,
        StreamTokenizer t, CompactSet info, String className) throws ReflectionReaderException {
        if (info != null) {
            Enumeration e = (new GlobClassMask(className)).findMatchingClasses(loader).elements();
            
            while (e.hasMoreElements()) {
                JBCClass c = loader.getClassCaseInsensitive((String)e.nextElement());
                    
                if (c != null) {
                    info.add(new ReflectionData(ReflectionData.SERIALIZED_CLASS, c));
                }
            }
        }
    }
    
    private void addReflectionMethodInfo(StandardClassLoader loader,
        StreamTokenizer t, CompactSet info, String methodName) throws ReflectionReaderException {
        if (info != null) {
            int lastDot = methodName.lastIndexOf('.');
            
            if (lastDot < 0) {
                throw error(t, "No '.' in method name: " + methodName);
            }
            
            String className = methodName.substring(0, lastDot);
            Enumeration e = (new GlobClassMask(className)).findMatchingClasses(loader).elements();
            GlobMatcher g = new GlobMatcher(methodName.substring(lastDot + 1));
                
            while (e.hasMoreElements()) {
                JBCClass c = (JBCClass)loader.getClassCaseInsensitive((String)e.nextElement());
                    
                if (c != null) {
                    for (Enumeration e2 = c.getInheritedMethods(); e2.hasMoreElements();) {
                        JBCMethod m = (JBCMethod)e2.nextElement();
                            
                        if (g.isMatch(m.getMethodName())) {
                            info.add(new ReflectionData(m));
                        }
                    }
                }
            }
        }
    }
    
    private void addReflectionInfo(StandardClassLoader loader, Reader r) throws IOException, ReflectionReaderException {
        StreamTokenizer t = new StreamTokenizer(r);
        
        t.commentChar('#');
        t.wordChars('*', '*');
        t.wordChars('?', '?');
        t.wordChars('_', '_');
        t.wordChars('.', '.');
        t.wordChars('<', '<');
        t.wordChars('>', '>');
        
        while (t.nextToken() != t.TT_EOF) {
            String methodName = readWord(t, "method name");
            
            Hashtable methodInfo = addReflectionMethod(loader, t, methodName);
            
            t.nextToken();
            skipToken(t, '[');
            
            t.nextToken();
            while (isExtendedWord(t)) {
                String callerMethodName = readWord(t, "caller method name");
                
                t.nextToken();
                skipToken(t, '{');
                
                CompactSet methodCallerInfo = addReflectionMethodHasCaller(
                    loader, t, methodInfo, callerMethodName);
            
                t.nextToken();
                while (isExtendedWord(t)) {
                    String categoryName = readWord(t, "category name");
                    
                    t.nextToken();
                    skipToken(t, '=');
                    
                    t.nextToken();
                    String valueName = readWord(t, "value name");
                    Object o;
                    
                    if (categoryName.equals("class")) {
                        addReflectionClassInfo(loader, t, methodCallerInfo, valueName);
                    } else if (categoryName.equals("method")) {
                        addReflectionMethodInfo(loader, t, methodCallerInfo, valueName);
                    } else if (categoryName.equals("serialized")) {
                        addSerializedClassInfo(loader, t, methodCallerInfo, valueName);
                    } else {
                        throw error(t, "Expected a category name ('class' or 'method')");
                    }
                    
                    t.nextToken();
                }
                
                skipToken(t, '}');
                t.nextToken();
            }
            
            skipToken(t, ']');
        }
        
        for (Enumeration e = liveMethods.elements(); e.hasMoreElements();) {
            checkLiveMethod((JBCMethod)e.nextElement());
        }
        
        updateMethods();
    }
    
    private void updateMethods() {
        if (reflectionMethods != null) {
            makeReflectionMethods();
            
            for (Enumeration e = reflectionMethods.keys(); e.hasMoreElements();) {
                setFunctionCode((String)e.nextElement());
            }
        }
    }
    
    public ReflectionHandler(StandardClassLoader loader) {
        this(loader.getWorld());
        
        loadMetadata(loader);
    }
    
    public void setReceiveLivenessNotifications() {
        receiveLivenessNotifications = true;
    }
    
    private boolean checkCallAfterConstant(JBCMethod caller, byte[] code, int offset,
        int instructionLength) {
        int callOffset = offset + instructionLength;
        boolean changed = false;
        
        switch (code[callOffset] & 0xFF) {
            case OP_invokestatic: {
                Object constant = JBCCodeUtilities.getLoadedConstant(caller, code, offset);
                
                if (constant instanceof String) {
                    MethodData data = caller.getData();
                    CodeRefData ref = JBCCodeUtilities.getCodeRefAt(data, code, callOffset + 1);
                    String name = ref.getSlotName();
                    
                    if (name.equals("class$")) {
                        JBCClass c = caller.getContainingClass().getClassLoader().getClass((String)constant);
                        
                        if (c != null) {
                            if (addReflectedClass(c)) {
                                changed = true;
                            }
                        }
                    }
                }
                break;
            }
        }
        
        return changed;
    }
    
    private boolean checkNewCallRelationship(JBCMethod caller, JBCMethod callee) {
        if (callee == null) {
            return false;
        } else {
            Object o = reflectionMethodInfo.get(callee);
            boolean changed = false;
            
            if (o != null) {
                Hashtable calleeInfo = (Hashtable)o;
                
                o = calleeInfo.get(caller);
                
                if (o == null) {
                    Globals.userError("Unknown caller of reflection method: " + caller + " ---> " + callee);
                } else {
                    Globals.writeLog(this, "Adding on behalf of " + caller + " ---> " + callee);
                    for (Enumeration e = ((CompactSet)o).elements(); e.hasMoreElements();) {
                        ReflectionData d = (ReflectionData)e.nextElement();
                        CompactSet destSet;
                        
                        switch (d.getKind()) {
                            case ReflectionData.REFLECTED_CLASS:
                                if (addReflectedClass((JBCClass)d.getData())) {
                                    changed = true;
                                }
                                break;
                                
                            case ReflectionData.REFLECTED_METHOD:
                                if (addReflectedMethod((JBCMethod)d.getData())) {
                                    changed = true;
                                }
                                break;
                                
                            default:
                                if (Globals.debug) {
                                    Globals.localError("Invalid reflection type");
                                }
                            case ReflectionData.SERIALIZED_CLASS:
                                if (addSerializedClass((JBCClass)d.getData())) {
                                    changed = true;
                                }
                                break;
                        }
                    }
                }
            }
            
            return changed;
        }
    }
    
    private void checkLiveMethod(JBCMethod m) {
        if (!m.getMethodName().equals("class$")) {
            MethodData data = m.getData();
            byte[] code = m.getData().getCode();
            boolean changed = false;
                
            if (code != null) {
                boolean[] instructions = JBCCodeUtilities.getInstructionStarts(data);
                    
                for (int i = 0; i < instructions.length; i++) {
                    if (instructions[i]) {
                        switch (code[i] & 0xFF) {
                            case OP_invokevirtual:
                            case OP_invokestatic:
                            case OP_invokespecial:
                            case OP_invokeinterface:
                                if (checkNewCallRelationship(m,
                                    JBCCodeUtilities.resolveInstructionMethod(m, code, i))) {
                                    changed = true;
                                }
                                break;
                                
                            case OP_ldc:
                                if (checkCallAfterConstant(m, code, i, 2)) {
                                    changed = true;
                                }
                                break;
                                
                            case OP_ldc_w:
                                if (checkCallAfterConstant(m, code, i, 3)) {
                                    changed = true;
                                }
                                break;
                        }
                    }
                }
            }
            
            if (changed) {
                updateMethods();
            }
        }
    }
    
    public void notifyLiveMethod(JBCMethod m) {
        liveMethods.add(m);
        checkLiveMethod(m);
    }
    
    private boolean isSerializable(JBCClass c) {
        if (javaIoSerializable == null) {
            javaIoSerializable = getWorld().getSpecialClass("java.io.Serializable");
        }
        
        while (c != null && c.isArray()) {
            JBCType elemType = c.getClassType().getArrayElementType();
            
            if (elemType instanceof JBCObjectType) {
                c = ((JBCObjectType)elemType).getClassDef();
            } else {
                c = null;
            }
        }
        
        return c == null || c.isSubclassOf(javaIoSerializable);
    }

    private boolean addSerializedClass(JBCClass c) {
        if (c != null && !c.isInterface() && isSerializable(c) && serializedClasses.get(c) == null) {
            serializedClasses.addUnconditionally(c);
            Globals.writeLog(this, "Adding serialized class: " + c);
            
            for (Enumeration e = c.getInheritedFields(); e.hasMoreElements();) {
                JBCField f = (JBCField)e.nextElement();
                
                if (!f.isStatic() && !f.isTransient()) {
                    JBCType t = f.getFieldType();
                    
                    if (t instanceof JBCObjectType) {
                        addSerializedClass(((JBCObjectType)t).getClassDef());
                    }
                }
            }
            
            if (c.isArray()) {
                JBCType elemType = c.getClassType().getArrayElementType();
                
                if (elemType instanceof JBCObjectType) {
                    addSerializedClass(((JBCObjectType)elemType).getClassDef());
                }
            }

            return true;
        } else {
            return false;
        }
    }
    
    private boolean addReflectedClass(JBCClass c) {
        if (reflectedClasses.get(c) == null) {
            reflectedClasses.addUnconditionally(c);
            Globals.writeLog(this, "Adding reflected class: " + c);
            return true;
        } else {
            return false;
        }
    }
    
    private boolean addReflectedMethod(JBCMethod m) {
        if (reflectedMethods.get(m) == null) {
            reflectedMethods.addUnconditionally(m);
            Globals.writeLog(this, "Adding reflected method: " + m);
            return true;
        } else {
            return false;
        }
    }
    
    private ExternalFlowgraph makeMakeSerializedObject() {
        ExternalCFGVariable obj = new ExternalAnonymousVariable();
        ExternalNode[] objCreators = new ExternalDefNode[serializedClasses.size()];
        int index = 0;
        
        for (Enumeration e = serializedClasses.elements(); e.hasMoreElements();) {
            JBCClass c = (JBCClass)e.nextElement();
            
            if (!c.isAbstract() && !c.isArray()) {
                objCreators[index] = new ExternalNewObjectNode(obj, c);
                index++;
            }
        }
        
        if (index == 0) {
            ExternalCFGDefNode result = new ExternalChoiceNode(new ExternalCFGVariable[0]);
            
            return new ExternalFG(result, result);
        } else {
            ExternalNode root = new ExternalNode();
            ExternalCFGNode[] gotoTargets = new ExternalCFGNode[index];
            
            System.arraycopy(objCreators, 0, gotoTargets, 0, index);
            root.setSuccessors(gotoTargets);
            
            ExternalCFGVariable[] objVarSingleton = { obj };
            ExternalCFGDefNode result = new ExternalChoiceNode(objVarSingleton);
            
            for (int i = 0; i < index; i++) {
                objCreators[i].setSuccessor(result);
            }
            
            return new ExternalFG(root, result);
        }
    }
    
    private ExternalFlowgraph makeMakeSerializedArray() {
        ExternalCFGVariable obj = new ExternalAnonymousVariable();
        ExternalNode[] objCreators = new ExternalDefNode[serializedClasses.size()];
        int index = 0;
        
        for (Enumeration e = serializedClasses.elements(); e.hasMoreElements();) {
            JBCClass c = (JBCClass)e.nextElement();
            
            if (!c.isAbstract() && c.isArray()) {
                objCreators[index] = new ExternalNewObjectNode(obj, c);
                index++;
            }
        }
        
        if (index == 0) {
            ExternalCFGDefNode result = new ExternalChoiceNode(new ExternalCFGVariable[0]);
            
            return new ExternalFG(result, result);
        } else {
            ExternalNode root = new ExternalNode();
            ExternalCFGNode[] gotoTargets = new ExternalCFGNode[index];
            
            System.arraycopy(objCreators, 0, gotoTargets, 0, index);
            root.setSuccessors(gotoTargets);
            
            ExternalCFGVariable[] objVarSingleton = { obj };
            ExternalCFGDefNode result = new ExternalChoiceNode(objVarSingleton);
            
            for (int i = 0; i < index; i++) {
                objCreators[i].setSuccessor(result);
            }
            
            return new ExternalFG(root, result);
        }
    }
    
    private ExternalFlowgraph makeMakeObjectAndCallZeroArgConstructor() {
        ExternalCFGVariable obj = new ExternalAnonymousVariable();
        ExternalNode[] objCreators = new ExternalDefNode[reflectedClasses.size()];
        ExternalNode[] objInitializers = new ExternalDefNode[objCreators.length];
        ExternalCFGVariable[] objVarSingleton = { obj };
        int index = 0;
        
        for (Enumeration e = reflectedClasses.elements(); e.hasMoreElements();) {
            JBCClass c = (JBCClass)e.nextElement();
            
            if (!c.isAbstract() && !c.isArray()) {
                JBCMethod m = c.getMethod("<init>", "()V");
                
                if (m != null) {
                    objCreators[index] = new ExternalNewObjectNode(obj, c);
                    objInitializers[index] = new ExternalMethodCallNode(m, objVarSingleton);
                    objCreators[index].setSuccessor(objInitializers[index]);
                    index++;
                }
            }
        }
        
        if (index == 0) {
            ExternalCFGDefNode result = new ExternalChoiceNode(new ExternalCFGVariable[0]);
            
            return new ExternalFG(result, result);
        } else {
            ExternalNode root = new ExternalNode();
            ExternalCFGNode[] gotoTargets = new ExternalCFGNode[index];
            
            System.arraycopy(objCreators, 0, gotoTargets, 0, index);
            root.setSuccessors(gotoTargets);
            
            ExternalCFGDefNode result = new ExternalChoiceNode(objVarSingleton);
            
            for (int i = 0; i < index; i++) {
                objInitializers[i].setSuccessor(result);
            }
            
            return new ExternalFG(root, result);
        }
    }
    
    private static ExternalCFGVariable[] makeMultiArgs(JBCMethod method, ExternalCFGVariable obj,
        ExternalCFGVariable argElement, ExternalCFGVariable unwrappedElement) {
        JBCType[] paramTypes = method.getMethodType().getParameterTypes();
        ExternalCFGVariable[] result = new ExternalCFGVariable[paramTypes.length];
        int i = 1;
       
        if (method.isStatic()) {
            i = 0;
        } else {
            result[0] = obj;
        }
        
        for (; i < paramTypes.length; i++) {
            JBCType t = paramTypes[i];
            
            if (t instanceof JBCObjectType) {
                result[i] = argElement;
            } else {
                result[i] = unwrappedElement;
            }
        }
        
        return result;
    }
    
    private ExternalFlowgraph makeMakeObjectAndCallArbitraryConstructor() {
        ExternalCFGVariable obj = new ExternalAnonymousVariable();
        ExternalCFGVariable argElement = new ExternalAnonymousVariable();
        ExternalCFGVariable unwrappedElement = new ExternalAnonymousVariable();
        int size = 0;
        
        for (Enumeration e = reflectedClasses.elements(); e.hasMoreElements();) {
            JBCClass c = (JBCClass)e.nextElement();
            
            for (Enumeration e2 = c.getMethods(); e2.hasMoreElements();) {
                e2.nextElement();
                size++;
            }
        }
        
        ExternalNode[] objCreators = new ExternalDefNode[size];
        ExternalNode[] objInitializers = new ExternalDefNode[objCreators.length];
        int index = 0;
        
        for (Enumeration e = reflectedClasses.elements(); e.hasMoreElements();) {
            JBCClass c = (JBCClass)e.nextElement();
            
            if (!c.isAbstract() && !c.isArray()) {
                for (Enumeration e2 = c.getMethods(); e2.hasMoreElements();) {
                    JBCMethod m = (JBCMethod)e2.nextElement();
                    
                    if (m.getMethodName().equals("<init>")) {
                        objCreators[index] = new ExternalNewObjectNode(obj, c);
                        objInitializers[index] = new ExternalMethodCallNode(m,
                            makeMultiArgs(m, obj, argElement, unwrappedElement));
                        objCreators[index].setSuccessor(objInitializers[index]);
                        index++;
                    }
                }
            }
        }
        
        if (index == 0) {
            ExternalCFGDefNode result = new ExternalChoiceNode(new ExternalCFGVariable[0]);
            
            return new ExternalFG(result, result);
        } else {
            ExternalNode root = new ExternalNode();
            ExternalCFGVariable argList = new ExternalAnonymousVariable();
            ExternalParameterNode paramNode = new ExternalParameterNode(argList, 0);
            
            root.setSuccessor(paramNode);
            
            JBCClass javaLangObject = getWorld().getSpecialClass("java.lang.Object");
            ExternalUserFieldNode argNode = new ExternalUserFieldNode(argElement, argList,
                javaLangObject.registerUserField("arrayelement", false));
                
            paramNode.setSuccessor(argNode);
            
            ExternalNode meet = new ExternalNode();
            ExternalCFGNode[] unwrapTargets = new ExternalCFGNode[wrapperClasses.length];
            
            for (int i = 0; i < wrapperClasses.length; i++) {
                ExternalFieldNode node = new ExternalFieldNode(unwrappedElement, argElement,
                    getWorld().getSpecialClass("java.lang." + (String)wrapperClasses[i]).getField("value"));
                    
                node.setSuccessor(meet);
                
                unwrapTargets[i] = node;
            }
            
            argNode.setSuccessors(unwrapTargets);
            
            ExternalCFGNode[] gotoTargets = new ExternalCFGNode[index];
            
            System.arraycopy(objCreators, 0, gotoTargets, 0, index);
            meet.setSuccessors(gotoTargets);
            
            ExternalCFGVariable[] objVarSingleton = { obj };
            ExternalCFGDefNode result = new ExternalChoiceNode(objVarSingleton);
            
            for (int i = 0; i < index; i++) {
                objInitializers[i].setSuccessor(result);
            }
            
            return new ExternalFG(root, result);
        }
    }
    
    private ExternalFlowgraph makeCallArbitraryMethod() {
        ExternalCFGVariable obj = new ExternalAnonymousVariable();
        ExternalCFGVariable argElement = new ExternalAnonymousVariable();
        ExternalCFGVariable unwrappedElement = new ExternalAnonymousVariable();
        ExternalNode[] objCalls = new ExternalDefNode[reflectedMethods.size()];
        int index = 0;
        
        for (Enumeration e = reflectedMethods.elements(); e.hasMoreElements();) {
            JBCMethod m = (JBCMethod)e.nextElement();
            
            if (!m.getMethodName().startsWith("<")) {
                objCalls[index] = new ExternalMethodCallNode(obj, m,
                    makeMultiArgs(m, obj, argElement, unwrappedElement));
                index++;
            }
        }
        
        if (index == 0) {
            ExternalCFGDefNode result = new ExternalChoiceNode(new ExternalCFGVariable[0]);
            
            return new ExternalFG(result, result);
        } else {
            ExternalNode root = new ExternalNode();
            ExternalCFGVariable argList = new ExternalAnonymousVariable();
            ExternalParameterNode objNode = new ExternalParameterNode(obj, 0);
            
            root.setSuccessor(objNode);
            
            ExternalParameterNode paramNode = new ExternalParameterNode(argList, 1);
            
            objNode.setSuccessor(paramNode);
            
            JBCClass javaLangObject = getWorld().getSpecialClass("java.lang.Object");
            ExternalUserFieldNode argNode = new ExternalUserFieldNode(argElement, argList,
                javaLangObject.registerUserField("arrayelement", false));
                
            paramNode.setSuccessor(argNode);
            
            ExternalNode meet = new ExternalNode();
            ExternalCFGNode[] unwrapTargets = new ExternalCFGNode[wrapperClasses.length];
            
            for (int i = 0; i < wrapperClasses.length; i++) {
                ExternalFieldNode node = new ExternalFieldNode(unwrappedElement, argElement,
                    getWorld().getSpecialClass("java.lang." + (String)wrapperClasses[i]).getField("value"));
                    
                node.setSuccessor(meet);
                
                unwrapTargets[i] = node;
            }
            
            argNode.setSuccessors(unwrapTargets);
            
            ExternalCFGNode[] gotoTargets = new ExternalCFGNode[index];
            
            System.arraycopy(objCalls, 0, gotoTargets, 0, index);
            meet.setSuccessors(gotoTargets);
            
            ExternalCFGVariable[] objVarSingleton = { obj };
            ExternalCFGDefNode result = new ExternalChoiceNode(objVarSingleton);
            
            for (int i = 0; i < index; i++) {
                objCalls[i].setSuccessor(result);
            }
            
            return new ExternalFG(root, result);
        }
    }
    
    private void addReflectionMethod(String name, ExternalFlowgraph fg) {
        if (fg != null) {
            reflectionMethods.put(name, fg);
        }
    }
    
    private boolean isMatchingType(JBCType t1, JBCType t2) {
        if (t1 instanceof JBCObjectType) {
            return t2 instanceof JBCObjectType;
        } else {
            return t1.equals(t2);
        }
    }
    
    private ExternalFlowgraph makeAssignSerializedField(JBCType type) {
        ExternalCFGVariable obj = new ExternalAnonymousVariable();
        ExternalCFGVariable data = new ExternalAnonymousVariable();
        ExternalNode[] fieldSetters = new ExternalNode[serializedClasses.size()];
        int index = 0;
        
        for (Enumeration e = serializedClasses.elements(); e.hasMoreElements();) {
            JBCClass c = (JBCClass)e.nextElement();
            
            if (!c.isAbstract()) {
                ExternalNode lastSetter = null;
                
                for (Enumeration e2 = c.getInheritedFields(); e2.hasMoreElements();) {
                    JBCField f = (JBCField)e2.nextElement();
                    
                    if (!f.isStatic() && !f.isTransient()
                        && isMatchingType(f.getFieldType(), type)) {
                        ExternalNode setter =
                            new ExternalFieldAssignmentNode(obj, f, data);
                            
                        if (lastSetter != null) {
                            lastSetter.setSuccessor(setter);
                        } else {
                            fieldSetters[index] = setter;
                        }
                        
                        lastSetter = setter;
                    }
                }
                
                if (lastSetter != null) {
                    index++;
                }
            }
        }
        
        if (index == 0) {
            return new ExternalFG();
        } else {
            ExternalNode root = new ExternalNode();
            ExternalNode defObjParam = new ExternalParameterNode(obj, 0);
            
            root.setSuccessor(defObjParam);
            
            ExternalNode defDataParam = new ExternalParameterNode(data, 2);
            
            defObjParam.setSuccessor(defDataParam);
            
            ExternalCFGNode[] gotoTargets = new ExternalCFGNode[index];
            
            System.arraycopy(fieldSetters, 0, gotoTargets, 0, index);
            defDataParam.setSuccessors(gotoTargets);
            
            return new ExternalFG(root);
        }
    }
    
    private ExternalFlowgraph makeInvokeReadObject() {
        ExternalCFGVariable obj = new ExternalAnonymousVariable();
        ExternalCFGVariable stream = new ExternalAnonymousVariable();
        ExternalDefNode[] callers = new ExternalDefNode[serializedClasses.size()];
        ExternalCFGVariable[] callerParams = { obj, stream };
        int index = 0;
        
        for (Enumeration e = serializedClasses.elements(); e.hasMoreElements();) {
            JBCClass c = (JBCClass)e.nextElement();
            
            if (!c.isAbstract()) {
                JBCMethod m = c.getMethod("readObject", "(Ljava.io.ObjectInputStream;)V");
                
                if (m != null && m.isPrivate() && !m.isStatic()) {
                    callers[index] = new ExternalMethodCallNode(m, callerParams);
                    index++;
                }
            }
        }
        
        if (index == 0) {
            return new ExternalFG();
        } else {
            ExternalNode root = new ExternalNode();
            ExternalNode defObjParam = new ExternalParameterNode(obj, 0);
            
            root.setSuccessor(defObjParam);
            
            ExternalNode defStreamParam = new ExternalParameterNode(stream, 2);
            
            defObjParam.setSuccessor(defStreamParam);
            
            ExternalCFGDefNode[] gotoTargets = new ExternalCFGDefNode[index];
            
            System.arraycopy(callers, 0, gotoTargets, 0, index);
            defStreamParam.setSuccessors(gotoTargets);
            
            ExternalCFGDefNode exn = new ExternalCatchNode(gotoTargets,
                getWorld().getSpecialClass("java.lang.Throwable"));
            
            for (int i = 0; i < index; i++) {
                callers[i].setSuccessor(exn);
            }
            
            return new ExternalFG(root, null, exn);
        }
    }
    
    private void makeReflectionMethods() {
        reflectionMethods = new Hashtable();
        
        addReflectionMethod("ReflectionHandler_makeSerializedObject",
            makeMakeSerializedObject());
        addReflectionMethod("ReflectionHandler_makeObjectAndCallZeroArgConstructor",
            makeMakeObjectAndCallZeroArgConstructor());
        addReflectionMethod("ReflectionHandler_makeObjectAndCallArbitraryConstructor",
            makeMakeObjectAndCallArbitraryConstructor());
        addReflectionMethod("ReflectionHandler_callArbitraryMethod",
            makeCallArbitraryMethod());
        addReflectionMethod("ReflectionHandler_makeSerializedArray",
            makeMakeSerializedArray());
        addReflectionMethod("ReflectionHandler_assignSerializedFieldBYTE",
            makeAssignSerializedField(JBCType.BYTE));
        addReflectionMethod("ReflectionHandler_assignSerializedFieldSHORT",
            makeAssignSerializedField(JBCType.SHORT));
        addReflectionMethod("ReflectionHandler_assignSerializedFieldCHAR",
            makeAssignSerializedField(JBCType.CHAR));
        addReflectionMethod("ReflectionHandler_assignSerializedFieldINT",
            makeAssignSerializedField(JBCType.INT));
        addReflectionMethod("ReflectionHandler_assignSerializedFieldLONG",
            makeAssignSerializedField(JBCType.LONG));
        addReflectionMethod("ReflectionHandler_assignSerializedFieldFLOAT",
            makeAssignSerializedField(JBCType.FLOAT));
        addReflectionMethod("ReflectionHandler_assignSerializedFieldDOUBLE",
            makeAssignSerializedField(JBCType.DOUBLE));
        addReflectionMethod("ReflectionHandler_assignSerializedFieldBOOL",
            makeAssignSerializedField(JBCType.BOOLEAN));
        addReflectionMethod("ReflectionHandler_assignSerializedFieldOBJECT",
            makeAssignSerializedField(JBCType.OBJECT));
        addReflectionMethod("ReflectionHandler_invoke_readObject",
            makeInvokeReadObject());
    }
    
    protected ExternalFlowgraph loadFunctionCode(String name) {
        if (reflectionMethods == null) {
            makeReflectionMethods();
        }
        
        return (ExternalFlowgraph)reflectionMethods.get(name);
    }
    
    protected ExternalFlowgraph loadMethodCode(JBCMethod m) {
        return null;
    }
    
    public void notifyClassLoaded(JBCClass c) {
    }
    
    public void notifyClassChanged(JBCClass c, ClassData from, ClassData to) {
    }
    
    public void notifyNativeCodeLoaded(JBCMethod m, ExternalFlowgraph fg) {
    }
    
    public void notifyNativeCodeLoaded(String name, ExternalFlowgraph fg) {
    }
    
    public void notifyNativeCodeChanged(JBCMethod m, ExternalFlowgraph from, ExternalFlowgraph to) {
    }
    
    public void notifyNativeCodeChanged(String name, ExternalFlowgraph from, ExternalFlowgraph to) {
    }
    
    public void notifyAddedMainInvocation(String name) {
    }
}
