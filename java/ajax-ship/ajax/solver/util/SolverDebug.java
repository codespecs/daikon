/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver.util;

import ajax.util.*;
import ajax.solver.*;
import java.io.*;
import ajax.util.ArrayEnumerator;
import java.util.*;
import ajax.Globals;

public class SolverDebug {
    private static String varString(Variable v) {
        return "\"" + Globals.getHexID(v.getHead()) + "\"";
    }
    
    private static void checkAppendVarInfo(Variable v, Hashtable visited, Stack varsToExamine) {
        v = v.getHead();
        
        if (visited.get(v) == null) {
            visited.put(v, v);
            
            varsToExamine.push(v);
        }
    }
    
    public static Object checkHasParent(World w, Variable v, Variable parentHead, ComponentLabel label) {
        for (Enumeration e = v.getParents(w); e.hasMoreElements();) {
            ParentElement s = (ParentElement)e.nextElement();
            
            if (s.getParentHead() == parentHead && s.getLabel().equals(label)) {
                return s;
            }
        }
        
        return null;
    }
    
    public static Object checkHasSource(World w, Variable v, Variable sourceHead, InstanceLabel label) {
        for (Enumeration e = v.getSources(w); e.hasMoreElements();) {
            SourceElement s = (SourceElement)e.nextElement();
            
            if (s.getSourceHead() == sourceHead && s.getLabel().equals(label)) {
                return s;
            }
        }
        
        return null;
    }
    
    static Object checkHasComponent(World w, Variable v, Variable componentHead, ComponentLabel label) {
        for (Enumeration e = v.getComponents(w); e.hasMoreElements();) {
            ComponentElement i = (ComponentElement)e.nextElement();
            
            if (i.getComponentHead() == componentHead && i.getLabel().equals(label)) {
                return i;
            }
        }
        
        return null;
    }
    
    static Object checkHasInstance(World w, Variable v, Variable instanceHead, InstanceLabel label) {
        for (Enumeration e = v.getInstances(w); e.hasMoreElements();) {
            InstanceElement i = (InstanceElement)e.nextElement();
            
            if (i.getInstanceHead() == instanceHead && i.getLabel().equals(label)) {
                return i;
            }
        }
        
        return null;
    }
    
    private static void appendVarInfo(Variable v, Hashtable visited, ConstraintAnnotator annotator,
        Writer w, Stack varsToExamine, World solver) throws IOException {
        w.write(varString(v));
        
        if (annotator != null) {
            w.write(" [label=\"" + Quote.quote(annotator.getVarLabel(v)) + "\"]");
        }
        w.write(";\n");
        
        for (Enumeration e = v.getInstances(solver); e.hasMoreElements();) {
            InstanceElement elem = (InstanceElement)e.nextElement();
            Variable to = elem.getInstance();
            
            w.write(varString(v) + " -> " + varString(to) + " [style=dashed, label=\""
                + Quote.quote(elem.getLabel().toString()));
            
            Object o = checkHasSource(solver, to, v.getHead(), elem.getLabel());
            if (o == null) {
                w.write("INVALID: Missing source!");
                checkHasSource(solver, to, v.getHead(), elem.getLabel());
            } else if (o != elem) {
                w.write("INVALID: Mismatched source!");
            }
            
            try {
                elem.validate(solver);
            } catch (ValidationException ex) {
                w.write("\\nINVALID: " + ex.getMessage());
            }
            
            w.write("\"];\n");
        }
        
        for (Enumeration e = v.getComponents(solver); e.hasMoreElements();) {
            ComponentElement elem = (ComponentElement)e.nextElement();
            Variable component = elem.getComponent();
            
            w.write(varString(v) + " -> " + varString(component)
                + " [label=\"" + Quote.quote(elem.getLabel().toString()));
            
            Object o = checkHasParent(solver, component, v.getHead(), elem.getLabel());
            if (o == null) {
                w.write("INVALID: Missing parent!");
                checkHasParent(solver, component, v.getHead(), elem.getLabel());
            } else if (o != elem) {
                w.write("INVALID: Mismatched parent!");
            }
            
            try {
                elem.validate(solver);
            } catch (ValidationException ex) {
                w.write("\\nINVALID: " + ex.getMessage());
            }
            
            w.write("\\n" + elem.toString() + "\"];\n");
        }
        
        for (Enumeration e = v.getParents(solver); e.hasMoreElements();) {
            ParentElement elem = (ParentElement)e.nextElement();
            Variable parentVar = elem.getParent();
            
            Object o = checkHasComponent(solver, parentVar, v, elem.getLabel());
            
            if (o == null) {
                w.write("INVALID: Missing component!");
                checkHasComponent(solver, parentVar, v, elem.getLabel());
            } else if (o != elem) {
                w.write("INVALID: Mismatched component!");
            }
        }
        
        for (Enumeration e = v.getSources(solver); e.hasMoreElements();) {
            SourceElement elem = (SourceElement)e.nextElement();
            Variable sourceVar = elem.getSource();
            
            Object o = checkHasInstance(solver, sourceVar, v, elem.getLabel());
            if (o == null) {
                w.write("INVALID: Missing instance!");
                checkHasInstance(solver, sourceVar, v, elem.getLabel());
            } else if (o != elem) {
                w.write("INVALID: Mismatched instance!");
            }
        }

        for (Enumeration e = v.getInstances(solver); e.hasMoreElements();) {
            annotator.countInstance();
            checkAppendVarInfo(((InstanceElement)e.nextElement()).getInstance(), visited, varsToExamine);
        }

        for (Enumeration e = v.getComponents(solver); e.hasMoreElements();) {
            annotator.countComponent();
            checkAppendVarInfo(((ComponentElement)e.nextElement()).getComponent(), visited, varsToExamine);
        }

        for (Enumeration e = v.getSources(solver); e.hasMoreElements();) {
            annotator.countSource();
            checkAppendVarInfo(((SourceElement)e.nextElement()).getSource(), visited, varsToExamine);
        }

        for (Enumeration e = v.getParents(solver); e.hasMoreElements();) {
            annotator.countParent();
            checkAppendVarInfo(((ParentElement)e.nextElement()).getParent(), visited, varsToExamine);
        }
    }
    
    public static String dumpVarInfo(Variable[] vs, World solver) {
        StringWriter w = new StringWriter();
        
        try {
            dumpVarInfo(null, w, solver, new ArrayEnumerator(vs));
            w.close();
            return w.toString();
        } catch (IOException ex) {
            return "<unexpected error>";
        }
    }
    
    public static String dumpVarInfo(Vector bindingList, World solver) {
        StringWriter w = new StringWriter();
        BindingListAnnotator annotator = new BindingListAnnotator(bindingList);

        try {
            dumpVarInfo(annotator, w, solver, annotator.getBoundVars());
            w.close();
            return w.toString();
        } catch (IOException ex) {
            return "<unexpected error>";
        }
    }
    
    public static void dumpVarInfo(ConstraintAnnotator annotator, Writer w, World solver, Enumeration vars) throws IOException {
        Hashtable visited = new Hashtable();
        Stack varsToExamine = new Stack();
        
        w.write("digraph g {\n");
        for (; vars.hasMoreElements();) {
            checkAppendVarInfo((Variable)vars.nextElement(), visited, varsToExamine);
        }
        while (!varsToExamine.isEmpty()) {
            appendVarInfo((Variable)varsToExamine.pop(), visited, annotator, w, varsToExamine, solver);
        }
        w.write("}\n");
    }
}
