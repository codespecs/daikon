/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi;

import ajax.Globals;
import ajax.solver.*;
import ajax.jbc.*;
import java.util.*;
import java.io.*;
import ajax.util.*;
import ajax.analyzer.*;
import ajax.analyzer.semi.constraints.ConstraintManager;

class SEMIQueryFamily {
/*
Cost estimates for these tables:
V = #distinct variables
Q = #distinct query fields
Q is much less than V. Often, Q is 1.

Cost of V -> Q -> intermediate is
    3*sizeof(IdentityHashtable(V)) + 3*V*sizeof(Hashtable(Q))
= 3*IdentityHashtable + 3*V*IdentityHashtableEntry
  + 3*V*Hashtable * 3*V*Q*HashtableEntry
    
Cost of Q -> V -> intermediate is
    sizeof(Hashtable(Q)) + Q*sizeof(SEMIQueryFieldData)
    + 3*Q*sizeof(IdentityHashtable(V))
    + sizeof(IdentityHashtable(V)) + V*sizeof(CompactSet(Q))
= Hashtable + Q*SEMIQueryFieldData + 3*Q*IdentityHashtable
  + 3*Q*V*IdentityHashtableEntry
  + IdentityHashtable + V*IdentityHashtableEntry
  + V*CompactSet + V*Q*CompactSetEntry
  
Former - latter
>= IdentityHashtable + 2*V*IdentityHashtableEntry
   + 3*V*Hashtable * V*Q*(3*HashtableEntry - 3*IdentityHashtableEntry - CompactSetEntry)
   - Q*SEMIQueryFieldData - 3*Q*IdentityHashtable
   - V*CompactSet
*/
    private Hashtable queryFieldToData = new Hashtable(5);
    private Hashtable targetCookiesToResults = new Hashtable(5);
    private IdentityHashtable varsToRelevantQueryFields = new IdentityHashtable(5);

    private JBCQueryFamily family;
    private GenericAnalyzerConsumer consumer;
    private World w;
    
    private static final Object DUMMY_INTERMEDIATE = new String("DUMMY_INTERMEDIATE");
    
    SEMIQueryFamily(World w, JBCQueryFamily family, GenericAnalyzerConsumer consumer) {
        this.family = family;
        this.consumer = consumer;
        this.w = w;
    }
    
    private SEMIQueryFieldData getQueryFieldData(Object queryField) {
        Object o = queryFieldToData.get(queryField);
        
        if (o != null) {
            return (SEMIQueryFieldData)o;
        } else {
            SEMIQueryFieldData data = new SEMIQueryFieldData();
            
            queryFieldToData.put(queryField, data);
            
            return data;
        }
    }
    
    private void updateResult(Object cookie, Object intermediate) {
        Object curResult = targetCookiesToResults.get(cookie);
        
        if (curResult == null) {
            targetCookiesToResults.put(cookie, intermediate);
        } else {
            intermediate = family.joinIntermediates(curResult, intermediate);
            
            if (intermediate.equals(curResult)) {
                return;
            }
            targetCookiesToResults.put(cookie, intermediate);
        }
        
        consumer.updateResult(family, cookie, intermediate);
    }
    
    private void associateVarWithField(Variable vHead, Object queryField) {
        if (Globals.debug && queryField instanceof CompactSet) {
            Globals.localError("Cannot handle CompactSet queryFields");
        }
        
        Object o = varsToRelevantQueryFields.get(vHead);
        
        if (o == null) {
            varsToRelevantQueryFields.put(vHead, queryField);
        } else if (o instanceof CompactSet) {
            ((CompactSet)o).add(queryField);
        } else if (!o.equals(queryField)) {
            Object[] objs = { o, queryField };
            
            varsToRelevantQueryFields.put(vHead,
                new CompactSet(2, new ArrayEnumerator(objs)));
        }
    }
    
    private int countAssociatedQueryFields(Variable vHead) {
        Object o = varsToRelevantQueryFields.get(vHead);
        
        if (o == null) {
            return 0;
        } else if (o instanceof CompactSet) {
            return ((CompactSet)o).size();
        } else {
            return 1;
        }
    }
    
    private Enumeration enumerateAssociatedQueryFields(Variable vHead) {
        Object o = varsToRelevantQueryFields.get(vHead);
        
        if (o == null) {
            return EmptyEnumerator.get();
        } else if (o instanceof CompactSet) {
            return ((CompactSet)o).elements();
        } else {
            return new SingletonEnumerator(o);
        }
    }
    
    private void changeIntermediateUp(Variable vHead, Object intermediate, Object queryField, SEMIQueryFieldData data) {
        data.varToIntermediateUp.put(vHead, intermediate);
        
        for (Enumeration e = vHead.getInstances(w); e.hasMoreElements();) {
            InstanceElement elem = (InstanceElement)e.nextElement();
            if (!SEMIAnalyzer.applyPolarities
                || (elem.getPolarity() & (1 << Variable.PMODE)) != 0) {
                addIntermediateUp(elem.getInstanceHead(),
                    intermediate, queryField, data);
	    }
        }
        
        addIntermediateDown(vHead, intermediate, data);
    }
    
    private void addIntermediateUp(Variable vHead, Object intermediate, Object queryField, SEMIQueryFieldData data) {
        Object curIntermediate = data.varToIntermediateUp.get(vHead);
        
        if (curIntermediate == null) {
            associateVarWithField(vHead, queryField);
            changeIntermediateUp(vHead, intermediate, queryField, data);
        } else if (curIntermediate != intermediate) {
            Object newIntermediate = family.joinIntermediates(intermediate, curIntermediate);
            
            if (!newIntermediate.equals(curIntermediate)) {
                changeIntermediateUp(vHead, newIntermediate, queryField, data);
            }
        }
    }
    
    private void changeIntermediateDown(Variable vHead, Object intermediate, SEMIQueryFieldData data) {
        data.varToIntermediateDown.put(vHead, intermediate);
        
        for (Enumeration e = vHead.getSources(w); e.hasMoreElements();) {
            SourceElement elem = (SourceElement)e.nextElement();
            if (!SEMIAnalyzer.applyPolarities
                || (elem.getPolarity() & (1 << Variable.NMODE)) != 0) {
                addIntermediateDown(elem.getSourceHead(),
                    intermediate, data);
	    }
        }
        
        for (Enumeration e = enumerateTargetCookies(data.varToTargetCookies.get(vHead)); e.hasMoreElements();) {
            updateResult(e.nextElement(), intermediate);
        }
    }
    
    private void addIntermediateDown(Variable vHead, Object intermediate, SEMIQueryFieldData data) {
        Object curIntermediate = data.varToIntermediateDown.get(vHead);
            
        if (curIntermediate != null && curIntermediate != intermediate) {
            Object newIntermediate = curIntermediate == DUMMY_INTERMEDIATE
                ? intermediate : family.joinIntermediates(intermediate, curIntermediate);
                
            if (!newIntermediate.equals(curIntermediate)) {
                changeIntermediateDown(vHead, newIntermediate, data);
            }
        }
    }
    
    private static Enumeration enumerateTargetCookies(Object set) {
        if (set == null) {
            return EmptyEnumerator.get();
        } else if (set instanceof IdentityCompactSet) {
            return ((IdentityCompactSet)set).elements();
        } else {
            return new SingletonEnumerator(set);
        }
    }

    private static int countTargetCookies(Object set) {
        if (set == null) {
            return 0;
        } else if (set instanceof IdentityCompactSet) {
            return ((IdentityCompactSet)set).size();
        } else {
            return 1;
        }
    }
    
    private static Object addTargetCookie(Variable vHead, Object curSet, Object targetCookie,
        SEMIQueryFieldData data) {
        if (curSet == null) {
            data.varToTargetCookies.put(vHead, targetCookie);
            return targetCookie;
        } else if (curSet instanceof IdentityCompactSet) {
            IdentityCompactSet set = (IdentityCompactSet)curSet;
            
            set.add(targetCookie);
            return set;
        } else if (curSet == targetCookie) {
            return curSet;
        } else {
            Object[] cookies = { curSet, targetCookie };
            IdentityCompactSet set = new IdentityCompactSet(2, new ArrayEnumerator(cookies));
            
            data.varToTargetCookies.put(vHead, set);
            return set;
        }
    }

    private void mergeAssociatedFields(Variable into, Variable from) {
        for (Enumeration e = enumerateAssociatedQueryFields(from); e.hasMoreElements();) {
            associateVarWithField(into, e.nextElement());
        }
    }
    
    void notifyVarsMerged(Variable to, Variable from) {
        /* first, merge the query field associations */
        if (countAssociatedQueryFields(to) < countAssociatedQueryFields(from)) {
            mergeAssociatedFields(from, to);
            varsToRelevantQueryFields.put(to, varsToRelevantQueryFields.remove(from));
        } else {
            mergeAssociatedFields(to, from);
            varsToRelevantQueryFields.remove(from);
        }
        
        for (Enumeration e = enumerateAssociatedQueryFields(to); e.hasMoreElements();) {
            Object queryField = e.nextElement();
            SEMIQueryFieldData data = (SEMIQueryFieldData)queryFieldToData.get(queryField);
            
            Object toIntermediateUp = data.varToIntermediateUp.get(to);
            Object fromIntermediateUp = data.varToIntermediateUp.get(from);
            
            if (fromIntermediateUp != null) {
                addIntermediateUp(to, fromIntermediateUp, queryField, data);
            }
            if (toIntermediateUp != null) {
                addIntermediateUp(from, toIntermediateUp, queryField, data);
            }
            
            if (data.varToIntermediateDown.get(from) != null) {
                pushTargetIntermediate(to, queryField, data, false);
            }
            if (data.varToIntermediateDown.get(to) != null) {
                pushTargetIntermediate(from, queryField, data, false);
            }
            
            Object toIntermediateDown = data.varToIntermediateDown.get(to);
            Object fromIntermediateDown = data.varToIntermediateDown.get(from);
            
            if (fromIntermediateDown != null && fromIntermediateDown != DUMMY_INTERMEDIATE) {
                addIntermediateDown(to, fromIntermediateDown, data);
            }
            if (toIntermediateDown != null && toIntermediateDown != DUMMY_INTERMEDIATE) {
                addIntermediateDown(from, toIntermediateDown, data);
            }
            
            if (Globals.debug) {
                Object curToIntermediateDown = data.varToIntermediateDown.get(to);
                Object curFromIntermediateDown = data.varToIntermediateDown.get(from);
            
                if (curToIntermediateDown != curFromIntermediateDown
                    && (curToIntermediateDown == null || !curToIntermediateDown.equals(curFromIntermediateDown))) {
                    Globals.localError("Inconsistent intermediates in merge: "
                        + curToIntermediateDown + ", " + curFromIntermediateDown
                        + "(originally " + toIntermediateDown + ", " + fromIntermediateDown + ")");
                }
            }
            
            Object toTargets = data.varToTargetCookies.get(to);
            Object fromTargets = data.varToTargetCookies.get(from);
            
            if (countTargetCookies(toTargets) < countTargetCookies(fromTargets)) {
                for (Enumeration e2 = enumerateTargetCookies(toTargets); e2.hasMoreElements();) {
                    fromTargets = addTargetCookie(from, fromTargets, e2.nextElement(), data);
                }
                data.varToTargetCookies.put(to, fromTargets);
            } else {
                for (Enumeration e2 = enumerateTargetCookies(fromTargets); e2.hasMoreElements();) {
                    toTargets = addTargetCookie(to, toTargets, e2.nextElement(), data);
                }
            }
            
            data.varToTargetCookies.remove(from);
            data.varToIntermediateUp.remove(from);
            data.varToIntermediateDown.remove(from);
        }
    }
    
    void notifyNewInstance(Variable instance, Variable of) {
        for (Enumeration e = enumerateAssociatedQueryFields(of); e.hasMoreElements();) {
            Object queryField = e.nextElement();
            SEMIQueryFieldData data = (SEMIQueryFieldData)queryFieldToData.get(queryField);
            Object ofIntermediateUp = data.varToIntermediateUp.get(of);
            
            if (ofIntermediateUp != null) {
                addIntermediateUp(instance, ofIntermediateUp, queryField, data);
            }
            pushTargetIntermediate(instance, queryField, data, false);
        }
        
        for (Enumeration e = enumerateAssociatedQueryFields(instance); e.hasMoreElements();) {
            Object queryField = e.nextElement();
            SEMIQueryFieldData data = (SEMIQueryFieldData)queryFieldToData.get(queryField);
            Object intermediate = data.varToIntermediateDown.get(instance);
                
            if (intermediate != null && intermediate != DUMMY_INTERMEDIATE) {
                addIntermediateDown(of, intermediate, data);
            }
        }
    }
    
    void addSourceDatum(Variable[] vs, JBCExpressionContext context, Object intermediate, Object queryField) {
        if (vs != null) {
  	    for (int i = 0; i < vs.length; i++) {
                addIntermediateUp(vs[i].getHead(), intermediate, queryField, getQueryFieldData(queryField));
	    }
        }
    }
    
/**
We have a new intermediate. This may cause stuff above us to be filled in.
*/
    private void pushTargetIntermediate(Variable vHead, Object queryField, SEMIQueryFieldData data, boolean force) {
        IdentityCompactSet downLeaves = new IdentityCompactSet();
        IdentityCompactSet upLeaves = new IdentityCompactSet();
        
        pushTargetIntermediateRecursive(vHead, queryField, data, force, downLeaves, upLeaves);
        
        for (Enumeration e = upLeaves.elements(); e.hasMoreElements();) {
            Variable leafHead = (Variable)e.nextElement();
            Object intermediate = data.varToIntermediateUp.get(leafHead);
                
            if (intermediate != null) {
                addIntermediateDown(leafHead, intermediate, data);
            }
        }
        
        for (Enumeration e = downLeaves.elements(); e.hasMoreElements();) {
            Variable leafHead = (Variable)e.nextElement();
            Object intermediate = data.varToIntermediateDown.get(leafHead);
                
            if (intermediate != DUMMY_INTERMEDIATE) {
                for (Enumeration e2 = leafHead.getSources(w); e2.hasMoreElements();) {
                    addIntermediateDown(((SourceElement)e2.nextElement()).getSourceHead(),
                        intermediate, data);
                }
            }
        }
    }
    
/**
Propagate DUMMY_INTERMEDIATE to all the instances of vHead, and their instances, etc.
If 'force' is set then we scan all instances, otherwise we stop when we find an
intermediate present. 'upLeaves' collects all variables scanned. 'downLeaves' collects
all the places where we found an intermediate already present.
*/
    private void pushTargetIntermediateRecursive(Variable vHead, Object queryField,
        SEMIQueryFieldData data, boolean force, IdentityCompactSet downLeaves, IdentityCompactSet upLeaves) {
        Object curIntermediate = data.varToIntermediateDown.get(vHead);
        
        if (curIntermediate == null || force) {
            if (curIntermediate == null) {
                data.varToIntermediateDown.put(vHead, DUMMY_INTERMEDIATE);
                associateVarWithField(vHead, queryField);
            }
            
            for (Enumeration e = vHead.getInstances(w); e.hasMoreElements();) {
                Variable instanceHead = ((InstanceElement)e.nextElement()).getInstanceHead();
                
                pushTargetIntermediateRecursive(instanceHead, queryField, data, false,
                    downLeaves, upLeaves);
            }
            
            upLeaves.add(vHead);
        } else {
            downLeaves.add(vHead);
        }
    }
    
    private void dumpTable(Writer w, String tableName, IdentityHashtable table) throws IOException {
        for (Enumeration e = table.keys(); e.hasMoreElements();) {
            Variable v = (Variable)e.nextElement();
            
            w.write(tableName + " for " + Globals.getHexID(v.getHead()) + ": " + table.get(v) + "\n");
        }
    }
    
    void dump(Writer w) throws IOException {
        w.write("Begin query family\n\n");
        
        for (Enumeration e = queryFieldToData.keys(); e.hasMoreElements();) {
            Object queryField = e.nextElement();
            SEMIQueryFieldData data = (SEMIQueryFieldData)queryFieldToData.get(queryField);
            
            w.write("Query field: " + queryField + "\n\n");
            dumpTable(w, "Intermediate-Up", data.varToIntermediateUp);
            dumpTable(w, "Intermediate-Down", data.varToIntermediateDown);
            dumpTable(w, "Targets", data.varToTargetCookies);
            w.write("\n");
        }
    }
    
    void addTargetDatum(Variable[] vs, JBCExpressionContext context, Object targetCookie, Object queryField) {
        if (vs != null) {
   	    for (int i = 0; i < vs.length; i++) {
                Variable vHead = vs[i].getHead();
		SEMIQueryFieldData data = getQueryFieldData(queryField);
            
		pushTargetIntermediate(vHead, queryField, data, false);
		associateVarWithField(vHead, queryField);
		addTargetCookie(vHead, data.varToTargetCookies.get(vHead), targetCookie, data);
                
		Object intermediate = data.varToIntermediateDown.get(vHead);
            
		if (intermediate != DUMMY_INTERMEDIATE) {
		  updateResult(targetCookie, intermediate);
		}
	    }
	}
    }
    
    private Variable[] getVarArray(Variable v, Variable boundTo, Variable boundFrom) {
        if (v == boundTo) {
            Variable[] result = { v, boundFrom };
            return result;
        } else if (v == boundFrom) {
            Variable[] result = { v, boundTo };
            return result;
        } else {
            Variable[] result = { v };
            return result;
        }
    }
    
    private boolean subsumesIntermediate(Object foundIntermediate, Object actualIntermediate) {
        if (actualIntermediate == null) {
            return true;
        } else if (foundIntermediate == null) {
            return false;
        } else {
            return family.joinIntermediates(foundIntermediate, actualIntermediate).equals(foundIntermediate);
        }
    }
    
    void validate(Variable boundTo, Variable boundFrom) {
        for (Enumeration e = queryFieldToData.keys(); e.hasMoreElements();) {
            Object queryField = e.nextElement();
            SEMIQueryFieldData data = (SEMIQueryFieldData)queryFieldToData.get(queryField);
            
            for (Enumeration e2 = data.varToIntermediateUp.keys(); e2.hasMoreElements();) {
                Variable v = (Variable)e2.nextElement();
                Object realIntermediateUp = data.varToIntermediateUp.get(v);
                Object intermediate = realIntermediateUp;
                Variable[] vars = getVarArray(v, boundTo, boundFrom);
                
                for (int i = 0; i < vars.length; i++) {
                    for (Enumeration e3 = vars[i].getSources(w); e3.hasMoreElements();) {
                        Variable[] sourceHeads = getVarArray(((SourceElement)e3.nextElement()).getSourceHead(), boundTo, boundFrom);
                        
                        for (int j = 0; j < sourceHeads.length; j++) {
                            Object sourceIntermediateUp = data.varToIntermediateUp.get(sourceHeads[j]);
                            
                            if (sourceIntermediateUp != null) {
                                if (intermediate != null) {
                                    intermediate = family.joinIntermediates(intermediate, sourceIntermediateUp);
                                } else {
                                    intermediate = sourceIntermediateUp;
                                }
                            }
                        }
                    }
                }
                
                if (!subsumesIntermediate(realIntermediateUp, intermediate)) {
                    Globals.localError("Intermediate-up error on field " + queryField + ": " + intermediate + ", " + realIntermediateUp);
                }
            }
            
            for (Enumeration e2 = data.varToIntermediateDown.keys(); e2.hasMoreElements();) {
                Variable v = (Variable)e2.nextElement();
                Object intermediate = data.varToIntermediateUp.get(v);
                Object realIntermediateDown = data.varToIntermediateDown.get(v);
                
                if (intermediate == DUMMY_INTERMEDIATE) {
                    intermediate = null;
                }
                if (realIntermediateDown == DUMMY_INTERMEDIATE) {
                    realIntermediateDown = null;
                }
                
                Variable[] vars = getVarArray(v, boundTo, boundFrom);
                
                for (int i = 0; i < vars.length; i++) {
                    for (Enumeration e3 = vars[i].getInstances(w); e3.hasMoreElements();) {
                        Variable[] instanceHeads = getVarArray(((InstanceElement)e3.nextElement()).getInstanceHead(), boundTo, boundFrom);
                        
                        for (int j = 0; j < instanceHeads.length; j++) {
                            Object instanceIntermediateUp = data.varToIntermediateDown.get(instanceHeads[j]);
                            
                            if (instanceIntermediateUp != null && instanceIntermediateUp != DUMMY_INTERMEDIATE) {
                                if (intermediate != null) {
                                    intermediate = family.joinIntermediates(intermediate, instanceIntermediateUp);
                                } else {
                                    intermediate = instanceIntermediateUp;
                                }
                            }
                        }
                    }
                }
                
                if (!subsumesIntermediate(realIntermediateDown, intermediate)) {
                    Globals.localError("Intermediate-down error on field " + queryField + ": " + intermediate + ", " + realIntermediateDown);
                }
            }
        }
    }
    
    protected void finalize() {
        if (Globals.debug) {
            validate(null, null);
        }
    }
}
