/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.rta;

import ajax.analyzer.*;
import ajax.util.*;
import ajax.jbc.*;
import java.util.*;
import ajax.Globals;

class RTAQueryFieldData {
    private RTAQueryFamily familyData;
    private JBCQueryFamily family;
    private RTA analysis;
    private Object queryField;
    private Hashtable sourceTypesToIntermediates = new Hashtable(5);
    private Hashtable destTypesToIntermediates = new Hashtable(5);
    private Hashtable destTypesToTargetCookies = new Hashtable(5);
    
    RTAQueryFieldData(RTAQueryFamily familyData, Object queryField) {
        this.familyData = familyData;
        this.queryField = queryField;
        family = familyData.getFamily();
        analysis = familyData.getAnalysis();
    }
    
    private void addTypeAndSupertypes(Object type) {
        CompactSet oldLeaves = new CompactSet();
        
        addTypeAndSupertypesRecursively(type, oldLeaves);
        
        for (Enumeration e = oldLeaves.elements(); e.hasMoreElements();) {
            Object leaf = e.nextElement();
            Object curIntermediate = sourceTypesToIntermediates.get(leaf);
            
            if (curIntermediate != null) {
                changeIntermediateSource(leaf, curIntermediate);
            }
        }
    }
    
    private void addTypeAndSupertypesRecursively(Object type, CompactSet oldLeaves) {
        if (!familyData.fieldUsesType(queryField, type)) {
            familyData.setFieldUsesType(queryField, type);
            
            for (Enumeration e = analysis.getSupertypes(type); e.hasMoreElements();) {
                addTypeAndSupertypesRecursively(e.nextElement(), oldLeaves);
            }
        } else {
            oldLeaves.add(type);
        }
    }
    
    private void addIntermediateSource(Object type, Object intermediate) {
        Object curIntermediate = sourceTypesToIntermediates.get(type);
        
        if (curIntermediate == null) {
            changeIntermediateSource(type, intermediate);
        } else {
            Object newIntermediate = family.joinIntermediates(curIntermediate, intermediate);
            
            if (!newIntermediate.equals(curIntermediate)) {
                changeIntermediateSource(type, newIntermediate);
            }
        }
    }
    
    private void addIntermediateTarget(Object type, Object intermediate) {
        Object curIntermediate = destTypesToIntermediates.get(type);
        
        if (curIntermediate == null) {
            changeIntermediateTarget(type, intermediate);
        } else {
            Object newIntermediate = family.joinIntermediates(curIntermediate, intermediate);
            
            if (!newIntermediate.equals(curIntermediate)) {
                changeIntermediateTarget(type, newIntermediate);
            }
        }
    }
    
    private void changeIntermediateSource(Object type, Object intermediate) {
        sourceTypesToIntermediates.put(type, intermediate);
        
        for (Enumeration e = analysis.getSubtypes(type); e.hasMoreElements();) {
            Object subtype = e.nextElement();
            
            if (familyData.fieldUsesType(queryField, subtype)) {
                addIntermediateSource(subtype, intermediate);
            }
        }
        
        if (analysis.isTypeInstantiated(type)) {
            addIntermediateTarget(type, intermediate);
        }
    }
    
    private void changeIntermediateTarget(Object type, Object intermediate) {
        destTypesToIntermediates.put(type, intermediate);
        
        for (Enumeration e = analysis.getSupertypes(type); e.hasMoreElements();) {
            /* the supertype set is, not surprisingly, closed under supertyping,
               so we don't have to check that the supertype is in the set;
               it must be. */
            addIntermediateTarget(e.nextElement(), intermediate);
        }
        
        Vector targetCookies = (Vector)destTypesToTargetCookies.get(type);
        
        if (targetCookies != null) {
            for (Enumeration e = targetCookies.elements(); e.hasMoreElements();) {
                familyData.updateResult(e.nextElement(), intermediate);
            }
        }
    }
    
    void addSourceDatum(Object type, Object intermediate) {
        addTypeAndSupertypes(type);
        addIntermediateSource(type, intermediate);
    }
    
    void addTargetDatum(Object type, Object targetCookie) {
        addTypeAndSupertypes(type);

        Vector targetCookies = (Vector)destTypesToTargetCookies.get(type);
        
        if (targetCookies == null) {
            targetCookies = new Vector();
            destTypesToTargetCookies.put(type, targetCookies);
        }
        
        targetCookies.addElement(targetCookie);
        
        Object curIntermediate = destTypesToIntermediates.get(type);
        
        if (curIntermediate != null) {
            familyData.updateResult(targetCookie, curIntermediate);
        }
    }
    
    void addSubtypeRelationship(Object supertype, Object subtype) {
        Object superIntermediate = sourceTypesToIntermediates.get(supertype);
            
        if (superIntermediate != null
            && familyData.fieldUsesType(queryField, subtype)) {
            addIntermediateSource(subtype, superIntermediate);
        }
            
        Object subIntermediate = destTypesToIntermediates.get(subtype);
            
        if (subIntermediate != null) {
            addIntermediateTarget(supertype, subIntermediate);
        }
    }
    
    void addTypeInstantiation(Object type) {
        Object curIntermediate = sourceTypesToIntermediates.get(type);
        
        if (curIntermediate != null) {
            addIntermediateTarget(type, curIntermediate);
        }
    }
}
