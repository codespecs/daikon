/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.rta;

import ajax.analyzer.*;
import ajax.util.*;
import ajax.jbc.*;
import java.util.*;
import ajax.Globals;

class RTAQueryFamily {
    private static final Object DEFAULT_QUERY_FIELD = new String("DEFAULT_QUERY_FIELD");
    
    private RTA analysis;
    private JBCQueryFamily family;
    private Hashtable queryFieldData = new Hashtable();
    private Hashtable typesToQueryFieldData = new Hashtable();
    private Hashtable targetCookiesToResults = new Hashtable();
    
    RTAQueryFamily(RTA analysis, JBCQueryFamily family) {
        this.analysis = analysis;
        this.family = family;
    }
    
    RTA getAnalysis() {
        return analysis;
    }
    
    JBCQueryFamily getFamily() {
        return family;
    }
    
    boolean fieldUsesType(Object queryField, Object type) {
        CompactSet fieldData = (CompactSet)typesToQueryFieldData.get(type);
        
        return fieldData != null && fieldData.get(queryField) != null;
    }
    
    void setFieldUsesType(Object queryField, Object type) {
        CompactSet fieldData = (CompactSet)typesToQueryFieldData.get(type);
        
        if (fieldData == null) {
            fieldData = new CompactSet();
            typesToQueryFieldData.put(type, fieldData);
        }
        
        fieldData.add(queryField);
    }
    
    private RTAQueryFieldData getFieldData(Object queryField) {
        if (queryField == null) {
            queryField = DEFAULT_QUERY_FIELD;
        }
        
        RTAQueryFieldData data = (RTAQueryFieldData)queryFieldData.get(queryField);
        
        if (data == null) {
            data = new RTAQueryFieldData(this, queryField);
            queryFieldData.put(queryField, data);
        }
        
        return data;
    }
    
    void addSourceDatum(Enumeration types, Object queryField, Object intermediate) {
        RTAQueryFieldData fieldData = getFieldData(queryField);
        
        while (types.hasMoreElements()) {
            fieldData.addSourceDatum(types.nextElement(), intermediate);
        }
    }
    
    void addTargetDatum(Enumeration types, Object queryField, Object targetCookie) {
        RTAQueryFieldData fieldData = getFieldData(queryField);
        
        while (types.hasMoreElements()) {
            fieldData.addTargetDatum(types.nextElement(), targetCookie);
        }
    }
    
    void addSubtypeRelationship(Object supertype, Object subtype) {
        CompactSet fieldData = (CompactSet)typesToQueryFieldData.get(subtype);
        
        if (fieldData != null) {
            for (Enumeration e = fieldData.enumerateElementsSafely(); e.hasMoreElements();) {
                getFieldData(e.nextElement()).addSubtypeRelationship(supertype, subtype);
            }
        }
    }
    
    void addTypeInstantiation(Object type) {
        CompactSet fieldData = (CompactSet)typesToQueryFieldData.get(type);
        
        if (fieldData != null) {
            for (Enumeration e = fieldData.enumerateElementsSafely(); e.hasMoreElements();) {
                getFieldData(e.nextElement()).addTypeInstantiation(type);
            }
        }
    }
    
    void updateResult(Object cookie, Object intermediate) {
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
        
        analysis.getConsumer().updateResult(family, cookie, intermediate);
    }
}
