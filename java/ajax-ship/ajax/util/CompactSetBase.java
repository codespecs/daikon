/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util;

import ajax.Globals;
import java.util.Enumeration;

public abstract class CompactSetBase implements Cloneable {
    private int count = 0;
    private Object[] items;
    private Object[][] overflows = null;
    
/** This provides the comparison operation. o1 is always an object
    currently in the set; o2 is always an object not currently in the set.
*/
    abstract protected boolean checkEqual(Object o1, Object o2);
    abstract protected int getHash(Object o);
    
/** If the number of elements is less than or equal to
    simpleArrayThreshold, then we just list the elements in the "items"
    array.
*/
    private static final int simpleArrayThreshold = 8;

/** Otherwise, we store into the "items" and "overflows" arrays.
    The length of the "items" array will be > (count*countMultiple)/4. */
    private static final int countMultiple = 4;
    
/** The length of the "overflows" array will = items.length/overflowFactor. */
    private static final int overflowFactor = 32;
    
    private static final int magicHashDelta = 0x718F141B;
    
    public Object clone() {
        try {
            return ((CompactSetBase)super.clone()).dup();
        } catch (CloneNotSupportedException ex) {
            return null;
        }
    }
    
    private static int nominalItemsHashLength(int count) {
        return (count*countMultiple)/4;
    }
    
    private boolean isInArrayForm() {
        return count <= simpleArrayThreshold;
    }
    
    private Object dup() {
        if (items != null) {
            items = (Object[])items.clone();
        }
        
        if (overflows != null) {
            overflows = (Object[][])overflows.clone();
        }
                    
        return this;
    }
    
    protected void notifyInitialElementAdded(Object obj) {
    }
    
    protected CompactSetBase(int initialSize, Enumeration initialElements) {
        if (initialSize == 0) {
            items = null;
        } else if (initialSize <= simpleArrayThreshold) {
            count = initialSize;
            items = new Object[initialSize];
            overflows = null;
            
            for (int i = 0; i < items.length; i++) {
                Object obj = initialElements.nextElement();
                
                items[i] = obj;
                notifyInitialElementAdded(obj);
            }
        } else {
            items = new Object[nominalItemsHashLength(initialSize) + 1];
            
            for (int i = 0; i < initialSize; i++) {
                Object obj = initialElements.nextElement();
                
                addUnconditionallyInHashForm(obj);
                notifyInitialElementAdded(obj);
            }
        }
        
        if (Globals.debug && initialElements.hasMoreElements()) {
            Globals.localError("Too many elements in initializer enumeration");
        }
    }
    
    protected CompactSetBase() {
        items = null;
    }
    
    public int size() {
        return count;
    }
    
    private void rehash() {
        int curCount = count;
        Enumeration e = new CompactSetEnumerator(items, overflows);
        int newSize = (nominalItemsHashLength(curCount)*3)/2 + 1;
        
        if ((newSize & 0x1) == 0) {
            newSize++;
        }
        
        // set items length to minimum size*3/2 + 1, rounded up to an odd number
        items = new Object[newSize];
        overflows = null;
        count = 0;
        
        while (e.hasMoreElements()) {
            addUnconditionallyInHashForm(e.nextElement());
        }
        
        if (Globals.debug && curCount != count) {
            Globals.localError("Mismatched elements after rehash");
        }
    }
    
    private void setArrayFromEnumeration(Enumeration e) {
        items = new Object[count];
        overflows = null;
        for (int i = 0; i < items.length; i++) {
            items[i] = e.nextElement();
        }
    }
    
    private void addUnconditionallyInHashForm(Object obj) {
        int itemsLen = items.length;
        
        if (nominalItemsHashLength(count + 1) > itemsLen) {
            rehash();
            itemsLen = items.length;
        }
        
        count++;
        
        int hash = getHash(obj);
        int hash1 = hash & 0x7FFFFFFF;
        int itemSlot1 = hash1 % itemsLen;
        
        if (items[itemSlot1] == null) {
            items[itemSlot1] = obj;
            return;
        }
        
        int hash2 = (hash + magicHashDelta) & 0x7FFFFFFF;
        int itemSlot2 = hash2 % itemsLen;
        
        if (items[itemSlot2] == null) {
            items[itemSlot2] = obj;
            return;
        }
        
        if (overflows == null) {
            overflows = new Object[itemsLen/overflowFactor + 1][];
        }
        
        int overflowLen = overflows.length;
        int overflowSlot1 = hash1 % overflowLen;
        Object[] overflows1 = overflows[overflowSlot1];
        
        if (overflows1 == null) {
            Object[] list = { obj };
            
            overflows[overflowSlot1] = list;
            return;
        }
        
        int overflowSlot2 = hash2 % overflowLen;
        Object[] overflows2 = overflows[overflowSlot2];
        
        if (overflows2 == null) {
            Object[] list = { obj };
            
            overflows[overflowSlot2] = list;
            return;
        }
        
        if (overflows1.length <= overflows2.length) {
            overflows[overflowSlot1] = insertIntoObjArray(overflows1, obj);
        } else {
            overflows[overflowSlot2] = insertIntoObjArray(overflows2, obj);
        }
    }
    
    public void addUnconditionally(Object obj) {
        if (obj == null) {
            throw new NullPointerException("Cannot add null reference to set");
        }
        
        if (isInArrayForm()) {
            if (count + 1 > simpleArrayThreshold) {
                Object[] oldItems = items;
                    
                items = new Object[oldItems.length];
                count = 0;
                    
                for (int i = 0; i < oldItems.length; i++) {
                    addUnconditionallyInHashForm(oldItems[i]);
                }
                
                addUnconditionallyInHashForm(obj);
            } else {
                items = insertIntoObjArray(items, obj);
                count++;
            }
        } else {
            addUnconditionallyInHashForm(obj);
        }
    }
    
    private static Object[] insertIntoObjArray(Object[] elems, Object obj) {
        if (elems != null) {
            Object[] result = new Object[elems.length + 1];
                
            result[0] = obj;
            System.arraycopy(elems, 0, result, 1, elems.length);
            
            return result;
        } else {
            Object[] result = { obj };
            
            return result;
        }
    }
    
    private static Object[] deleteFromObjArray(Object[] elems, int i) {
        if (elems.length <= 1) {
            return null;
        } else {
            Object[] result = new Object[elems.length - 1];
            
            System.arraycopy(elems, 0, result, 0, i);
            System.arraycopy(elems, i + 1, result, i, result.length - i);
            
            return result;
        }
    }
    
    private void normalizeHashDeletion() {
        if (count <= simpleArrayThreshold) {
            setArrayFromEnumeration(new CompactSetEnumerator(items, overflows));
        } else if (nominalItemsHashLength(count) < items.length/3) {
            rehash();
        }
    }
    
    public Object remove(Object obj) {
        if (isInArrayForm()) {
            if (items != null) {
                for (int i = 0; i < items.length; i++) {
                    Object o = items[i];
                    
                    if (checkEqual(o, obj)) {
                        items = deleteFromObjArray(items, i);
                        count--;
                        return o;
                    }
                }
            }
            
            return null;
        } else {
            return removeInHashForm(obj);
        }
    }
            
    private Object removeInHashForm(Object obj) {
        int hash = getHash(obj);
        
        {   int hash1 = hash & 0x7FFFFFFF;
            int slotNum = hash1 % items.length;
            Object slotObj = items[slotNum];
            
            if (slotObj != null && checkEqual(slotObj, obj)) {
                count--;
                items[slotNum] = null;
                normalizeHashDeletion();
                return slotObj;
            }
            
            if (overflows != null) {
                int overflowNum = hash1 % overflows.length;
                Object[] curList = overflows[overflowNum];
                    
                if (curList != null) {
                    for (int i = curList.length - 1; i >= 0; i--) {
                        Object o = curList[i];
                            
                        if (checkEqual(o, obj)) {
                            count--;
                            overflows[overflowNum] = deleteFromObjArray(curList, i);
                            normalizeHashDeletion();
                            return o;
                        }
                    }
                }
            }
        }

        {   int hash2 = (hash + magicHashDelta) & 0x7FFFFFFF;
            int slotNum = hash2 % items.length;
            Object slotObj = items[slotNum];
            
            if (slotObj != null && checkEqual(slotObj, obj)) {
                count--;
                items[slotNum] = null;
                normalizeHashDeletion();
                return slotObj;
            }
            
            if (overflows != null) {
                int overflowNum = hash2 % overflows.length;
                Object[] curList = overflows[overflowNum];
                    
                if (curList != null) {
                    for (int i = curList.length - 1; i >= 0; i--) {
                        Object o = curList[i];
                            
                        if (checkEqual(o, obj)) {
                            count--;
                            overflows[overflowNum] = deleteFromObjArray(curList, i);
                            normalizeHashDeletion();
                            return o;
                        }
                    }
                }
            }
        }
        
        return null;
    }
    
    public Enumeration elements() {
        if (isInArrayForm()) {
            if (items != null) {
                return new ArrayEnumerator(items);
            } else {
                return EmptyEnumerator.get();
            }
        } else {
            return new CompactSetEnumerator(items, overflows);
        }
    }
    
    public Enumeration enumerateElementsSafely() {
        return elements();
    }

    public void add(Object obj) {
        if (get(obj) == null) {
            addUnconditionally(obj);
        }
    }

    public Object get(Object obj) {
        if (isInArrayForm()) {
            if (items != null) {
                for (int i = 0; i < items.length; i++) {
                    Object o = items[i];
                    
                    if (checkEqual(o, obj)) {
                        return o;
                    }
                }
            }
            
            return null;
        } else {
            return getInHashForm(obj);
        }
    }
        
    private Object getInHashForm(Object obj) {
        int hash = getHash(obj);
        int itemsLen = items.length;
        
        {   int hash1 = hash & 0x7FFFFFFF;
            Object slotObj = items[hash1 % itemsLen];
            
            if (slotObj != null && checkEqual(slotObj, obj)) {
                return slotObj;
            }
            
            if (overflows != null) {
                Object[] curList = overflows[hash1 % overflows.length];
                    
                if (curList != null) {
                    for (int i = curList.length - 1; i >= 0; i--) {
                        Object o = curList[i];
                            
                        if (checkEqual(o, obj)) {
                            return o;
                        }
                    }
                }
            }
        }

        {   int hash2 = (hash + magicHashDelta) & 0x7FFFFFFF;
            Object slotObj = items[hash2 % itemsLen];
            
            if (slotObj != null && checkEqual(slotObj, obj)) {
                return slotObj;
            }
            
            if (overflows != null) {
                Object[] curList = overflows[hash2 % overflows.length];
                    
                if (curList != null) {
                    for (int i = curList.length - 1; i >= 0; i--) {
                        Object o = curList[i];
                            
                        if (checkEqual(o, obj)) {
                            return o;
                        }
                    }
                }
            }
        }
        
        return null;
    }
    
    public String toString() {
        StringBuffer buf = new StringBuffer();
            
        for (Enumeration e = elements(); e.hasMoreElements();) {
            if (buf.length() > 0) {
                buf.append(", ");
            }
            buf.append(e.nextElement().toString());
        }
        return buf.toString();
    }
    
    public boolean equals(Object o) {
        if (o instanceof CompactSetBase) {
            CompactSetBase s = (CompactSetBase)o;
            
            if (s.size() != size()) {
                return false;
            } else {
                CompactSetBase t = this;
                
                if (s.size() < size()) {
                    t = s;
                    s = this;
                }
                
                for (Enumeration e = t.elements(); e.hasMoreElements();) {
                    if (s.get(e.nextElement()) == null) {
                        return false;
                    }
                }
                
                return true;
            }
        } else {
            return false;
        }
    }
    
    public boolean contains(Enumeration elements) {
        while (elements.hasMoreElements()) {
            if (get(elements.nextElement()) == null) {
                return false;
            }
        }
        
        return true;
    }
    
    public void removeAll(Enumeration elements) {
        while (elements.hasMoreElements()) {
            remove(elements.nextElement());
        }
    }
/*    
    public void finalize() {
        if (Globals.debug) {
            int slotsEmpty = 0;
            
            for (int i = 0; i < items.length; i++) {
                if (items[i] == null) {
                    slotsEmpty++;
                }
            }
            
            int overflows = count - (items.length - slotsEmpty);
            
            if (overflows > count/2 + 5) {
                Globals.writeLog(this, "Too many empty slots in IdentityCompactSet; #items = " + items.length + ", #contents = " + count
                    + " #empty slots = " + slotsEmpty + " (" + toString() + ")");
            }
        }
    } */
}
