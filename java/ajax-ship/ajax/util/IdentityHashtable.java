/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util;

/*
This code is completely stolen from java.util.Hashtable
*/

import java.util.*;
import java.io.*;

/**
This is just like java.util.Hashtable, except that it IGNORES
Object.equals and Object.hashCode and hashes only on the object identity.
This gives some performance benefits, in time as well as space.
Also, it is NOT synchronized in any way, for more performance benefits (time).
*/
class IdentityHashtableEntry implements Cloneable {
    Object key;
    Object value;
    IdentityHashtableEntry next;

    protected Object clone() {
        try {
	        IdentityHashtableEntry entry = (IdentityHashtableEntry)super.clone();
        	
        	entry.dup();
	    
    	    return entry;
        } catch (CloneNotSupportedException ex) {
            return null;
        }
    }
    
    private void dup() {
        if (next != null) {
            next = (IdentityHashtableEntry)next.clone();
        }
    }
}

public class IdentityHashtable extends Dictionary implements Cloneable, Serializable {
    /**
     * The hash table data.
     */
    private transient IdentityHashtableEntry table[];

    /**
     * The total number of entries in the hash table.
     */
    private transient int count;

    /**
     * Rehashes the table when count exceeds this threshold.
     */
    private int threshold;

    /**
     * The load factor for the hashtable.
     */
    private float loadFactor;

    /**
     * Constructs a new, empty hashtable with the specified initial 
     * capacity and the specified load factor. 
     *
     * @param      initialCapacity   the initial capacity of the hashtable.
     * @param      loadFactor        a number between 0.0 and 1.0.
     * @exception  IllegalArgumentException  if the initial capacity is less
     *               than or equal to zero, or if the load factor is less than
     *               or equal to zero.
     */
    public IdentityHashtable(int initialCapacity, float loadFactor) {
	if ((initialCapacity <= 0) || (loadFactor <= 0.0)) {
	    throw new IllegalArgumentException();
	}
	this.loadFactor = loadFactor;
	table = new IdentityHashtableEntry[initialCapacity];
	threshold = (int)(initialCapacity * loadFactor);
    }

    /**
     * Constructs a new, empty hashtable with the specified initial capacity
     * and default load factor.
     *
     * @param   initialCapacity   the initial capacity of the hashtable.
     */
    public IdentityHashtable(int initialCapacity) {
	this(initialCapacity, 1);
    }

    /**
     * Constructs a new, empty hashtable with a default capacity and load
     * factor. 
     */
    public IdentityHashtable() {
	this(5, 1);
    }

    /**
     * Returns the number of keys in this hashtable.
     *
     * @return  the number of keys in this hashtable.
     */
    public int size() {
	return count;
    }

    /**
     * Tests if this hashtable maps no keys to values.
     *
     * @return  <code>true</code> if this hashtable maps no keys to values;
     *          <code>false</code> otherwise.
     */
    public boolean isEmpty() {
	return count == 0;
    }

    /**
     * Returns an enumeration of the keys in this hashtable.
     *
     * @return  an enumeration of the keys in this hashtable.
     * @see     java.util.Enumeration
     * @see     java.util.Hashtable#elements()
     */
    public Enumeration keys() {
	return new IdentityHashtableEnumerator(table, true);
    }

    /**
     * Returns an enumeration of the values in this hashtable.
     * Use the Enumeration methods on the returned object to fetch the elements
     * sequentially.
     *
     * @return  an enumeration of the values in this hashtable.
     * @see     java.util.Enumeration
     * @see     java.util.Hashtable#keys()
     */
    public Enumeration elements() {
	return new IdentityHashtableEnumerator(table, false);
    }

    /**
     * Tests if some key maps into the specified value in this hashtable.
     * This operation is more expensive than the <code>containsKey</code>
     * method.
     *
     * @param      value   a value to search for.
     * @return     <code>true</code> if some key maps to the
     *             <code>value</code> argument in this hashtable;
     *             <code>false</code> otherwise.
     * @exception  NullPointerException  if the value is <code>null</code>.
     * @see        java.util.Hashtable#containsKey(java.lang.Object)
     */
    public boolean contains(Object value) {
	if (value == null) {
	    throw new NullPointerException();
	}

	IdentityHashtableEntry tab[] = table;
	for (int i = tab.length ; i-- > 0 ;) {
	    for (IdentityHashtableEntry e = tab[i] ; e != null ; e = e.next) {
		if (e.value.equals(value)) {
		    return true;
		}
	    }
	}
	return false;
    }
    
    /**
     * Tests if the specified object is a key in this hashtable.
     * 
     * @param   key   possible key.
     * @return  <code>true</code> if the specified object is a key in this
     *          hashtable; <code>false</code> otherwise.
     * @see     java.util.Hashtable#contains(java.lang.Object)
     */
    public boolean containsKey(Object key) {
	IdentityHashtableEntry tab[] = table;
	int hash = IdentityManager.getIdentityHashCode(key);
	int index = (hash & 0x7FFFFFFF) % tab.length;
	for (IdentityHashtableEntry e = tab[index] ; e != null ; e = e.next) {
	    if (e.key == key) {
		return true;
	    }
	}
	return false;
    }

    /**
     * Returns the value to which the specified key is mapped in this hashtable.
     *
     * @param   key   a key in the hashtable.
     * @return  the value to which the key is mapped in this hashtable;
     *          <code>null</code> if the key is not mapped to any value in
     *          this hashtable.
     * @see     java.util.Hashtable#put(java.lang.Object, java.lang.Object)
     */
    public Object get(Object key) {
	IdentityHashtableEntry tab[] = table;
	int hash = IdentityManager.getIdentityHashCode(key);
	int index = (hash & 0x7FFFFFFF) % tab.length;
	for (IdentityHashtableEntry e = tab[index] ; e != null ; e = e.next) {
	    if (e.key == key) {
		return e.value;
	    }
	}
	return null;
    }

    /**
     * Rehashes the contents of the hashtable into a hashtable with a 
     * larger capacity. This method is called automatically when the 
     * number of keys in the hashtable exceeds this hashtable's capacity 
     * and load factor. 
     */
    protected void rehash() {
	int oldCapacity = table.length;
	IdentityHashtableEntry oldTable[] = table;

	int newCapacity = oldCapacity * 2 + 1;
	IdentityHashtableEntry newTable[] = new IdentityHashtableEntry[newCapacity];

	threshold = (int)(newCapacity * loadFactor);
	table = newTable;

	//System.out.println("rehash old=" + oldCapacity + ", new=" + newCapacity + ", thresh=" + threshold + ", count=" + count);

	for (int i = oldCapacity ; i-- > 0 ;) {
	    for (IdentityHashtableEntry old = oldTable[i] ; old != null ; ) {
		IdentityHashtableEntry e = old;
		old = old.next;

		int index = (IdentityManager.getIdentityHashCode(e.key) & 0x7FFFFFFF) % newCapacity;
		e.next = newTable[index];
		newTable[index] = e;
	    }
	}
    }

    /**
     * Maps the specified <code>key</code> to the specified 
     * <code>value</code> in this hashtable. Neither the key nor the 
     * value can be <code>null</code>. 
     * <p>
     * The value can be retrieved by calling the <code>get</code> method 
     * with a key that is equal to the original key. 
     *
     * @param      key     the hashtable key.
     * @param      value   the value.
     * @return     the previous value of the specified key in this hashtable,
     *             or <code>null</code> if it did not have one.
     * @exception  NullPointerException  if the key or value is
     *               <code>null</code>.
     * @see     java.lang.Object#equals(java.lang.Object)
     * @see     java.util.Hashtable#get(java.lang.Object)
     */
    public Object put(Object key, Object value) {
	// Make sure the value is not null
	if (value == null) {
	    throw new NullPointerException();
	}

	// Makes sure the key is not already in the hashtable.
	IdentityHashtableEntry tab[] = table;
	int hash = IdentityManager.getIdentityHashCode(key);
	int index = (hash & 0x7FFFFFFF) % tab.length;
	for (IdentityHashtableEntry e = tab[index] ; e != null ; e = e.next) {
	    if (e.key == key) {
		Object old = e.value;
		e.value = value;
		return old;
	    }
	}

	if (count >= threshold) {
	    // Rehash the table if the threshold is exceeded
	    rehash();
	    return put(key, value);
	} 

	// Creates the new entry.
	IdentityHashtableEntry e = new IdentityHashtableEntry();
	e.key = key;
	e.value = value;
	e.next = tab[index];
	tab[index] = e;
	count++;
	return null;
    }

    /**
     * Removes the key (and its corresponding value) from this 
     * hashtable. This method does nothing if the key is not in the hashtable.
     *
     * @param   key   the key that needs to be removed.
     * @return  the value to which the key had been mapped in this hashtable,
     *          or <code>null</code> if the key did not have a mapping.
     */
    public Object remove(Object key) {
	IdentityHashtableEntry tab[] = table;
	int hash = IdentityManager.getIdentityHashCode(key);
	int index = (hash & 0x7FFFFFFF) % tab.length;
	for (IdentityHashtableEntry e = tab[index], prev = null ; e != null ; prev = e, e = e.next) {
	    if (e.key == key) {
		if (prev != null) {
		    prev.next = e.next;
		} else {
		    tab[index] = e.next;
		}
		count--;
		return e.value;
	    }
	}
	return null;
    }

    /**
     * Clears this hashtable so that it contains no keys. 
     *
     */
    public void clear() {
	IdentityHashtableEntry tab[] = table;
	for (int index = tab.length; --index >= 0; )
	    tab[index] = null;
	count = 0;
    }

    /**
     * Creates a shallow copy of this hashtable. The keys and values 
     * themselves are not cloned. 
     * This is a relatively expensive operation.
     *
     * @return  a clone of the hashtable.
     */
    public Object clone() {
	try { 
	    IdentityHashtable t = (IdentityHashtable)super.clone();
	    t.table = new IdentityHashtableEntry[table.length];
	    for (int i = table.length ; i-- > 0 ; ) {
		t.table[i] = (table[i] != null) 
		    ? (IdentityHashtableEntry)table[i].clone() : null;
	    }
	    return t;
	} catch (CloneNotSupportedException e) { 
	    // this shouldn't happen, since we are Cloneable
	    throw new InternalError();
	}
    }

    /**
     * Returns a rather long string representation of this hashtable.
     *
     * @return  a string representation of this hashtable.
     */
    public String toString() {
	int max = size() - 1;
	StringBuffer buf = new StringBuffer();
	Enumeration k = keys();
	Enumeration e = elements();
	buf.append("{");

	for (int i = 0; i <= max; i++) {
	    String s1 = k.nextElement().toString();
	    String s2 = e.nextElement().toString();
	    buf.append(s1 + "=" + s2);
	    if (i < max) {
		buf.append(", ");
	    }
	}
	buf.append("}");
	return buf.toString();
    }

    /**
     * WriteObject is called to save the state of the hashtable to a stream.
     * Only the keys and values are serialized since the hash values may be
     * different when the contents are restored.
     * iterate over the contents and write out the keys and values.
     */
    private void writeObject(java.io.ObjectOutputStream s)
        throws IOException
    {
	// Write out the length, threshold, loadfactor
	s.defaultWriteObject();

	// Write out length, count of elements and then the key/value objects
	s.writeInt(table.length);
	s.writeInt(count);
	for (int index = table.length-1; index >= 0; index--) {
	    IdentityHashtableEntry entry = table[index];

	    while (entry != null) {
		s.writeObject(entry.key);
		s.writeObject(entry.value);
		entry = entry.next;
	    }
	}
    }

    /**
     * readObject is called to restore the state of the hashtable from
     * a stream.  Only the keys and values are serialized since the
     * hash values may be different when the contents are restored.
     * Read count elements and insert into the hashtable. 
     */
    private void readObject(java.io.ObjectInputStream s)
         throws IOException, ClassNotFoundException
    {
	// Read in the length, threshold, and loadfactor
	s.defaultReadObject();

	// Read the original length of the array and number of elements
	int origlength = s.readInt();
	int elements = s.readInt();

	// Compute new size with a bit of room 5% to grow but
	// No larger than the original size.  Make the length
	// odd if it's large enough, this helps distribute the entries.
	// Guard against the length ending up zero, that's not valid.
	int length = (int)(elements * loadFactor) + (elements / 20) + 3;
	if (length > elements && (length & 1) == 0)
	    length--;
	if (origlength > 0 && length > origlength)
	    length = origlength;

	table = new IdentityHashtableEntry[length];
	count = 0;

	// Read the number of elements and then all the key/value objects
	for (; elements > 0; elements--) {
	    Object key = s.readObject();
	    Object value = s.readObject();
	    put(key, value);
	}
    }
}

/**
 * A hashtable enumerator class.  This class should remain opaque 
 * to the client. It will use the Enumeration interface. 
 */
class IdentityHashtableEnumerator implements Enumeration {
    boolean keys;
    int index;
    IdentityHashtableEntry table[];
    IdentityHashtableEntry entry;

    IdentityHashtableEnumerator(IdentityHashtableEntry table[], boolean keys) {
	this.table = table;
	this.keys = keys;
	this.index = table.length;
    }
	
    public boolean hasMoreElements() {
	if (entry != null) {
	    return true;
	}
	while (index-- > 0) {
	    if ((entry = table[index]) != null) {
		return true;
	    }
	}
	return false;
    }

    public Object nextElement() {
	if (entry == null) {
	    while ((index-- > 0) && ((entry = table[index]) == null)) {
	    }
	}
	if (entry != null) {
	    IdentityHashtableEntry e = entry;
	    entry = e.next;
	    return keys ? e.key : e.value;
	}
	throw new NoSuchElementException("IdentityHashtableEnumerator");
    }
}
