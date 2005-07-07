// In Java 1.4, this is called "IdentityHashMap"

/*
 * @(#)HashMap.java	1.29 99/04/22
 *
 * Copyright 1997-1999 by Sun Microsystems, Inc.,
 * 901 San Antonio Road, Palo Alto, California, 94303, U.S.A.
 * All rights reserved.
 *
 * This software is the confidential and proprietary information
 * of Sun Microsystems, Inc. ("Confidential Information").  You
 * shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement
 * you entered into with Sun.
 */

package utilMDE;
import java.io.*;
import java.util.*;

/**
 * This is a modified version of HashMap from JDK 1.2.2.
 * The modification makes key comparisons with ==, not with equals().
 * This can improve performance, for instance when using interned strings
 * as keys, but should be used with care.  For instance, it generally
 * should not be used when the keys are Integer objects.
 * I could also use a version of HashMap that takes a Hasher argument,
 * but for this special case, this code is faster.<p>
 *
 * Original comment for HashMap follows:<p>
 *
 * Hash table based implementation of the <tt>Map</tt> interface.  This
 * implementation provides all of the optional map operations, and permits
 * <tt>null</tt> values and the <tt>null</tt> key.  (The <tt>HashMap</tt>
 * class is roughly equivalent to <tt>Hashtable</tt>, except that it is
 * unsynchronized and permits nulls.)  This class makes no guarantees as to
 * the order of the map; in particular, it does not guarantee that the order
 * will remain constant over time.<p>
 *
 * This implementation provides constant-time performance for the basic
 * operations (<tt>get</tt> and <tt>put</tt>), assuming the hash function
 * disperses the elements properly among the buckets.  Iteration over
 * collection views requires time proportional to the "capacity" of the
 * <tt>HashMap</tt> instance (the number of buckets) plus its size (the number
 * of key-value mappings).  Thus, it's very important not to set the intial
 * capacity too high (or the load factor too low) if iteration performance is
 * important.<p>
 *
 * An instance of <tt>HashMap</tt> has two parameters that affect its
 * performance: <i>initial capacity</i> and <i>load factor</i>.  The
 * <i>capacity</i> is the number of buckets in the hash table, and the initial
 * capacity is simply the capacity at the time the hash table is created.  The
 * <i>load factor</i> is a measure of how full the hash table is allowed to
 * get before its capacity is automatically increased.  When the number of
 * entries in the hash table exceeds the product of the load factor and the
 * current capacity, the capacity is roughly doubled by calling the
 * <tt>rehash</tt> method.<p>
 *
 * As a general rule, te default load factor (.75) offers a good tradeoff
 * between time and space costs.  Higher values decrease the space overhead
 * but increase the lookup cost (reflected in most of the operations of the
 * <tt>HashMap</tt> class, including <tt>get</tt> and <tt>put</tt>).  The
 * expected number of entries in the map and its load factor should be taken
 * into account when setting its initial capacity, so as to minimize the
 * number of <tt>rehash</tt> operations.  If the initial capacity is greater
 * than the maximum number of entries divided by the load factor, no
 * <tt>rehash</tt> operations will ever occur.<p>
 *
 * If many mappings are to be stored in a <tt>HashMap</tt> instance, creating
 * it with a sufficiently large capacity will allow the mappings to be stored
 * more efficiently than letting it perform automatic rehashing as needed to
 * grow the table.<p>
 *
 * <b>Note that this implementation is not synchronized.</b> If multiple
 * threads access this map concurrently, and at least one of the threads
 * modifies the map structurally, it <i>must</i> be synchronized externally.
 * (A structural modification is any operation that adds or deletes one or
 * more mappings; merely changing the value associated with a key that an
 * instance already contains is not a structural modification.)  This is
 * typically accomplished by synchronizing on some object that naturally
 * encapsulates the map.  If no such object exists, the map should be
 * "wrapped" using the <tt>Collections.synchronizedMap</tt> method.  This is
 * best done at creation time, to prevent accidental unsynchronized access to
 * the map: <pre> Map m = Collections.synchronizedMap(new HashMap(...));
 * </pre><p>
 *
 * The iterators returned by all of this class's "collection view methods" are
 * <i>fail-fast</i>: if the map is structurally modified at any time after the
 * iterator is created, in any way except through the iterator's own
 * <tt>remove</tt> or <tt>add</tt> methods, the iterator will throw a
 * <tt>ConcurrentModificationException</tt>.  Thus, in the face of concurrent
 * modification, the iterator fails quickly and cleanly, rather than risking
 * arbitrary, non-deterministic behavior at an undetermined time in the
 * future.
 *
 * @author  Josh Bloch
 * @author  Arthur van Hoff
 * @version 1.29, 04/22/99
 * @see     Object#hashCode()
 * @see     Collection
 * @see	    Map
 * @see	    TreeMap
 * @see	    Hashtable
 * @deprecated  As of JDK 1.4, replaced by IdentityHashMap
 */

@Deprecated
public final class EqHashMap extends AbstractMap implements Map, Cloneable,
					 java.io.Serializable {
    /**
     * The hash table data.
     */
    private transient Entry table[];

    /**
     * The total number of mappings in the hash table.
     */
    private transient int count;

    /**
     * The table is rehashed when its size exceeds this threshold.  (The
     * value of this field is (int)(capacity * loadFactor).)
     *
     * @serial
     */
    private int threshold;

    /**
     * The load factor for the hashtable.
     *
     * @serial
     */
    private float loadFactor;

    /**
     * The number of times this HashMap has been structurally modified
     * Structural modifications are those that change the number of mappings in
     * the HashMap or otherwise modify its internal structure (e.g.,
     * rehash).  This field is used to make iterators on Collection-views of
     * the HashMap fail-fast.  (See ConcurrentModificationException).
     */
    private transient int modCount = 0;

    /**
     * Constructs a new, empty map with the specified initial
     * capacity and the specified load factor.
     *
     * @param      initialCapacity   the initial capacity of the HashMap.
     * @param      loadFactor        the load factor of the HashMap
     * @throws     IllegalArgumentException  if the initial capacity is less
     *               than zero, or if the load factor is nonpositive.
     */
    public EqHashMap(int initialCapacity, float loadFactor) {
	if (initialCapacity < 0)
	    throw new IllegalArgumentException("Illegal Initial Capacity: "+
                                               initialCapacity);
        if (loadFactor <= 0)
            throw new IllegalArgumentException("Illegal Load factor: "+
                                               loadFactor);
        if (initialCapacity==0)
            initialCapacity = 1;
	this.loadFactor = loadFactor;
	table = new Entry[initialCapacity];
	threshold = (int)(initialCapacity * loadFactor);
    }

    /**
     * Constructs a new, empty map with the specified initial capacity
     * and default load factor, which is <tt>0.75</tt>.
     *
     * @param   initialCapacity   the initial capacity of the HashMap.
     * @throws    IllegalArgumentException if the initial capacity is less
     *              than zero.
     */
    public EqHashMap(int initialCapacity) {
	this(initialCapacity, 0.75f);
    }

    /**
     * Constructs a new, empty map with a default capacity and load
     * factor, which is <tt>0.75</tt>.
     */
    public EqHashMap() {
	this(101, 0.75f);
    }

    /**
     * Constructs a new map with the same mappings as the given map.  The
     * map is created with a capacity of twice the number of mappings in
     * the given map or 11 (whichever is greater), and a default load factor,
     * which is <tt>0.75</tt>.
     */
    public EqHashMap(Map t) {
	this(Math.max(2*t.size(), 11), 0.75f);
	putAll(t);
    }

    /**
     * Returns the number of key-value mappings in this map.
     *
     * @return the number of key-value mappings in this map.
     */
    public int size() {
	return count;
    }

    /**
     * Returns <tt>true</tt> if this map contains no key-value mappings.
     *
     * @return <tt>true</tt> if this map contains no key-value mappings.
     */
    public boolean isEmpty() {
	return count == 0;
    }

    /**
     * Returns <tt>true</tt> if this map maps one or more keys to the
     * specified value.
     *
     * @param value value whose presence in this map is to be tested.
     * @return <tt>true</tt> if this map maps one or more keys to the
     *         specified value.
     */
    public boolean containsValue(Object value) {
	Entry tab[] = table;

	if (value==null) {
	    for (int i = tab.length ; i-- > 0 ; )
		for (Entry e = tab[i] ; e != null ; e = e.next)
		    if (e.value==null)
			return true;
	} else {
	    for (int i = tab.length ; i-- > 0 ; )
		for (Entry e = tab[i] ; e != null ; e = e.next)
		    if (value.equals(e.value))
			return true;
	}

	return false;
    }

    /**
     * Returns <tt>true</tt> if this map contains a mapping for the specified
     * key.
     *
     * @return <tt>true</tt> if this map contains a mapping for the specified
     * key.
     * @param key key whose presence in this Map is to be tested.
     */
    public boolean containsKey(Object key) {
	Entry tab[] = table;
	int index = (key == null) ? 0 : (key.hashCode() & 0x7FFFFFFF) % tab.length;
	for (Entry e = tab[index]; e != null; e = e.next)
	  if (key==e.key)
	    return true;

	return false;
    }

    /**
     * Returns the value to which this map maps the specified key.  Returns
     * <tt>null</tt> if the map contains no mapping for this key.  A return
     * value of <tt>null</tt> does not <i>necessarily</i> indicate that the
     * map contains no mapping for the key; it's also possible that the map
     * explicitly maps the key to <tt>null</tt>.  The <tt>containsKey</tt>
     * operation may be used to distinguish these two cases.
     *
     * @return the value to which this map maps the specified key.
     * @param key key whose associated value is to be returned.
     */
    public Object get(Object key) {
	Entry tab[] = table;

	int index = (key == null) ? 0 : (key.hashCode() & 0x7FFFFFFF) % tab.length;
	for (Entry e = tab[index]; e != null; e = e.next)
	  if (key==e.key)
	    return e.value;

	return null;
    }

    /**
     * Rehashes the contents of this map into a new <tt>HashMap</tt> instance
     * with a larger capacity. This method is called automatically when the
     * number of keys in this map exceeds its capacity and load factor.
     */
    private void rehash() {
	int oldCapacity = table.length;
	Entry oldMap[] = table;

	int newCapacity = oldCapacity * 2 + 1;
	Entry newMap[] = new Entry[newCapacity];

	modCount++;
	threshold = (int)(newCapacity * loadFactor);
	table = newMap;

	for (int i = oldCapacity ; i-- > 0 ; ) {
	    for (Entry old = oldMap[i] ; old != null ; ) {
		Entry e = old;
		old = old.next;

		int index = (e.hash & 0x7FFFFFFF) % newCapacity;
		e.next = newMap[index];
		newMap[index] = e;
	    }
	}
    }

    /**
     * Associates the specified value with the specified key in this map.
     * If the map previously contained a mapping for this key, the old
     * value is replaced.
     *
     * @param key key with which the specified value is to be associated.
     * @param value value to be associated with the specified key.
     * @return previous value associated with specified key, or <tt>null</tt>
     *	       if there was no mapping for key.  A <tt>null</tt> return can
     *	       also indicate that the HashMap previously associated
     *	       <tt>null</tt> with the specified key.
     */
    public Object put(Object key, Object value) {
	// Makes sure the key is not already in the HashMap.
	Entry tab[] = table;
        int hash = 0;
        int index = 0;

	index = (key == null) ? 0 : (key.hashCode() & 0x7FFFFFFF) % tab.length;
	for (Entry e = tab[index] ; e != null ; e = e.next) {
	    if (key == e.key) {
		Object old = e.value;
		e.value = value;
		return old;
	    }
	}

	modCount++;
	if (count >= threshold) {
	    // Rehash the table if the threshold is exceeded
	    rehash();

            tab = table;
            index = (hash & 0x7FFFFFFF) % tab.length;
	}

	// Creates the new entry.
	Entry e = new Entry(hash, key, value, tab[index]);
	tab[index] = e;
	count++;
	return null;
    }

    /**
     * Removes the mapping for this key from this map if present.
     *
     * @param key key whose mapping is to be removed from the map.
     * @return previous value associated with specified key, or <tt>null</tt>
     *	       if there was no mapping for key.  A <tt>null</tt> return can
     *	       also indicate that the map previously associated <tt>null</tt>
     *	       with the specified key.
     */
    public Object remove(Object key) {
	Entry tab[] = table;

	int index = (key == null) ? 0 : (key.hashCode() & 0x7FFFFFFF) % tab.length;

	for (Entry e = tab[index], prev = null; e != null;
	     prev = e, e = e.next) {
	    if (key==e.key) {
		modCount++;
		if (prev != null)
		    prev.next = e.next;
		else
		    tab[index] = e.next;

		count--;
		Object oldValue = e.value;
		e.value = null;
		return oldValue;
	    }
	}

	return null;
    }

    /**
     * Copies all of the mappings from the specified map to this one.
     *
     * These mappings replace any mappings that this map had for any of the
     * keys currently in the specified Map.
     *
     * @param t Mappings to be stored in this map.
     */
    public void putAll(Map t) {
	Iterator i = t.entrySet().iterator();
	while (i.hasNext()) {
	    Map.Entry e = (Map.Entry) i.next();
	    put(e.getKey(), e.getValue());
	}
    }

    /**
     * Removes all mappings from this map.
     */
    public void clear() {
	Entry tab[] = table;
	modCount++;
	for (int index = tab.length; --index >= 0; )
	    tab[index] = null;
	count = 0;
    }

    /**
     * Returns a shallow copy of this <tt>HashMap</tt> instance: the keys and
     * values themselves are not cloned.
     *
     * @return a shallow copy of this map.
     */
    public Object clone() {
	try {
	    EqHashMap t = (EqHashMap)super.clone();
	    t.table = new Entry[table.length];
	    for (int i = table.length ; i-- > 0 ; ) {
		t.table[i] = (table[i] != null)
		    ? (Entry)table[i].clone() : null;
	    }
	    t.keySet = null;
	    t.entrySet = null;
            t.values = null;
	    t.modCount = 0;
	    return t;
	} catch (CloneNotSupportedException e) {
	    // this shouldn't happen, since we are Cloneable
	    throw new InternalError();
	}
    }

    // Views

    private transient Set keySet = null;
    private transient Set entrySet = null;
    private transient Collection values = null;

    /**
     * Returns a set view of the keys contained in this map.  The set is
     * backed by the map, so changes to the map are reflected in the set, and
     * vice-versa.  The set supports element removal, which removes the
     * corresponding mapping from this map, via the <tt>Iterator.remove</tt>,
     * <tt>Set.remove</tt>, <tt>removeAll</tt>, <tt>retainAll</tt>, and
     * <tt>clear</tt> operations.  It does not support the <tt>add</tt> or
     * <tt>addAll</tt> operations.
     *
     * @return a set view of the keys contained in this map.
     */
    public Set keySet() {
	if (keySet == null) {
	    keySet = new AbstractSet() {
		public Iterator iterator() {
		    return new HashIterator(KEYS);
		}
		public int size() {
		    return count;
		}
                public boolean contains(Object o) {
                    return containsKey(o);
                }
		public boolean remove(Object o) {
		    return EqHashMap.this.remove(o) != null;
		}
		public void clear() {
		    EqHashMap.this.clear();
		}
	    };
	}
	return keySet;
    }

    /**
     * Returns a collection view of the values contained in this map.  The
     * collection is backed by the map, so changes to the map are reflected in
     * the collection, and vice-versa.  The collection supports element
     * removal, which removes the corresponding mapping from this map, via the
     * <tt>Iterator.remove</tt>, <tt>Collection.remove</tt>,
     * <tt>removeAll</tt>, <tt>retainAll</tt>, and <tt>clear</tt> operations.
     * It does not support the <tt>add</tt> or <tt>addAll</tt> operations.
     *
     * @return a collection view of the values contained in this map.
     */
    public Collection values() {
	if (values==null) {
	    values = new AbstractCollection() {
                public Iterator iterator() {
                    return new HashIterator(VALUES);
                }
                public int size() {
                    return count;
                }
                public boolean contains(Object o) {
                    return containsValue(o);
                }
                public void clear() {
                    EqHashMap.this.clear();
                }
            };
        }
	return values;
    }

    /**
     * Returns a collection view of the mappings contained in this map.  Each
     * element in the returned collection is a <tt>Map.Entry</tt>.  The
     * collection is backed by the map, so changes to the map are reflected in
     * the collection, and vice-versa.  The collection supports element
     * removal, which removes the corresponding mapping from the map, via the
     * <tt>Iterator.remove</tt>, <tt>Collection.remove</tt>,
     * <tt>removeAll</tt>, <tt>retainAll</tt>, and <tt>clear</tt> operations.
     * It does not support the <tt>add</tt> or <tt>addAll</tt> operations.
     *
     * @return a collection view of the mappings contained in this map.
     * @see java.util.Map.Entry
     */
    public Set entrySet() {
	if (entrySet==null) {
	    entrySet = new AbstractSet() {
                public Iterator iterator() {
                    return new HashIterator(ENTRIES);
                }

                public boolean contains(Object o) {
                    if (!(o instanceof Map.Entry))
                        return false;
                    Map.Entry entry = (Map.Entry)o;
                    Object key = entry.getKey();
                    Entry tab[] = table;
                    int hash = (key==null ? 0 : key.hashCode());
                    int index = (hash & 0x7FFFFFFF) % tab.length;

                    for (Entry e = tab[index]; e != null; e = e.next)
                        if (e.hash==hash && e.equals(entry))
                            return true;
                    return false;
                }

		public boolean remove(Object o) {
                    if (!(o instanceof Map.Entry))
                        return false;
                    Map.Entry entry = (Map.Entry)o;
                    Object key = entry.getKey();
                    Entry tab[] = table;
                    int hash = (key==null ? 0 : key.hashCode());
                    int index = (hash & 0x7FFFFFFF) % tab.length;

                    for (Entry e = tab[index], prev = null; e != null;
                         prev = e, e = e.next) {
                        if (e.hash==hash && e.equals(entry)) {
                            modCount++;
                            if (prev != null)
                                prev.next = e.next;
                            else
                                tab[index] = e.next;

                            count--;
                            e.value = null;
                            return true;
                        }
                    }
                    return false;
                }

                public int size() {
                    return count;
                }

                public void clear() {
                    EqHashMap.this.clear();
                }
            };
        }

	return entrySet;
    }

    /**
     * HashMap collision list entry.
     */
    private static final class Entry implements Map.Entry {
	int hash;
	Object key;
	Object value;
	Entry next;

	Entry(int hash, Object key, Object value, Entry next) {
	    this.hash = hash;
	    this.key = key;
	    this.value = value;
	    this.next = next;
	}

	protected Object clone() {
	    return new Entry(hash, key, value,
			     (next==null ? null : (Entry)next.clone()));
	}

	// Map.Entry Ops

	public Object getKey() {
	    return key;
	}

	public Object getValue() {
	    return value;
	}

	public Object setValue(Object value) {
	    Object oldValue = this.value;
	    this.value = value;
	    return oldValue;
	}

	public boolean equals(Object o) {
	    if (!(o instanceof Map.Entry))
		return false;
	    Map.Entry e = (Map.Entry)o;

	    return (key==e.getKey()) &&
	       (value==null ? e.getValue()==null : value.equals(e.getValue()));
	}

	public int hashCode() {
	    return hash ^ (value==null ? 0 : value.hashCode());
	}

	public String toString() {
	    return key+"="+value;
	}
    }

    // Types of Iterators
    private static final int KEYS = 0;
    private static final int VALUES = 1;
    private static final int ENTRIES = 2;

    private final class HashIterator implements Iterator {
	Entry[] table = EqHashMap.this.table;
	int index = table.length;
	Entry entry = null;
	Entry lastReturned = null;
	int type;

	/**
	 * The modCount value that the iterator believes that the backing
	 * List should have.  If this expectation is violated, the iterator
	 * has detected concurrent modification.
	 */
	private int expectedModCount = modCount;

	HashIterator(int type) {
	    this.type = type;
	}

	public boolean hasNext() {
	    while (entry==null && index>0)
		entry = table[--index];

	    return entry != null;
	}

	public Object next() {
	    if (modCount != expectedModCount)
		throw new ConcurrentModificationException();

	    while (entry==null && index>0)
		entry = table[--index];

	    if (entry != null) {
		Entry e = lastReturned = entry;
		entry = e.next;
		return type == KEYS ? e.key : (type == VALUES ? e.value : e);
	    }
	    throw new NoSuchElementException();
	}

	public void remove() {
	    if (lastReturned == null)
		throw new IllegalStateException();
	    if (modCount != expectedModCount)
		throw new ConcurrentModificationException();

	    Entry[] tab = EqHashMap.this.table;
	    int index = (lastReturned.hash & 0x7FFFFFFF) % tab.length;

	    for (Entry e = tab[index], prev = null; e != null;
		 prev = e, e = e.next) {
		if (e == lastReturned) {
		    modCount++;
		    expectedModCount++;
		    if (prev == null)
			tab[index] = e.next;
		    else
			prev.next = e.next;
		    count--;
		    lastReturned = null;
		    return;
		}
	    }
	    throw new ConcurrentModificationException();
	}
    }

    /**
     * Save the state of the <tt>HashMap</tt> instance to a stream (i.e.,
     * serialize it).
     *
     * @serialData The <i>capacity</i> of the HashMap (the length of the
     *		   bucket array) is emitted (int), followed  by the
     *		   <i>size</i> of the HashMap (the number of key-value
     *		   mappings), followed by the key (Object) and value (Object)
     *		   for each key-value mapping represented by the HashMap
     * The key-value mappings are emitted in no particular order.
     */
    private void writeObject(java.io.ObjectOutputStream s)
        throws IOException
    {
	// Write out the threshold, loadfactor, and any hidden stuff
	s.defaultWriteObject();

	// Write out number of buckets
	s.writeInt(table.length);

	// Write out size (number of Mappings)
	s.writeInt(count);

        // Write out keys and values (alternating)
	for (int index = table.length-1; index >= 0; index--) {
	    Entry entry = table[index];

	    while (entry != null) {
		s.writeObject(entry.key);
		s.writeObject(entry.value);
		entry = entry.next;
	    }
	}
    }

    private static final long serialVersionUID = 362498820763181265L;

    /**
     * Reconstitute the <tt>HashMap</tt> instance from a stream (i.e.,
     * deserialize it).
     */
    private void readObject(java.io.ObjectInputStream s)
         throws IOException, ClassNotFoundException
    {
	// Read in the threshold, loadfactor, and any hidden stuff
	s.defaultReadObject();

	// Read in number of buckets and allocate the bucket array;
	int numBuckets = s.readInt();
	table = new Entry[numBuckets];

	// Read in size (number of Mappings)
	int size = s.readInt();

	// Read the keys and values, and put the mappings in the HashMap
	for (int i=0; i<size; i++) {
	    Object key = s.readObject();
	    Object value = s.readObject();
	    put(key, value);
	}
    }

    int capacity() {
        return table.length;
    }

    float loadFactor() {
        return loadFactor;
    }
}
