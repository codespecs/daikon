package daikon.dcomp;

import utilMDE.*;
import daikon.chicory.DaikonVariableInfo;

import java.lang.ref.*;
import java.util.*;

/**
 * Union/Find datastructure for Objects without the ranking optimization.
 * All refernences to the Objects are weak so that they will be removed from
 * the sets when no longer referenced.
 */
class TagEntry extends WeakReference<Object> {

  /** Maps each object to its entry in the Union-Find datastructure **/
  public static final WeakIdentityHashMap<Object,TagEntry> object_map
    = new WeakIdentityHashMap<Object,TagEntry>();

  private static SimpleLog debug = new SimpleLog(false);

  /**
   * Parent in the tree that represents the set for this element.  If null,
   * this entry is the representive one
   */
  private TagEntry parent;

  /** Create an entry as a separate set **/
  public TagEntry (Object obj) {
    this (obj, null);
  }

  /** Create an entry and add it to an existing set **/
  public TagEntry (Object obj, TagEntry parent) {
    super (obj);
    this.parent = parent;
  }

  /**
   * Creates a set that only contains obj
   */
  public static TagEntry create (Object obj) {
    assert !object_map.containsKey (obj);
    TagEntry entry = new TagEntry(obj);
    object_map.put (obj, entry);
    return (entry);
  }

  /**
   * Merge the two sets identified by their roots
   */
  public static void union (TagEntry root1, TagEntry root2) {
    assert root1.parent == null;
    assert root2.parent == null;
    if (root1 != root2)
      root2.parent = root1;
  }

  /**
   * Merge the sets that contain the specified objects.  If this is the
   * first time either of the objects was seen, create an entry for it.
   */
  public static void union (Object obj1, Object obj2) {
    assert (obj1 != null) && (obj2 != null);
    debug.log ("union of '%s' and '%s'%n", obj1, obj2);
    union (get_entry (obj1).find(), get_entry (obj2).find());
  }

  /**
   * Find the entry associated with obj.  If an entry does not currently
   * exist, create it
   */
  public static TagEntry get_entry (Object obj) {

    assert obj != null;
    TagEntry entry = object_map.get(obj);
    if (entry == null)
      entry = create (obj);
    return (entry);
  }

  /**
   * Find the TagEntry that is the representative of this set.
   * As part of finding the representative, the path from the specified entry
   * to the representative is compressed.
   */
  public TagEntry find() {

    if (parent == null)
      return this;

    // Find the tag at the top of the list
    TagEntry tag = this;
    while (tag.parent != null)
      tag = tag.parent;
    TagEntry top = tag;

    // Set everyone to point to the top
    tag = this;
    while (tag.parent != null) {
      TagEntry next = tag.parent;
      tag.parent = top;
      tag = next;
    }

    return top;
  }

  /**
   * Given an Object returns the Object that is the representative for
   * its set.  If there is no entry for Object in the map, it must be
   * in a set by itself.
   */
  public static Object find (Object obj) {
    assert obj != null;
    TagEntry entry = object_map.get (obj);
    if (entry == null)
      return obj;
    TagEntry root = entry.find();
    return root.get();
  }

  /**
   * Returns each of the sets with elements in each set on a separate
   * line.
   */
  public static String dump() {

    LinkedHashMap<Object, List<Object>> sets
      = new LinkedHashMap<Object,List<Object>>();

    for (Object obj : object_map.keySet()) {
      Object rep = find (obj);
      List<Object> set = sets.get (rep);
      if (set == null) {
        set = new ArrayList<Object>();
        sets.put (rep, set);
      }
      set.add (obj);
    }

    String out = String.format ("%d objects in object_map%n",
                                object_map.size());
    for (Object rep : sets.keySet()) {
      List<Object> set = sets.get (rep);
      String line = "";
      for (Object entry : set) {
        if (line != "")
          line += ", ";
        if (entry instanceof DaikonVariableInfo)
          line += String.format ("%s ", ((DaikonVariableInfo)entry).getName());
        else
          line += String.format ("%s [%s]", entry.getClass(), entry);
      }
      out += String.format ("%s%n", line);
    }
    return (out);
  }

}
