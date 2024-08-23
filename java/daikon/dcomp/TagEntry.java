package daikon.dcomp;

import daikon.DynComp;
import daikon.chicory.DaikonVariableInfo;
import daikon.plumelib.util.WeakIdentityHashMap;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.StringJoiner;
import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * Union-Find datastructure for Objects without the ranking optimization. All references to the
 * Objects are weak so that they will be removed from the sets when no longer referenced.
 */
// TODO: Make TagEntry generic.
@SuppressWarnings("interning")
class TagEntry extends WeakReference<Object> {

  /** Maps each object to its entry in the Union-Find datastructure. */
  public static final WeakIdentityHashMap<Object, TagEntry> object_map =
      new WeakIdentityHashMap<Object, TagEntry>();

  // private static SimpleLog debug = new SimpleLog(false);

  /**
   * Parent in the tree that represents the set for this element. If this, this entry is the
   * representative one.
   */
  private @Nullable TagEntry parent;

  /**
   * An element in the tree that this element interacted with. If A trace-points to B, then A and B
   * have directly interacted (stronger condition than being in the same comparability set).
   *
   * <p>This is not a symmetric relation; for each interaction, at most one TagEntry's {@code
   * tracer} field is set.
   *
   * <p>Is null for the root, and also is null if DynComp.trace_file is null.
   *
   * <p>This is not necessarily an interaction with the parent, because {@link #find} sets {@link
   * #parent} but does not set {@link #tracer}.
   */
  private @Nullable TagEntry tracer;

  /**
   * The class name, method name, and line number where the interaction occurred. Is null if {@link
   * #tracer} is null.
   */
  protected String tracer_loc = "";

  /**
   * Create an entry as a separate set. Does not put {@code obj} in {@link object_map}.
   *
   * @param obj the object to put in a new set
   */
  public TagEntry(Object obj) {
    super(obj);
    this.parent = null;
    this.tracer = null;
    // System.out.printf("Make %s with parent %s%n", this, this.parent);
  }

  /**
   * Create an entry and add it to an existing set. Does not put {@code obj} in {@link object_map}.
   *
   * @param obj the object to put in a new set
   * @param parent the parent of the new set
   */
  public TagEntry(Object obj, TagEntry parent) {
    super(obj);
    this.parent = parent;
    this.tracer = parent;
    // System.out.printf("Made %s with parent p%s%n", this, this.parent);
  }

  /**
   * Creates a set that only contains obj. Puts {@code obj} in {@link object_map}.
   *
   * @param obj the object to create a new set for
   * @return a new set containing {@code obj}
   */
  public static TagEntry create(Object obj) {
    assert !object_map.containsKey(obj);
    TagEntry entry = new TagEntry(obj);
    object_map.put(obj, entry);
    return entry;
  }

  /**
   * Merge the sets that contain the specified objects. If this is the first time either of the
   * objects was seen, create an entry for it.
   */
  public static void union(Object obj1, Object obj2) {
    assert (obj1 != null) && (obj2 != null);
    // debug.log("union of '%s' and '%s'%n", obj1, obj2);

    TagEntry te1 = get_entry(obj1);
    TagEntry te2 = get_entry(obj2);
    TagEntry root1 = te1.find();
    TagEntry root2 = te2.find();

    if (root1 != root2) {
      root2.parent = root1;
      if (DynComp.trace_file != null) {
        te1.rootMe();
        te2.rootMe();
        te2.tracer = te1;
        te2.tracer_loc = generateTraceString();
      }
    }
  }

  /** Find the entry associated with obj. If an entry does not currently exist, create it. */
  public static TagEntry get_entry(Object obj) {

    assert obj != null;
    TagEntry entry = object_map.get(obj);
    if (entry == null) {
      entry = create(obj);
    }
    return entry;
  }

  /**
   * Find the TagEntry that is the representative of this set. As part of finding the
   * representative, the path from the specified entry to the representative is compressed.
   */
  public TagEntry find() {

    if (parent == null) {
      return this;
    }

    // Find the canonical representative
    TagEntry root = this;
    while (root.parent != null) {
      root = root.parent;
    }

    // Set everyone to point to the root
    TagEntry tag = this;
    while (tag.parent != null) {
      TagEntry next = tag.parent;
      tag.parent = root;
      tag = next;
    }

    return root;
  }

  /**
   * Given an Object, returns the Object that is the representative for its set. If there is no
   * entry for Object in the map, it must be in a set by itself.
   *
   * @param obj the object to find the representative for
   * @return the representative of the set containing {@code obj}
   */
  public static Object find(Object obj) {
    assert obj != null;
    TagEntry entry = object_map.get(obj);
    if (entry == null) {
      return obj;
    }
    TagEntry root = entry.find();

    // It shouldn't matter that this isn't a member of the set, only that
    // it is unique.
    // return root;

    // If the root object has been garbage collected, just return
    // a reference to the tag instead.  Since the caller has no references
    // to the root, it can't matter what we returned.
    // TODO: In that case, why not return obj?
    Object root_ref = root.get();
    if (root_ref == null) {
      root_ref = root;
    }
    return root_ref;
  }

  // ///////////////////////////////////////////////////////////////////////////
  // Tracers
  //

  /**
   * Return information about where the given object interacted with some other object in its set.
   *
   * @param obj an object in the union-find data structure
   * @return information about where the given object interacted with some other object in its set
   */
  public static String get_line_trace(Object obj) {
    return get_entry(obj).tracer_loc;
  }

  /**
   * Return the canonical member of this object's set, based on tracers. Returns null if this
   * object's interactions were not recorded.
   *
   * @param obj an object that might be in the union-find data structure
   * @return the canonical member of this object's set (based on tracers), or possibly null
   */
  public static @Nullable Object tracer_find(Object obj) {
    TagEntry entry = object_map.get(obj);
    if (entry == null) {
      return obj;
    }
    TagEntry tracer = entry.getTracer();
    if (tracer == null) {
      return null;
    }
    Object tr_ref = tracer.get();
    if (tr_ref == null) {
      tr_ref = tracer;
    }
    return tr_ref;
  }

  public static Object troot_find(Object obj) {
    TagEntry entry = object_map.get(obj);
    if (entry == null) {
      return obj;
    }
    TagEntry troot = entry.getTraceRoot();
    Object tr_ref = troot.get();
    if (tr_ref == null) {
      tr_ref = troot;
    }
    return tr_ref;
  }

  /**
   * Recursively traces from this object to the root of its tracer tree, and reverses the direction
   * of every pointer on the path, such that this object is now the root of its tracer tree.
   *
   * @param newTracer the new {@code tracer} value for this
   * @param tloc the new {@code tracer_loc} value for this
   */
  public void reroute(@Nullable TagEntry newTracer, String tloc) {
    TagEntry thisTracer = this.tracer;
    if (thisTracer != null) {
      // System.out.println("Tracer not null");
      thisTracer.reroute(this, tracer_loc);
    }
    this.tracer = newTracer;
    this.tracer_loc = tloc;
  }

  public void rootMe() {
    this.reroute(null, "");
  }

  /**
   * Returns the tracer of this node.
   *
   * @return the tracer of this node
   */
  public @Nullable TagEntry getTracer() {
    return tracer;
  }

  /**
   * Follow tracer pointers as far as possible.
   *
   * @return the last element in the path created by {@link #tracer} fields
   */
  public TagEntry getTraceRoot() {
    if (tracer == null) {
      return this;
    } else {
      return tracer.getTraceRoot();
    }
  }

  /**
   * Return a description of where an interaction occurred. Is essentially a stack trace of depth
   * {@link DynComp#trace_line_depth}.
   *
   * @return a description of where an interaction occurred
   */
  public static String generateTraceString() {
    ArrayList<StackTraceElement> stackTrace =
        new ArrayList<StackTraceElement>(
            Arrays.<StackTraceElement>asList(new Exception().getStackTrace()));
    if (stackTrace
        .get(stackTrace.size() - 1)
        .getClassName()
        .equals("daikon.dcomp.Premain$ShutdownThread")) {
      return "";
    }

    while (stackTrace.get(0).getClassName().equals("daikon.dcomp.DCRuntime")
        || stackTrace.get(0).getClassName().equals("daikon.dcomp.TagEntry")) {
      stackTrace.remove(0);
    }

    if (DynComp.trace_line_depth == 1) {
      return traceLineToString(stackTrace.get(0), false);
    } else {
      int depth = Math.min(DynComp.trace_line_depth, stackTrace.size());
      StringJoiner result = new StringJoiner(" <- ");
      for (int i = 0; i < depth; i++) {
        result.add(traceLineToString(stackTrace.get(i), true));
      }
      return result.toString();
    }
  }

  /**
   * Returns a description of the given StackTraceElement.
   *
   * @param ste a StackTraceElement to describe
   * @param abbreviate if true, omit package name
   * @return a description of the given StackTraceElement
   */
  private static String traceLineToString(StackTraceElement ste, boolean abbreviate) {
    String className = ste.getClassName();
    if (abbreviate) {
      className = className.substring(className.lastIndexOf("."));
    }
    return className + ":" + ste.getMethodName() + "(), " + ste.getLineNumber();
  }

  // ///////////////////////////////////////////////////////////////////////////
  // Debugging output
  //

  /**
   * Returns each of the sets with elements in each set on a separate line.
   *
   * @return a verbose printed representation of this
   */
  public static String dump() {

    LinkedHashMap<Object, List<Object>> sets = new LinkedHashMap<>();

    // Fill sets from object_map by placing every object in an ArrayList
    // whose key is its root.
    for (Object obj : object_map.keySet()) {
      Object rep = find(obj);
      List<Object> set = sets.computeIfAbsent(rep, __ -> new ArrayList<Object>());
      set.add(obj);
    }

    StringJoiner result = new StringJoiner(System.lineSeparator());
    result.add(String.format("%d objects in object_map", object_map.size()));
    for (Object rep : sets.keySet()) {
      List<Object> set = sets.get(rep);
      StringJoiner line = new StringJoiner(", ");
      for (Object entry : set) {
        if (entry instanceof DaikonVariableInfo) {
          line.add(String.format("%s ", ((DaikonVariableInfo) entry).getName()));
        } else {
          line.add(String.format("%s [%s]", entry.getClass(), entry));
        }
      }
      result.add(String.format("%s%n", line));
    }
    return result.toString();
  }
}
