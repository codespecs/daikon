package daikon.mints;

import daikon.inv.Invariant;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * A sequence of invariants contained in some Java method.
 *
 * @author Huascar Sanchez
 */
class InvariantSequence implements Iterable<KnownInvariant> {

  private final Source                source;
  private final List<KnownInvariant> invariants;


  /**
   * Construct an Invariant Sequence located at some source method.
   * @param source the source of these invariants.
   */
  InvariantSequence(Source source) {

    this.source     = Objects.requireNonNull(source);
    this.invariants = new LinkedList<>();

  }

  /**
   * Adds a non empty list of invariants to this segment.
   *
   * @param x the list of invariants.
   */
  void add(List<Invariant> x) {
    if(x.isEmpty()) return;

    final List<KnownInvariant> translated = translate(x);

    for(KnownInvariant each : translated){
      if(Objects.isNull(each)) continue;

      invariants.add(each);
    }
  }

  private static List<KnownInvariant> translate(List<Invariant> x){
    return x.stream()
      .map(KnownInvariant::from)
      .collect(Collectors.toList());

  }

  /**
   * @return the content of this segment.
   */
  List<KnownInvariant> content() {
    return invariants;
  }

  @Override public Iterator<KnownInvariant> iterator() {
    return content().iterator();
  }

  /**
   * @return true if the segment is empty; false otherwise.
   */
  boolean isEmpty() {
    return content().isEmpty();
  }

  /**
   * @return a normalized view of this segment's invariants.
   */
  List<String> normalized() {
    final List<KnownInvariant> content = content();
    final List<String> result = new LinkedList<>();

    for (KnownInvariant each : content) {
      result.add(each.typeOf());
    }

    return result;
  }

  /**
   * @return the source or location from where these
   * invariants were extracted.
   */
  Source source() {
    return source;
  }

  /**
   * @return the size of this segment.
   */
  int size() {
    return this.invariants.size();
  }

  @Override public String toString() {
    final Source src = source();

    return src.className() + "#" + src.methodName()
      + "(" + size() + ((src.isEntry() ? " entry": " exit"))
      + " invariants)";
  }
}
