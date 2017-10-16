package daikon.tools.runtimechecker;

import java.io.ObjectStreamException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/*>>>
import org.checkerframework.checker.interning.qual.*;
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * Represents a violation of a {@code Property}.
 *
 * @see Property
 */
public class Violation implements Serializable {

  private static final long serialVersionUID = 1L;

  // The violated property.
  private final Property property;

  // The time at which the violation happened (entry or exit
  // from method).
  private final Time time;

  /** The violated property. */
  public Property property() {
    return property;
  }

  /** The time at which the violation happened (entry or exit from method). */
  public Time time() {
    return time;
  }

  /**
   * Indicates at which program point the violation occurred: at method entry or method exit.
   *
   * <p>This class contains only two (static) objects: {@code onEntry} and {@code onExit}.
   */
  // This should be an enum
  /*@UsesObjectEquals*/
  public static class Time implements Serializable {

    private static final long serialVersionUID = 1L;

    public final String name;

    public final String xmlname;

    private Time(String name, String xmlname) {
      this.name = name;
      this.xmlname = xmlname;
    }

    /*@Pure*/
    @Override
    public int hashCode(/*>>>@GuardSatisfied Time this*/) {
      return name.hashCode();
    }

    /*@SideEffectFree*/
    @Override
    public String toString(/*>>>@GuardSatisfied Time this*/) {
      return name;
    }

    public String xmlString() {
      return xmlname;
    }

    public static final Time onEntry = new Time("violated on entry ", "<ON_ENTRY>");

    public static final Time onExit = new Time("violated on exit  ", "<ON_EXIT>");

    // See documentation for Serializable.
    private Object readResolve() throws ObjectStreamException {
      if (name.equals("violated on entry")) {
        return onEntry;
      } else {
        assert name.equals("violated on exit ") : name;
        return onExit;
      }
    }
  }

  /**
   * Creates the violation represented by {@code vioString}.
   *
   * <p>Precondition: the string is of the form:
   *
   * <p>{@code <INVINFO> property time</INVINFO>}
   *
   * <p>Where {@code property} is valid XML representation of a {@code Property}, and time is {@code
   * <ON_ENTRY>} or {@code <ON_EXIT>}.
   */
  public static Violation get(String vioString) {

    if (!(vioString.matches(".*(<INVINFO>.*</INVINFO>).*"))) {
      throw new IllegalArgumentException(vioString);
    }

    String annoString = vioString.replaceFirst(".*(<INVINFO>.*</INVINFO>).*", "$1");

    Property anno;
    try {
      anno = Property.get(annoString);
    } catch (MalformedPropertyException e) {
      throw new IllegalArgumentException(e.getMessage(), e);
    }

    Time t;
    if (vioString.matches(".*<ON_ENTRY>.*")) {
      t = Time.onEntry;
    } else if (vioString.matches(".*<ON_EXIT>.*")) {
      t = Time.onExit;
    } else {
      throw new Error("Bad time");
    }

    return get(anno, t);
  }

  // Collection of all the Violation objects created.
  // The key is the hashCode of the Violation that is the value.
  private static HashMap<Integer, Violation> violationsMap = new HashMap<Integer, Violation>();

  // [[[ TODO: ensure args are not null (otherwise hashCode,
  // equals can break). Do the same thing for Property. ]]]
  private Violation(Property anno, Time t) {
    this.property = anno;
    this.time = t;
  }

  /** Returns a violation with the given attributes. */
  public static Violation get(Property anno, Time t) {
    Violation vio = new Violation(anno, t);
    Integer key = vio.hashCode();
    if (violationsMap.containsKey(key)) {
      return violationsMap.get(key);
    } else {
      violationsMap.put(key, vio);
      return vio;
    }
  }

  /**
   * if {@code property} is an entry or exit property, returns the violation corresponding to this
   * property. If it's an object invariant property, throws an exception.
   */
  public static Violation get(Property property) {
    Time t;
    if (property.kind() == Property.Kind.enter) {
      t = Time.onEntry;
    } else if (property.kind() == Property.Kind.exit) {
      t = Time.onExit;
    } else {
      throw new IllegalArgumentException(
          "property must be ENTER or EXIT kind: " + property.toString());
    }
    return get(property, t);
  }

  /** The XML String representing this property. */
  public String xmlString() {
    return "<VIOLATION>" + property.xmlString() + time.xmlString() + "</VIOLATION>";
  }

  /** String representation. */
  /*@SideEffectFree*/
  @Override
  public String toString(/*>>>@GuardSatisfied Violation this*/) {
    return time.toString() + " : " + property.toString();
  }

  /** String representation. */
  /*@SideEffectFree*/
  public String toStringWithMethod(/*>>>@GuardSatisfied Violation this*/) {
    return time.toString() + "of " + property.method() + " : " + property.toString();
  }

  /** Two violations are equal if their properties and times are equal. */
  /*@EnsuresNonNullIf(result=true, expression="#1")*/
  /*@Pure*/
  @Override
  public boolean equals(
      /*>>>@GuardSatisfied Violation this,*/
      /*@GuardSatisfied*/ /*@Nullable*/ Object o) {
    if (o == null) {
      return false;
    }
    if (!(o instanceof Violation)) {
      return false;
    }
    Violation other = (Violation) o;
    return (this.property.equals(other.property) && this.time.equals(other.time));
  }

  /*@Pure*/
  @Override
  public int hashCode(/*>>>@GuardSatisfied Violation this*/) {
    return property.hashCode() + time.hashCode();
  }

  /**
   * Returns all violations in {@code vios} that violate properties with confidence greater than or
   * equal to {@code thresh}.
   */
  public static Violation[] viosWithConfGEQ(Violation[] vios, double thresh) {
    List<Violation> ret = new ArrayList<Violation>();
    for (int i = 0; i < vios.length; i++) {
      Violation v = vios[i];
      Property a = v.property;
      if (a.confidence >= thresh) {
        ret.add(v);
      }
    }
    return ret.toArray(new Violation[] {});
  }

  /**
   * Returns all violations in {@code vios} that violate properties with confidence less than {@code
   * thresh}.
   */
  public static Violation[] viosWithConfLT(Violation[] vios, double thresh) {
    List<Violation> ret = new ArrayList<Violation>();
    for (int i = 0; i < vios.length; i++) {
      Violation v = vios[i];
      Property a = v.property;
      if (a.confidence < thresh) {
        ret.add(v);
      }
    }
    return ret.toArray(new Violation[] {});
  }

  /** Returns all violations in {@code vios} with the given time. */
  public static Violation[] withTime(Violation[] vios, Time time) {
    List<Violation> ret = new ArrayList<Violation>();
    for (int i = 0; i < vios.length; i++) {
      Violation v = vios[i];
      if (v.time == time) {
        ret.add(v);
      }
    }
    return ret.toArray(new Violation[] {});
  }

  /** Returns all violations in {@code vios} with the given king. */
  public static Violation[] withKind(Violation[] vios, Property.Kind kind) {
    List<Violation> ret = new ArrayList<Violation>();
    for (int i = 0; i < vios.length; i++) {
      if (kind == vios[i].property().kind()) {
        ret.add(vios[i]);
      }
    }
    return ret.toArray(new Violation[] {});
  }

  /**
   * Looks for legal XML representation of violations in the given string, and returns all
   * violations that are successfully parsed.
   */
  // [[[ TODO: There's something unsatisfying about this method
  // swallowing up erroneous input silently. Fix this. ]]]
  public static Violation[] findViolations(String vioString) {

    if (vioString == null || vioString.equals("")) {
      return new Violation[] {};
    }
    Set<Violation> vios = new HashSet<Violation>();
    String[] cutUp = vioString.split("<VIOLATION>");
    for (int splits = 0; splits < cutUp.length; splits++) {
      try {
        String s = cutUp[splits];
        Violation v = Violation.get("<VIOLATION>" + s); // [[[ explain
        // this! ]]]
        vios.add(v);
      } catch (Exception e) {
        // go on to next split
      }
    }
    return vios.toArray(new Violation[] {});
  }

  public String toNiceString(String prefix, double confidenceThreshold) {
    return prefix
        + ((property.confidence > confidenceThreshold) ? "H" : " ")
        + " "
        + prefix
        + "   "
        + toString()
        + daikon.Global.lineSep;
  }

  /**
   * A human-readable String representation of a list of violations. The violations are sorted by
   * "time" (which is not the same as sorting them by time!) and violations of high-confidence
   * properties are prepended with "H".
   */
  public static String toNiceString(
      String prefix, Set<Violation> vios, double confidenceThreshold) {

    // TODO; It is bizarre that withTime requires conversion to an array.
    Violation[] vios_array = vios.toArray(new Violation[] {});
    Violation[] onEntry = Violation.withTime(vios_array, Violation.Time.onEntry);
    Violation[] onExit = Violation.withTime(vios_array, Violation.Time.onExit);

    assert onEntry.length + onExit.length == vios.size()
        : "onEntry: "
            + Arrays.asList(onEntry).toString()
            + "onExit:  "
            + Arrays.asList(onExit).toString()
            + "vios: "
            + vios;

    StringBuffer retval = new StringBuffer();
    for (int i = 0; i < onEntry.length; i++) {
      retval.append(onEntry[i].toNiceString(prefix, confidenceThreshold));
    }
    for (int i = 0; i < onExit.length; i++) {
      retval.append(onExit[i].toNiceString(prefix, confidenceThreshold));
    }
    return retval.toString();
  }

  // See documentation for Serializable.
  private Object readResolve() throws ObjectStreamException {
    return get(property, time);
  }
}
