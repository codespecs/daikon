package MapQuick;

import java.lang.Comparable;

/**
 * A StreetClassification describes a street category.  It is what is
 * often called an <i>enumeration</i> type.  There are a handful of
 * values the type may hold, and the set of options is fixed at
 * compile time.
 * 
 * <p> Example uses: <pre>
 * StreetClassification myType = StreetClassification.LOCAL_ROAD;
 * if (myType == StreetClassification.UNKNOWN) {
 *   // ...
 * } </pre>
 * 
 * <p> Notice that you may reference the constant values as you would
 * with any other static variable, e.g. ClassName.STATIC_FIELD_NAME.
 * Also, you may use the <code>==</code> operator to check for
 * equality, since these objects are flyweights.
 *
 * <p> The ordering given by the <code>compareTo</code> method of this
 * class is consistent with equals, and gives the following ordering:
 * PRIM_HWY, SEC_HWY, LOCAL_ROAD, UNKNOWN.
 */
public final class StreetClassification
  implements Comparable
{
  /**
   * Classification indicating a primary highway.  Primary highways
   * include interstate highways and some toll highways; these
   * highways are accesed by way of ramps and have multiple lanes of
   * traffic.
   */
  public static final StreetClassification PRIM_HWY = new StreetClassification("Primary Highway", "PRIM_HWY", 0);

  /**
   * Classification indicating a secondary highway.  Secondary
   * highways include state highways and some county highways.
   */
  public static final StreetClassification SEC_HWY = new StreetClassification("Secondary Highway", "SEC_HWY", 1);

  /**
   * Classification indicating a local road.  Local roads are for
   * local traffic.  Scenic park roads and unpaved roads are also
   * included in this category.
   */
  public static final StreetClassification LOCAL_ROAD = new StreetClassification("Local Road", "LOCAL_ROAD", 2);

  /**
   * Classification indicating an unknown type of street.  This
   * classificiation is given to streets that do not fall within one
   * of the other three categories or to streets for which not enough
   * information is known to classify them.
   */
  public static final StreetClassification UNKNOWN = new StreetClassification("Unknown", "UNKNOWN", 99);


  /**
   * @requires type is one of {"PRIM_HWY", "SEC_HWY", "LOCAL_ROAD", "UNKNOWN"}.
   * @returns result such that result.unparse().equals(type)
   */
  public static StreetClassification parse(String type)
  {
    if (PRIM_HWY.unparse().equals(type)) return PRIM_HWY;
    if (SEC_HWY.unparse().equals(type)) return SEC_HWY;
    if (LOCAL_ROAD.unparse().equals(type)) return LOCAL_ROAD;
    if (UNKNOWN.unparse().equals(type)) return UNKNOWN;

    throw new IllegalArgumentException("Unknown type: " + type);
  }

  /**
   * @returns string respresentation of this.  Result will be one of
   * {"PRIM_HWY", "SEC_HWY", "LOCAL_ROAD", "UNKNOWN"}.
   */
  public String unparse()
  {
    return repr;
  }

  // ==================== IMPLEMENTATION ====================

  private StreetClassification(String name, String repr, int priority)
  {
    this.name = name;
    this.repr = repr;
    this.priority = priority;
  }

  private final String name;
  private final String repr;
  private final int priority;

  /**
   * @returns a String representation of this
   */
  public String toString()
  {
    return "StreetClassification[" + name + "]";
  }

  /**
   * @requires (o != null) && (o instanceof StreetClassification)
   *
   * @return an int representing the ordering of this as compared to
   * the argument, as defined by the class overview.
   *
   * @see Comparable#compareTo(Object)
   */
  public int compareTo(Object o)
  {
    return priority - ((StreetClassification) o).priority;
  }

  //
  // We are a flyweight so we don't need to redefine equals
  //
  // public boolean equals(Objet o);
  // public int hashCode(Objet o);
  //

}
