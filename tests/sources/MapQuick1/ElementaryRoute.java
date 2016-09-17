package MapQuick1;

import MapQuick.*;
import java.text.DecimalFormat;
import junit.framework.Assert;

public class ElementaryRoute extends Route
{
  // Constructors

  public ElementaryRoute(GeoSegment gs)
  {
    super(gs);
  }

  private ElementaryRoute(ElementaryRoute r, GeoSegment gs)
  {
    super(r, gs);
    Assert.assertTrue(r.end().equals(gs.p1()));
    Assert.assertTrue(r.name().equals(gs.name()));
  }

  public ElementaryRoute addSegment(GeoSegment gs)
  {
    return new ElementaryRoute(this, gs);
  }

  // Observers

  public String name()
  {
    return first.name();
  }

  // Specified by Route superclass
  public ElementaryRoute[] elementaryRoutes()
  {
    return new ElementaryRoute[] { this };
  }

  // Specified by Object superclass
  public String toString()
  {
    return "{Elem{" + super.toString()+ "}}";
  }

  /** Returns directions for following a route.
   * @requires 0 <= heading < 360
   * @return a one-line newline-terminated human-readable directions string.
   * The directions string is of the form
   *
   * <pre>Turn right onto Baker Street and go 1.2 miles.</pre>
   *
   * Here, "Turn right" rotates the traveler from the given heading
   * to this.startHeading, "Baker Street" is this.name, and 1.2 miles
   * is this.length. The length is printed with tenth-of-a-mile precision.
   *
   * Let the turn angle be a. The turn should be annotated as
   * <pre>
   * Continue             if a &lt; 10
   * Turn slight right    if 10 &lt;= a &lt; 60
   * Turn right           if 60 &lt;= a &lt; 120
   * Turn sharp right     if 120 &lt;= a &lt; 179
   * U-turn               if 179 &lt;= a </pre>
   *
   * and likewise for left turns.
   */
  public String directions(double heading)
  {
    Assert.assertTrue((0 <= heading) && (heading < 360));

    return
      computeTurn(heading, startHeading()) +
      " onto " + name() +
      " and go " + roundToTenth(length()) +
      " miles.\n";
  }

  private static String computeTurn(double from, double to)
  {
    Assert.assertTrue((0 <= from) && (from < 360));
    Assert.assertTrue((0 <= to) && (to < 360));

    // compute the severity of a left turn
    String turn = "left";
    double amt = from - to;
    if (amt < 0) {
      amt += 360;
    }
    Assert.assertTrue((0 <= amt) && (amt < 360));

    // if too severe, it's really a right turn
    if (amt > 180) {
      amt = 360 - amt;
      turn = "right";
    }
    Assert.assertTrue((0 <= amt) && (amt <= 180));

    if (amt < 10) {
      return "Continue";
    } else if (amt < 60) {
      return "Turn slight " + turn;
    } else if (amt < 120) {
      return "Turn " + turn;
    } else if (amt < 179) {
      return "Turn sharp " + turn;
    } else {
      return "U-turn";
    }
  }

  private static final DecimalFormat _formatter = new DecimalFormat("#0.0");
  private static String roundToTenth(double n)
  {
    String result = _formatter.format(n);
    Assert.assertTrue(Math.abs(Double.parseDouble(result) - n) <= 0.5);
    return result;
  }

} // ElementaryRoute
