package daikon.tools.jtb;

import java.util.*;
import java.util.regex.*;

/*>>>
import org.checkerframework.checker.interning.qual.*;
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * Utility class to parse annotations generated with the Annotate program using the {@code
 * --wrap_xml} flag.
 *
 * <p>An example of the String representation of an annotation, as output with the {@code
 * --wrap_xml} flag, is:
 *
 * <pre>{@code
 * <INVINFO>
 * <INV> this.topOfStack <= this.theArray.length-1 </INV>
 * <ENTER>
 * <DAIKON>  this.topOfStack <= size(this.theArray[])-1  </DAIKON>
 * <DAIKONCLASS>class daikon.inv.binary.twoScalar.IntLessEqual</DAIKONCLASS>
 * <METHOD>  isEmpty()  </METHOD>
 * </INVINFO>
 * }</pre>
 *
 * The above string should actually span only one line.
 *
 * <p>To be well-formed, an annotation should be enclosed in {@code <INVINFO>} tags, contain
 *
 * <pre>{@code
 * <DAIKON> and
 * <METHOD>
 * }</pre>
 *
 * tags, and exactly one of
 *
 * <pre>{@code
 * <ENTER>,
 * <EXIT>,
 * <OBJECT>, or
 * <CLASS>.
 * }</pre>
 *
 * Obviously, the tool Annotate outputs well-formed annotations, so the user shouldn't have to worry
 * too much about well-formedness.
 *
 * <p>Two annotations are equal iff their fields "daikonRep", "method" and "kind" are equal.
 *
 * <p>The factory method get(String annoString) returns an annotation that equals to the annotation
 * represented by annoString. In particular, if the same String is given twice to get(String
 * annoString), the method will return the same Annotation object.
 */
public class Annotation {

  // Maps into all the Annotation objects created.
  private static HashMap<Integer, Annotation> annotationsMap = new HashMap<Integer, Annotation>();

  /** Daikon representation (as output by Daikon's default output format). */
  private final String daikonRep;
  /** The way this annotation would be printed by Daikon. */
  public String daikonRep(/*>>>@GuardSatisfied Annotation this*/) {
    return daikonRep;
  }

  private final String method;
  /** The method that this annotation refers to. */
  public String method(/*>>>@GuardSatisfied Annotation this*/) {
    return method;
  }

  private final Kind kind;
  /** The kind of this annotation. */
  public Kind kind(/*>>>@GuardSatisfied Annotation this*/) {
    return kind;
  }

  private String invRep;
  /**
   * Representation of this annotation (the format depends on which output format was used to create
   * the annotation in Daikon; it's one of JAVA, JML, ESC or DBC).
   */
  public String invRep() {
    return invRep;
  }

  public String daikonClass;
  /** The Daikon class name that this invariant represents an instance of. */
  public String daikonClass() {
    return daikonClass;
  }

  private Annotation(
      Kind kind, String daikonRep, String method, String invRep, String daikonClass) {
    this.kind = kind;
    this.daikonRep = daikonRep;
    this.method = method;
    this.invRep = invRep;
    this.daikonClass = daikonClass;
  }

  /** Parse a String and return the annotation that it represents. */
  // [[ Note: Using an XML parser seems like too strong a hammer here,
  // and the performance of string matching is not an obvious
  // bottleneck. ]]
  public static Annotation get(String annoString) throws Annotation.MalformedAnnotationException {

    // check well-formedness
    boolean wellformed = true;
    if (!(annoString.matches(".*<INVINFO>.*</INVINFO>.*")
        && annoString.matches(".*<DAIKON>(.*)</DAIKON>.*")
        && annoString.matches(".*<METHOD>(.*)</METHOD>.*"))) {
      throw new Annotation.MalformedAnnotationException(annoString);
    }

    // figure out what kind of annotation it is
    Kind k;
    if (annoString.matches(".*<ENTER>.*")) {
      k = Kind.enter;
    } else if (annoString.matches(".*<EXIT>.*")) {
      k = Kind.exit;
    } else if (annoString.matches(".*<OBJECT>.*") || annoString.matches(".*<CLASS>.*")) {
      k = Kind.objectInvariant;
    } else {
      throw new Annotation.MalformedAnnotationException(annoString);
    }

    String theDaikonRep = annoString.replaceFirst(".*<DAIKON>(.*)</DAIKON>.*", "$1").trim();
    String theMethod = annoString.replaceFirst(".*<METHOD>(.*)</METHOD>.*", "$1").trim();
    String theInvRep = annoString.replaceFirst(".*<INV>(.*)</INV>.*", "$1").trim();
    String theDaikonClass =
        annoString.replaceFirst(".*<DAIKONCLASS>(.*)</DAIKONCLASS>.*", "$1").trim();

    Annotation anno = Annotation.get(k, theDaikonRep, theMethod, theInvRep, theDaikonClass);

    return anno;
  }

  /**
   * Thrown by method get(String annoString) if annoString doesn't represent a well-formed
   * annotation.
   */
  public static class MalformedAnnotationException extends Exception {
    static final long serialVersionUID = 20050923L;

    public MalformedAnnotationException(String s) {
      super(s);
    }
  }

  /**
   * XML representation. May be different from the String used to generate the input; only those
   * tags that were parsed by the get() method will be output here.
   */
  public String xmlString() {
    return "<INVINFO> "
        + kind.xmlString()
        + "<DAIKON>"
        + daikonRep
        + " </DAIKON> "
        + "<METHOD> "
        + method
        + " </METHOD>"
        + "<INV>"
        + invRep
        + "</INV>"
        + " <DAIKONCLASS>"
        + daikonClass
        + " </DAIKONCLASS>"
        + "</INVINFO>";
  }

  /**
   * Find, parse and return all distinct annotations found in a String. The string {@code
   * annoString} may contain none, one, or several annotations. Malformed annotations are ignored.
   */
  public static Annotation[] findAnnotations(String annoString) {
    List<String> l = Collections.singletonList(annoString);
    return findAnnotations(l);
  }

  /**
   * Find, parse and return all distinct annotations found in a list of strings. Each string in
   * {@code annoString} may contain none, one, or several annotations. Malformed annotations are
   * ignored.
   */
  public static Annotation[] findAnnotations(List<String> annoStrings) {

    if (annoStrings == null) {
      return new Annotation[] {};
    }
    //Pattern p = Pattern.compile("(<INVINFO>.*</INVINFO>)");
    Set<Annotation> annos = new HashSet<Annotation>();
    for (String location : annoStrings) {
      if (location == null || location.equals("")) {
        continue;
      }
      String[] cutUp = location.split("<INVINFO>");
      //Matcher m = p.matcher(location);
      for (int splits = 0; splits < cutUp.length; splits++) {
        //while (m.find()) {
        try {
          //String s = m.group(1);
          String s = cutUp[splits];
          Annotation anno = Annotation.get("<INVINFO>" + s);
          // [[[ explain this! ]]]
          annos.add(anno);
        } catch (Exception e) {
          // malformed annotation; just go to next iteration
        }
      }
    }
    return annos.toArray(new Annotation[] {});
  }

  // This class should really be an enum.
  /**
   * A class representing the kind of an annotation. An invariant is either {@code Kind.enter},
   * {@code Kind.exit}, or {@code Kind.objectInvariant} For well-formed Annotations, the following
   * holds:
   *
   * <pre>
   *    a.kind == Kind.enter
   * || a.kind == Kind.exit
   * || a.kind == Kind.objectInvariant
   * </pre>
   */
  /*@UsesObjectEquals*/
  public static class Kind {
    public final String name;
    public final String xmlname;

    private Kind(String name, String xmlname) {
      this.name = name;
      this.xmlname = xmlname;
    }
    /*@Pure*/
    @Override
    public int hashCode(/*>>>@GuardSatisfied Kind this*/) {
      return name.hashCode();
    }
    /*@SideEffectFree*/
    @Override
    public String toString(/*>>>@GuardSatisfied Kind this*/) {
      return name;
    }

    public String xmlString() {
      return xmlname;
    }

    public static final Kind enter = new Kind("precondition ", "<ENTER>");
    public static final Kind exit = new Kind("postcondition", "<EXIT>");
    public static final Kind objectInvariant = new Kind("obj invariant", "<OBJECT>");
  }

  /** Easy-on-the-eye format. */
  /*@SideEffectFree*/
  @Override
  public String toString(/*>>>@GuardSatisfied Annotation this*/) {
    return kind.toString() + " : " + daikonRep();
  }

  /** Two annotations are equal iff their fields "daikonRep", "method" and "kind" are equal. */
  /*@EnsuresNonNullIf(result=true, expression="#1")*/
  /*@Pure*/
  @Override
  public boolean equals(
      /*>>>@GuardSatisfied Annotation this,*/
      /*@GuardSatisfied*/ /*@Nullable*/ Object o) {
    if (o == null) {
      return false;
    }
    if (!(o instanceof Annotation)) {
      return false;
    }
    Annotation anno = (Annotation) o;
    return (this.daikonRep().equals(anno.daikonRep())
        && (this.method().equals(anno.method()))
        && (this.kind().equals(anno.kind())));
  }

  /*@Pure*/
  @Override
  public int hashCode(/*>>>@GuardSatisfied Annotation this*/) {
    return daikonRep.hashCode() + kind.hashCode() + method.hashCode();
  }

  /** Get the annotation with corresponding properties. */
  public static Annotation get(
      Kind kind, String daikonRep, String method, String invRep, String daikonClass)
      throws Annotation.MalformedAnnotationException {

    Annotation anno = new Annotation(kind, daikonRep, method, invRep, daikonClass);
    Integer key = anno.hashCode();
    if (annotationsMap.containsKey(key)) {
      return annotationsMap.get(key);
    } else {
      annotationsMap.put(key, anno);
      return anno;
    }
  }

  // This is never used, and the "break" clause seems to be buggy, so
  // that this returns at most one property.
  // /**
  //  * The annotations in {@code annas} of kind {@code kind}.
  //  */
  // public static Annotation[] getKind(Annotation[] annas, Kind kind) {
  //   List<Annotation> retval = new ArrayList<Annotation>();
  //   for (int i = 0; i < annas.length; i++) {
  //     if (kind == annas[i].kind) {
  //       retval.add(annas[i]);
  //     }
  //     break;
  //   }
  //   return retval.toArray(new Annotation[] {
  //   });
  // }

}
