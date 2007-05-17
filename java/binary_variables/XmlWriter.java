package binary_variables;

/**
 * XmlWriter represents an XML file in memory that is still missing the root tag.
 * You can only add XML elements (not XML attributes).
 * For example, to build this XML:
 * <rootTag>
 *   <i>42</i>
 *   <l>
 *     <r>s1</r>
 *     <r>s2</r>
 *   </l>
 * </rootTag>
 * You use this code snippet:
 * XmlWriter root = new XmlWriter();
 * root.add("i",42);
 * XmlWriter l = new XmlWriter();
 * l.add("r","s1");
 * l.add("r","s2");
 * root.add("l",l);
 * return root.toXml("rootTag");
 *
 * Date: 23/02/2007
 */
import java.util.ArrayList;
import utilMDE.Pair;

final class XmlWriter {
    private final ArrayList<Pair<String, Object>> vec = new ArrayList<Pair<String, Object>>(2);
    public XmlWriter() { }
    public void add(String tag, Object o) {
        vec.add(new Pair<String, Object>(tag, o));
    }
    public String toString() {
        return toXml("UnknownTag");
    }
    public String toXml(String tag) {
        StringBuilder s = new StringBuilder();
        s.append("<").append(tag).append(">").append("\n");
        toXml(s, 1);
        s.append("</").append(tag).append(">").append("\n");
        return s.toString();
    }
    private void toXml(StringBuilder s, int indent) {
        for (Pair<String, Object> pair : vec) {
            String k = pair.a;
            Object v = pair.b;
            for(int i=0; i<indent; i++) s.append(" ");
            s.append("<").append(k).append(">");
            if (v instanceof XmlWriter) {
                s.append("\n");
                ((XmlWriter)v).toXml(s, indent+1);
                for(int i=0; i<indent; i++) s.append(" ");
            } else {
                s.append(v.toString());
            }
            s.append("</").append(k).append(">\n");
        }
    }
}