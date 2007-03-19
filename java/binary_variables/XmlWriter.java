package binary_variables;

/**
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
        return toXml("SomeTag");
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