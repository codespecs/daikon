package binary_variables;

/**
 * Date: 22/02/2007
 */
public final class BinaryVariable extends Addressable<BinaryVariable> {
  public final String name;
  public Object value;
  private final String fullName;
  public BinaryVariable(String name, Address addr) {
    this(name, addr, null);
  }
  public BinaryVariable(String name, Address addr, Object value) {
    super(addr);
    this.name = name.intern();
    this.value = value;
    fullName = (name.replace('[','_').replace(']','_').replace('+','_').replace('-','_')+"_"+getAddr()).intern();
  }

  public int compareTo(BinaryVariable o) {
    int res = super.compareTo(o);
    return res==0 ? name.compareTo(o.name) : res;
  }
  public boolean equals(Object o) {
    BinaryVariable b = (BinaryVariable)o;
    return super.equals(o) && b.name.equals(name);
  }
  public XmlWriter toXml() {
    XmlWriter xmlWriter = new XmlWriter();
    xmlWriter.add("name", name);
    xmlWriter.add("address", getAddr());
    return xmlWriter;
  }
  public String toString() {
    return toXml().toXml("BinaryVariable");
  }
  public String getFullName(BasicBlock b) {
    return b.getAddr()+"_"+fullName;
  }
}
