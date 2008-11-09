package binary_variables;

/**
 * A binary variable is identified by its name and address (module+offset).
 * The fullname is a concatenation of the name and address.
 * The name is either a register name such as ebp, esp, edi, eax, etc,
 * or a memory location such as [16+ebp], [-12+ebp], [0+eax+(ecx*4)].
 * A binary variable occurs in a single basic block (there is a single basic block for which binaryVariables contains it),
 * but a binary variable can appear in several program points (since we add binary variables of dominators).
 *
 * The only mutable part of a binary variable is its value, which changes on every execution of a basic block.
 * (It was easier to implement the value in BinaryVariable as opposed to using a HashMap that would map BVs to values.)
 *
 * Date: 22/02/2007
 */
public final class BinaryVariable extends Addressable<BinaryVariable> implements XmlSerialize  {
  public final String name;
  public final boolean isRegister;
  private final String fullName;
  public Object value; // the mutable part of a binary variable
  public BinaryVariable(String name, Address addr) {
    this(name, addr, null);
  }
  public BinaryVariable(String name, Address addr, Object value) {
    super(addr);
    this.name = name.intern();  // e.g., "[0+esi+(esi*1)]"
    this.value = value;
    // If I don't replace illegal characters (such as []()+*-) I get an error in Daikon:
    //Exception in thread "main" java.lang.UnsupportedOperationException: parse error: '[8+esp]_buf'
    // at daikon.VarInfoName.parse(VarInfoName.java:163)
    // todo: find a better replacement that is 1-1 (so we can replace back to the original name)
    fullName = (name.replace('[','_').replace(']','_').replace('(','_').replace(')','_').replace('+','_').replace('*','_').replace('-','_')
        +"_"+getAddr()).intern();
    isRegister = !name.contains("[") && !name.contains("]");
  }

  public int compareTo(BinaryVariable o) {
    int res = super.compareTo(o);
    return res==0 ? name.compareTo(o.name) : res;
  }
  public boolean equals(/*@Nullable*/ Object o) {
    if (o == null) {
      return false;
    }
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
  public String getFullName() {
    return fullName;
  }
}
