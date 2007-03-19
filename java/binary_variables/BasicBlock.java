package binary_variables;

import java.util.TreeSet;

/**
 * Date: 22/02/2007
 */
public final class BasicBlock extends Addressable<BasicBlock> {
  public final TreeSet<BinaryVariable> binaryVariables;
  Function function;
  TreeSet<BasicBlock> dominators = new TreeSet<BasicBlock>(); // including yourself
  TreeSet<BasicBlock> predecessors = new TreeSet<BasicBlock>(); // not including yourself

  public BasicBlock(Address addr, TreeSet<BinaryVariable> binaryVariables) {
    super(addr);
    this.binaryVariables = binaryVariables;
  }

  public XmlWriter toXml() {
    XmlWriter xmlWriter = new XmlWriter();
    xmlWriter.add("address", super.toString());
    XmlWriter domin = new XmlWriter();
    for (BasicBlock bb : dominators) domin.add("dominator",bb.getAddr());
    xmlWriter.add("dominators", domin);
    XmlWriter predXml = new XmlWriter();
    for (BasicBlock bb : predecessors) predXml.add("predecessor",bb.getAddr());
    xmlWriter.add("predecessors", predXml);
    xmlWriter.add("function", function!=null ? function.getAddr() : "");
    XmlWriter bv = new XmlWriter();
    for (BinaryVariable b : binaryVariables) bv.add("binary-variable",b.toXml());
    xmlWriter.add("binary-variables", bv);
    return xmlWriter;
  }
  public String toString() {
    return toXml().toXml("BasicBlock");
  }
  public String getPPTName() {
    return "BB-"+getAddr()+"-InFunction-"+function.getAddr()+":::";
  }
}
