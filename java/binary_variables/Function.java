package binary_variables;

import java.util.TreeSet;

/**
 * Date: 22/02/2007
 */
public final class Function extends Addressable<Function> {
  TreeSet<BasicBlock> basicBlocks = new TreeSet<BasicBlock>();

  public Function(Address addr) {
    super(addr);
  }

  public XmlWriter toXml(boolean isDeep) {
    XmlWriter xmlWriter = new XmlWriter();
    xmlWriter.add("address", getAddr());
    XmlWriter bbs = new XmlWriter();
    for (BasicBlock bb : basicBlocks) bbs.add("basic-block", isDeep ? bb.toXml() : bb.getAddr());
    xmlWriter.add("basic-blocks", bbs);
    return xmlWriter;
  }
  public String toString() {
    return toXml(true).toXml("Function");
  }
}
