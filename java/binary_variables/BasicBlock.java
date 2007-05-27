package binary_variables;

import java.util.TreeSet;
import java.util.HashSet;

/**
 * A basic block contains a list of binary variables,
 * the function it belongs to, and basic blocks that are the dominators, predecessors, and successors.
 *
 * Date: 22/02/2007
 */
public final class BasicBlock extends Addressable<BasicBlock> implements XmlSerialize {
  // useDominators determines if we should use dominators when outputing the dtrace file
  // In some cases there are many dominators and therefore there are too many variables in a program point.
  public final static boolean useDominators = true;

  public final TreeSet<BinaryVariable> binaryVariables;
  public Function function; // the function can change since sometimes basic blocks are shared between several functions.
  // the set of dominators decreases as we see more sample executions
  // (dominators are basic blocks in the same function that always get executed before this basic block.)
  final TreeSet<BasicBlock> dominators = new TreeSet<BasicBlock>(); // always include yourself (this)

  public BasicBlock(Address addr, TreeSet<BinaryVariable> binaryVariables, Function f) {
    super(addr);
    this.binaryVariables = binaryVariables;
    this.function = f;
  }
  /**
   * For serialization in XML. See XmlSerialize.
   */
  public XmlWriter toXml() {
    XmlWriter xmlWriter = new XmlWriter();
    xmlWriter.add("address", super.toString());

    XmlWriter xml = new XmlWriter();
    for (BasicBlock bb : dominators) xml.add("dominator",bb.getAddr());
    xmlWriter.add("dominators", xml);

    xmlWriter.add("function", function!=null ? function.getAddr() : "");
    XmlWriter bv = new XmlWriter();
    for (BinaryVariable b : binaryVariables) bv.add("binary-variable",b.toXml());
    xmlWriter.add("binary-variables", bv);
    return xmlWriter;
  }
  public String toString() {
    return toXml().toXml("BasicBlock");
  }

  /**
   * @return a unique name for the program point associated with this basic block.
   */
  public String getPPTName() {
    return "BB-"+getAddr()+"-InFunction-"+(function==null?"NULL":function.getAddr())+":::";
  }

  interface VisitBinaryVariable {
    void visit(BinaryVariable bv) throws Throwable;
  }

  /**
   * Traverses all the binary variables in this program point.
   * (Using a visitor is easier to implement than using an iterator.)
   * A program point includes the binary variables from this basic block,
   * and some binary variables from its dominators, e.g.,
   * memory location that are read (and their access chain should not have changed).
   * See our design document (in CVS: CVSROOT=:ext:USERNAME@pag.csail.mit.edu:/afs/csail.mit.edu/group/pag/projects/application_communities/.CVS)
   *
   * @param visit
   * @throws Throwable
   */
  public void visitVariables(VisitBinaryVariable visit) throws Throwable {
    TreeSet<BasicBlock> d = useDominators ? dominators : new TreeSet<BasicBlock>();
    if (!useDominators) d.add(this);
    TreeSet<BinaryVariable> noDuplicates = new TreeSet<BinaryVariable>(); // I want to test we have no duplicate binary variables in multiple basic blocks
    HashSet<String> visitedRegisters = new HashSet<String>();
    for (BasicBlock b : d)
      for (BinaryVariable bv : b.binaryVariables) {
        assert !noDuplicates.contains(bv);
        noDuplicates.add(bv);
        if (b==this || !bv.isRegister) { // from the dominators we visit only memory (not registers),
            // since only memory can be accessed using a live shield (see our design document)

          if (bv.isRegister) {
            // Output the value of a register only once.
            // The reason is that the calculation in a basic block is determinitic (no control flow changes),
            // so invariants found between future values of a register are always true (and thus cannot be violated in an attack).
            if (!visitedRegisters.contains(bv.name)) {
              visitedRegisters.add(bv.name);
              visit.visit(bv);
            }
          } else {
            // memory location
            //todo: examine the registers used in accessing this memory location,
            // and make sure no register have changed from the time of the memory access
            // until the beginning of this basic block.
            visit.visit(bv);
          }
        }
      }
  }
}
