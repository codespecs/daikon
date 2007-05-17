package binary_variables;

import java.io.PrintStream;
import java.io.Writer;
import java.io.IOException;
import java.util.*;

/**
 * Execution represents a snapshot of the execution of a program,
 * i.e., the stack of every thread, and all the basic blocks and functions encountered so far.
 *
 * Date: 22/02/2007
 */
public final class Execution implements XmlSerialize {
  private Writer out;
  private HashMap<Integer, ArrayList<StackEntry>> thread2stack = new HashMap<Integer, ArrayList<StackEntry>>(); // thread_id -> stack
  public HashMap<Address, BasicBlock> address2BB = new HashMap<Address, BasicBlock>(); // address -> basic block
  private HashMap<Address, Function> address2Func = new HashMap<Address, Function>(); // address -> function

  public Execution() {
  }

  /**
   * In the second phase we write the dtrace file to out.
   */
  public void setOutput(Writer out) {
    this.out = out;
  }
  public XmlWriter toXml() {
    XmlWriter xmlWriter = new XmlWriter();
    XmlWriter bbs = new XmlWriter();
    for (BasicBlock bb : address2BB.values()) bbs.add("basic-block",bb.toXml());
    xmlWriter.add("basic-blocks", bbs);
    XmlWriter functions = new XmlWriter();
    for (Function f : address2Func.values()) functions.add("function",f.toXml(false));
    xmlWriter.add("functions", functions);

    XmlWriter stacks = new XmlWriter();
    xmlWriter.add("stacks", stacks);
    for (Integer threadid : thread2stack.keySet()) {
      XmlWriter stackXml = new XmlWriter();
      stacks.add("stack", stackXml);
      stackXml.add("thread-id", threadid);
      ArrayList<StackEntry> stack = thread2stack.get(threadid);
      XmlWriter entries = new XmlWriter();
      stackXml.add("entries", entries);
      for (StackEntry stackEntry: stack) {
        entries.add("entry", stackEntry.toXml());
      }
    }
    return xmlWriter;
  }
  public String toString() {
    return toXml().toXml("Execution");
  }

  /**
   * Check the validity of this snapshot.
   * We currently check that a binary variable appears in a single basic block,
   * and that a basic block appears in a single function.
   * TODO: This check currently fails in Sung's trace files:
The same binary variable appears in two basic blocks:

line 490:
1512 buf.exe:0x1909 BB # 0x00401909
1512 buf.exe:0x1909 BV esi 0x00140650
1512 buf.exe:0x1919 BV esi 0x00140650
1512 buf.exe:0x1919 BV [256+esi] 0x210116 # 0x00140750
1512 buf.exe:0x191f BV esi 0x00140650     ********************************
1512 buf.exe:0x191f BV eax 0x00140750
1512 buf.exe:0x1921 CBR to buf.exe:0x193d #op=OP_jnb_short

line 518:

1512 buf.exe:0x1923 BB # 0x00401923
1512 buf.exe:0x1923 BV esi 0x00140650
1512 buf.exe:0x1923 BV [4+esi] 0x140178 # 0x00140654
1512 buf.exe:0x1927 BV esi 0x00140650
1512 buf.exe:0x1927 BV [0+esi] 0x140178 # 0x00140650
1512 buf.exe:0x1933 BV esi 0x00140650
1512 buf.exe:0x1936 BV eax 0x00140650
1512 buf.exe:0x191f BV esi 0x00140658     ********************************
1512 buf.exe:0x191f BV eax 0x00140750
1512 buf.exe:0x1921 CBR to buf.exe:0x193d #op=OP_jnb_short
   */
  private void checkValidity() {
    // a basic block appears in a single function
    TreeMap<BasicBlock,Function> allBBs = new TreeMap<BasicBlock,Function>();
    for (Function f : address2Func.values()) {
      for (BasicBlock bb : f.basicBlocks) {
        assert !allBBs.containsKey(bb) : "Basic block appears in two functions: bb="+bb+" f1="+f + " f2="+allBBs.get(bb);
        allBBs.put(bb, f);
      }
    }

    // a binary variable appears in a single basic block
    TreeMap<BinaryVariable,BasicBlock> allBVs = new TreeMap<BinaryVariable,BasicBlock>();
    for (BasicBlock bb : address2BB.values()) {
      for (BinaryVariable bv : bb.binaryVariables) {
        assert !allBVs.containsKey(bv) : "Binary variable appears in two basic blocks: bv="+bv+" bb1="+bb + " bb2="+allBVs.get(bv);
        allBVs.put(bv, bb);
      }
    }
  }

  /**
   * Thread thread_id executed a BasicBlock at address and found the values for the binary variables.
   * @param thread_id
   * @param address
   * @param values (can be null in the first stage in which we just study the control flow)
   */
  public void addBB(int thread_id, Address address, TreeSet<BinaryVariable> values) throws Throwable {
    // make sure all variables are in the same module
    for (BinaryVariable bv : values)
      assert bv.module.equals(address.module);

    if (!thread2stack.containsKey(thread_id)) {
      ArrayList<StackEntry> stack = new ArrayList<StackEntry>();
      Address dummyAddr = new Address(-thread_id, "__main");
      Function f = new Function(dummyAddr);
      address2Func.put(dummyAddr, f);
      stack.add( new StackEntry(f, new Address(-thread_id,"Fake"))); // adding the main function, no return address
      System.out.println("Adding thread="+thread_id);
      thread2stack.put(thread_id, stack);
    }
    ArrayList<StackEntry> stack = thread2stack.get(thread_id);
    StackEntry top = stack.get(stack.size()-1);
    Function f = top.function;

    BasicBlock bb;
    if (!address2BB.containsKey(address)){
      // Adding a new basic block
      bb = new BasicBlock(address, values, f);
      address2BB.put(address, bb);

      assert !f.basicBlocks.contains(bb);
      // check if we need to split the new bb
      if (bb.binaryVariables.size()>0) {
        BinaryVariable lastBB = bb.binaryVariables.last();
        for (BasicBlock otherBB : f.basicBlocks)
          if (otherBB.binaryVariables.size()>0) {
            BinaryVariable lastOtherBB = otherBB.binaryVariables.last();
            // Remove any share BVs
            if (bb.binaryVariables.contains(lastOtherBB) || otherBB.binaryVariables.contains(lastBB)) {
              // bb and otherBB share binary variables, and one of them will be split.
              boolean isSmaller = bb.compareTo(otherBB) < 0;
              BasicBlock bb1 = isSmaller ? bb : otherBB;
              BasicBlock bb2 = isSmaller ? otherBB : bb;
              assert bb1.binaryVariables.containsAll( bb2.binaryVariables ) : "bb1="+bb1.binaryVariables + " bb2="+bb2.binaryVariables;
              bb1.binaryVariables.removeAll( bb2.binaryVariables );
            }
          }
      }
      f.basicBlocks.add(bb);

      checkValidity();
    }

    bb = address2BB.get(address);
    // sometimes we this bb belongs to a different function because there are optimization that removes function calls
    // (and share code between several functions)
    if (!bb.function.equals(f)) {
      // Then this BB must be a function entry!
      callFunction(thread_id, address, new Address(-1,"Fake"));
      Function newFunc = address2Func.get(address);
      Function g = bb.function;
      assert g.basicBlocks.contains(bb);
      assert bb.dominators.contains(bb);
      // I go over all the BBs in "g" and each one that have bb as a dominator will belong to newFunc
      for (Iterator<BasicBlock> it =g.basicBlocks.iterator(); it.hasNext();) {
        BasicBlock bbs = it.next();
        if (bbs.dominators.contains(bb)) {
          bbs.function = newFunc;
          newFunc.basicBlocks.add(bbs);
          it.remove();
        }
      }
      assert bb.function==newFunc;
    }


    if (bb.binaryVariables.equals(values))
      addBB(top, bb, values);
    else {
      // the BB must have been splitted before
      while(true) {
        //System.out.println("values="+values + " bb="+bb + " address="+address+"\n\n\n");
        assert values.containsAll(bb.binaryVariables) : values + " bb="+bb + " address="+address;
        TreeSet<BinaryVariable> tmp = (TreeSet<BinaryVariable>)values.clone();
        tmp.retainAll(bb.binaryVariables);
        addBB(top, bb, tmp);

        values.removeAll(bb.binaryVariables);
        if (values.size()==0) break;
        bb = f.basicBlocks.tailSet(bb, false).first();
      }
    }
  }
  private void addBB(StackEntry top, BasicBlock bb, TreeSet<BinaryVariable> values) throws Throwable {
    // Make sure "bb" has the same set of BVs
    assert bb.binaryVariables.equals(values) : "values="+values+"\noldValues="+bb.binaryVariables; // it ignore the value in the binary variable
    assert bb.function.basicBlocks.contains(bb) : "f="+bb.function+" bb="+bb;

    // refine the set of dominators
    top.basicBlocks.put(bb, values);
    if (bb.dominators.size()==0)
      bb.dominators.addAll(top.basicBlocks.keySet());
    else
      bb.dominators.retainAll(top.basicBlocks.keySet());
    //System.out.println("In bb="+bb.startAddress);

    if (out!=null) {
      out.write(bb.getPPTName()+"\n");
      bb.visitVariables( new BasicBlock.VisitBinaryVariable() {
        public void visit(BinaryVariable bv) throws Throwable {
          out.write(bv.getFullName()+"\n"+bv.value+"\n1\n");
        }
      });
      out.write("\n");
    }
  }

  /**
   * Thread thread_id called a function (address) and the return address is used to match calls to returns.
   */
  public void callFunction(int thread_id, Address address, Address returnAddress) {
    ArrayList<StackEntry> stack = thread2stack.get(thread_id);
    assert stack!=null : "thread_id="+thread_id+" address="+address+" returnAddress="+returnAddress+" thread2stack="+thread2stack;
    if (!address2Func.containsKey(address)) address2Func.put(address, new Function(address));
    Function f = address2Func.get(address);
    stack.add( new StackEntry(f, returnAddress) );
  }

  /**
   * Thread thread_id returned from a function to returnAddress.
   * We unwind (pop) the stack until we find the matching stack entry.
   */
  public void returnFunction(int thread_id, Address returnAddress) {
    ArrayList<StackEntry> stack = thread2stack.get(thread_id);
    assert stack!=null;
    while(true) {
      int s = stack.size() - 1;
      if (stack.get(s).returnAddress.equals(returnAddress)) {
        stack.remove(s);
        return;
      }
      stack.remove(s);
    }
  }

}

/**
 * We maintain the stack trace of each thread in Execution.
 * Each StackEntry is a call to some function, the return address,
 * the basic blocks that have been executed so far during this function call (lastBB is the last one among them)
 */
class StackEntry implements XmlSerialize {
  final Function function;
  final Address returnAddress;
  // all the BBs executed so far in this function. For each BB we keep the latest values found in it. (it includes lastBB)
  TreeMap<BasicBlock,TreeSet<BinaryVariable>> basicBlocks = new TreeMap<BasicBlock,TreeSet<BinaryVariable>>();

  StackEntry(Function function, Address returnAddress) {
    this.function = function;
    this.returnAddress = returnAddress;
  }

  public XmlWriter toXml() {
    XmlWriter xmlWriter = new XmlWriter();
    xmlWriter.add("returnAddress", returnAddress);
    xmlWriter.add("function", function.getAddr());
    XmlWriter bbs = new XmlWriter();
    for (BasicBlock bb : basicBlocks.keySet()) bbs.add("basic-block", bb.getAddr());
    xmlWriter.add("basic-blocks", bbs);
    return xmlWriter;
  }
  public String toString() {
    return toXml().toXml("StackEntry");
  }
}