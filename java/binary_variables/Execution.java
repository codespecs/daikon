package binary_variables;

import java.io.PrintStream;
import java.io.Writer;
import java.io.IOException;
import java.util.*;

/**
 * Date: 22/02/2007
 */
public final class Execution {
  private Writer out;
  private HashMap<Integer, ArrayList<StackEntry>> thread2stack = new HashMap<Integer, ArrayList<StackEntry>>(); // thread_id -> stack
  public HashMap<Address, BasicBlock> address2BB = new HashMap<Address, BasicBlock>();
  private HashMap<Address, Function> address2Func = new HashMap<Address, Function>();

  public Execution() {
  }
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
   * Thread thread_id executed a BasicBlock at address and found the values for the binary variables.
   * @param thread_id
   * @param address
   * @param values (can be null in the first stage in which we just study the control flow)
   */
  public void addBB(int thread_id, Address address, TreeSet<BinaryVariable> values) throws IOException {
    // make sure all variables are in the same module
    for (BinaryVariable bv : values)
      assert bv.module.equals(address.module);

    BasicBlock bb;
    if (!address2BB.containsKey(address)){
      bb = new BasicBlock(address, values);
      address2BB.put(address, bb);
    } else {
      bb = address2BB.get(address);
      // Make sure "bb" has the same set of BVs
      assert bb.binaryVariables.equals(values) : "values="+values+"\noldValues="+bb.binaryVariables; // it ignore the value in the binary variable
    }
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
    if (bb.function!=null) {
      if (bb.function==f) {
        // all ok :)
      } else {
        /* Then this BB must be a function entry!
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
        assert bb.function==newFunc;*/
      }
    } else {
      bb.function = f;
      // add the BB to the function
      if (!f.basicBlocks.contains(bb)) {
        f.basicBlocks.add(bb);
        // Do I need to split another BB? It is enough to check just the one before me.
        SortedSet<BasicBlock> headSet = f.basicBlocks.headSet(bb);
        if (headSet.size()>0 && bb.binaryVariables.size()>0) {
          BasicBlock beforeMe = headSet.last();
          if (beforeMe.binaryVariables.size()>0 &&  beforeMe.binaryVariables.last().equals(bb.binaryVariables.last())) {
            // we need to split because we share binary-variables
            // So what if you share a binary variable? the program point can include variables from next blocks
            //System.out.println("Spliting! Removing: "+bb.binaryVariables);
            //beforeMe.binaryVariables.removeAll(bb.binaryVariables);
          }
        }
      }
    }
    assert bb.function.basicBlocks.contains(bb) : "f="+bb.function+" bb="+bb;

    // refine the set of dominators
    top.basicBlocks.put(bb, values);
    if (bb.dominators.size()==0)
      bb.dominators.addAll(top.basicBlocks.keySet());
    else
      bb.dominators.retainAll(top.basicBlocks.keySet());
    // add to predecessors
    if (top.lastBB!=null) {
      //System.out.println("Edge:"+top.lastBB.startAddress+" to "+bb.startAddress);
      bb.predecessors.add(top.lastBB);
    }
    //System.out.println("In bb="+bb.startAddress);
    top.lastBB = bb;

    if (out!=null) {
      out.write(bb.getPPTName()+"\n");

      TreeSet<BasicBlock> dominators = Program.useDominators ? bb.dominators : new TreeSet<BasicBlock>();
      if (!Program.useDominators) dominators.add(bb);
      for (BasicBlock d : dominators) {
        TreeSet<BinaryVariable> t = top.basicBlocks.get(d);
        for (BinaryVariable bv : t)
          out.write(bv.getFullName(d)+"\n"+bv.value+"\n1\n");
      }
      out.write("\n");
    }
  }
  public void callFunction(int thread_id, Address address, Address returnAddress) {
    ArrayList<StackEntry> stack = thread2stack.get(thread_id);
    assert stack!=null : "thread_id="+thread_id+" address="+address+" returnAddress="+returnAddress+" thread2stack="+thread2stack;
    if (!address2Func.containsKey(address)) address2Func.put(address, new Function(address));
    Function f = address2Func.get(address);
    stack.add( new StackEntry(f, returnAddress) );
  }
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

class StackEntry {
  final Function function;
  final Address returnAddress;
  TreeMap<BasicBlock,TreeSet<BinaryVariable>> basicBlocks = new TreeMap<BasicBlock,TreeSet<BinaryVariable>>(); // all the BBs executed so far in this function. For each BB we keep the latest values found in it.
  BasicBlock lastBB; // to add to bb.predecessors
  StackEntry(Function function, Address returnAddress) {
    this.function = function;
    this.returnAddress = returnAddress;
  }

  public XmlWriter toXml() {
    XmlWriter xmlWriter = new XmlWriter();
    xmlWriter.add("returnAddress", returnAddress);
    xmlWriter.add("function", function.getAddr());
    xmlWriter.add("lastBB", lastBB==null ? "" : lastBB.getAddr());
    XmlWriter bbs = new XmlWriter();
    for (BasicBlock bb : basicBlocks.keySet()) bbs.add("basic-block", bb.getAddr());
    xmlWriter.add("basic-blocks", bbs);
    return xmlWriter;
  }
  public String toString() {
    return toXml().toXml("StackEntry");
  }
}