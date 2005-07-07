package daikon.chicory;


import org.apache.bcel.generic.*;


/**
 * Any information needed by InstTransform routines about the method
 * and class
 */
public class MethodContext {

  public ClassGen cg;
  public ConstantPoolGen cpg;
  public InstructionFactory ifact;
  public MethodGen mgen;

  public MethodContext (ClassGen cg) {
    this.cg = cg;
    ifact = new InstructionFactory (cg);
    cpg = cg.getConstantPool();
  }

  public MethodContext (ClassGen cg, MethodGen mgen) {
    this.cg = cg;
    ifact = new InstructionFactory (cg);
    cpg = cg.getConstantPool();
    this.mgen = mgen;
  }
}
