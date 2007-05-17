package binary_variables;

import utilMDE.UtilMDE;

import java.io.*;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.TreeSet;

import daikon.Daikon;

/**
 * Main takes src_trace_file and creates target_dtrace_file.
 * The input file is a trace file output of the front-end,
 *  as described in our design document
 * (and Sung's Wiki: http://scripts.mit.edu/~hunkim/wiki/index.php?title=Binary_Variable_Front-end_Formant),
 * and the output is a dtrace file (the input to Daikon).
 * The translation currently works in two phases:
 * 1) We calculate the control-flow structure and the dominators of each basic block,
 * 2) We output a program point for each basic block that includes binary variables
 *  from that basic block and some from its dominators.
 *
 * I tested this translation using both TestExecution and a small C program that we traced:
 void add(int* res, int val) {
  *res += val;
 }
 int f(int i) {
  char buf[100];
  int j;
  int sum;
  sum = 0;
  for (j=0; j<i; j++) {
   buf[j] = i;
   add(&sum, buf[j]);
  }
  return sum;
 }
 int main() {
  int i;
  for (i=0; i<42; i++)
   f(i);
  return 0;
 }
 *
 * Date: 22/02/2007
 */
public final class Main {
  public static void main(String[] args) throws Throwable {
    if (args.length!=2) {
      throw new Daikon.TerminationMessage("Usage: java binary_variables.Main src_trace_file target_dtrace_file");
    }
    //new TestExecution();
    Main p = new Main();
    p.phase(null, UtilMDE.lineNumberFileReader(args[0]) );
    System.out.println("Finished phase 1");
    p.phase(UtilMDE.bufferedFileWriter(args[1]), UtilMDE.lineNumberFileReader(args[0]) );

    //p.phase(UtilMDE.bufferedFileWriter(args[1].replace("gz","txt")), UtilMDE.lineNumberFileReader(args[0]) );
    System.out.println("Finished phase 2, and ended program");
    //System.out.println(p.execution);
  }


  private HashMap<Integer, ArrayList<BinaryVariable>> thread2bb = new HashMap<Integer, ArrayList<BinaryVariable>>();
  private Execution execution = new Execution();

  /**
   * In the first phase, out is null,
   * and in the second phase out is the dtrace file we wish to write.
   */
  public void phase(final Writer out, BufferedReader input) throws Throwable {
    if (out!=null) {
      // second phase: writing the output
      out.write("\n" +
          "VarComparability\n" +
          "none\n" +
          "\n");

      int max_var_num = 0;
      for (BasicBlock bb : execution.address2BB.values()) {
        out.write("DECLARE\n"+bb.getPPTName()+"\n");
        max_var_num = Math.max(max_var_num, bb.binaryVariables.size());
        bb.visitVariables( new BasicBlock.VisitBinaryVariable() {
          public void visit(BinaryVariable bv) throws Throwable {
            out.write(bv.getFullName()+"\nint\nint\n22\n");
          }
        });
        out.write("\n");
      }
      System.out.println("maximum number of variables in a program point is "+max_var_num);
      execution.setOutput(out);
    }
    int lineNo = 0;
    String line=null;
    try {
      while ( (line=input.readLine())!=null ) {
        lineNo++;
        line = line.trim();
        if (!line.equals("") && !line.startsWith("@") && !line.startsWith("#")) // comment lines start with @ or #
          readLine(line);
      }
    } catch(Throwable e) {
      System.err.println("Error in line "+lineNo+": '"+line+"'");
      throw e;
    }
    input.close();
    // adding the last basic block for each thread.
    for (Integer thread : thread2bb.keySet()) {
      ArrayList<BinaryVariable> bb = thread2bb.get(thread);
      if (bb.size()>0) {
        addBB(null, bb, thread);
      }
    }
    if (out!=null) out.close();
  }

  /**
   * Adds a basic block in address (addr) with binary variables (bb) that was executed by thread.
   */
  private void addBB(Address addr, ArrayList<BinaryVariable> bb, int thread) throws Throwable {
    // we assert that all BVs are from this module, and they are all in increasing address order
    if (bb.size()>0) {
      BinaryVariable first = bb.get(0);
      String module = first.module;
      long offset = first.address;
      for (BinaryVariable bv : bb) {
        assert bv.module.equals(module);
        //todo: Sung has a bug in his trace file where sometimes this invariant fails:
        /*
line 509:
1512	buf.exe:0x1923	BB	# 0x00401923
1512	buf.exe:0x1923	BV	esi	0x00140650
1512	buf.exe:0x1923	BV	[4+esi]	0x140178	# 0x00140654
1512	buf.exe:0x1927	BV	esi	0x00140650
1512	buf.exe:0x1927	BV	[0+esi]	0x140178	# 0x00140650
1512	buf.exe:0x1933	BV	esi	0x00140650
1512	buf.exe:0x1936	BV	eax	0x00140650
1512	buf.exe:0x191f	BV	esi	0x00140658     ******************************** 0x191f < 0x1936
1512	buf.exe:0x191f	BV	eax	0x00140750
1512	buf.exe:0x1921 CBR to buf.exe:0x193d #op=OP_jnb_short

without any jump in between...
        */
        if (false)
          assert offset <= bv.address; // sometimes you have the same address, e.g.,
          //buf.exe:0x10fa	BV	eax	0x0012ffe0
          //buf.exe:0x10fa	BV	esp	0x0012ffb4
        offset = bv.address;
      }
    }

    TreeSet<BinaryVariable> binaryVariables = new TreeSet<BinaryVariable>(bb);
    // the start address of a basic block is its first binary variable (if one exists)
    Address bbAddr = bb.size()==0 ? addr : new Address(bb.get(0));
    assert bbAddr!=null;
    execution.addBB(thread, bbAddr, binaryVariables);
    thread2bb.put(thread, new ArrayList<BinaryVariable>());
  }

  /**
   * Given an string such as "ntdll.dll:0xeb8b"
   * it returns an Address object.
   */
  private static Address parseAddr(String addr) {
    String[] arr = addr.split(":");
    return new Address(parseHex(arr[1]), arr[0]);
  }

  /**
   * Given a hex number such as "0xeb8b", returns an equal long number.
   */
  private static long parseHex(String addr) {
    if (addr.equals("???")) return 0; // todo: Sung should fix this!
    assert addr.startsWith("0x");
    return Long.parseLong(addr.substring(2), 16);
  }

  // This is a special case we found during our experiments:
  // ntdll.dll:0xeb8b is a special address that have a sysenter instruction.
  // Determina's instrumentation misses this instruction (you cannot instrument a system call).
  private static final long sysenterLocation = parseHex("0xeb8b");
  private static boolean isSysEnter(Addressable bv) { // There is an implicit return in this address (We can't see the code there since it is a system call)
    return bv.module.equalsIgnoreCase("ntdll.dll") && bv.address==sysenterLocation;
  }

  /**
   * Handles one line in the input trace file.
   * Each line has the following pattern:
   * thread-id Address type
   * where type is either BV, CALL, ICALL, BB, RET, CBR, UBR, MBR
   * Address is module:offset-in-hex
   */
  private void readLine(String line) throws Throwable {
    String[] args = line.split("[ \t]+");
    for (int i=0; i<args.length; i++) args[i] = args[i].trim();

    int thread;
    try {
      thread = Integer.parseInt(args[0]);
    } catch(NumberFormatException e) {
      return;
    }
    Address addr = parseAddr(args[1]);
    if (addr.module.equalsIgnoreCase("NULL")) {
      System.out.println("Found a NULL module");
      return;
    }
    String type = args[2];

    if (!thread2bb.containsKey(thread)) thread2bb.put(thread, new ArrayList<BinaryVariable>());
    ArrayList<BinaryVariable> bb = thread2bb.get(thread);

    if (bb.size()>0) {
      BinaryVariable last = bb.get(bb.size()-1);

      if (isSysEnter(last) && !isSysEnter(addr)) {
        //System.out.println("Found sysenter on line="+line);
        addBB(addr, bb, thread);
        execution.returnFunction(thread, addr);
      }
    }

    if (type.equals("BV")) { // binary variable
      //588	KERNEL32.dll:0x16fc6	BV	src_ebp	1245168
      String name = args[3];
      long value = parseHex(args[4]);
      BinaryVariable bv = new BinaryVariable(name, addr, value);
      bb.add(bv);
    } else {
      //588	KERNEL32.dll:0x24fa	BB
      // Note that some basic blocks don't have a start tag because determina merges basic blocks together
      // ended a basic block
      if (bb.size()>0) addBB(addr, bb, thread);


      if (type.equals("CALL") || type.equals("ICALL")) { // function call (either direct or indirect)
        //1796	KERNEL32.dll:0xb3ca MBR to ntdll.dll:0x13171
        //2748	KERNEL32.dll:0x16fce ICALL to ntdll.dll:0xe642 ret KERNEL32.dll:0x16fd4
        Address funcAddr = parseAddr(args[4]);
        Address returnAddr = parseAddr(args[6]);
        execution.callFunction(thread, funcAddr, returnAddr);

      } else if (type.equals("BB")) { // start of a basic block
        // nothing to do, since I implicitly end a basic block when I find a control-flow change (such as call, ret, or jump)

      } else if (type.equals("RET")) { // function return
        //2748	KERNEL32.dll:0x16fc0	RET to KERNEL32.dll:0x16fc0
        Address returnAddr = parseAddr(args[4]);
        execution.returnFunction(thread, returnAddr);
      } else if (type.equals("CBR") || type.equals("UBR") || type.equals("MBR")) { // jump (conditional, unconditional, multi-way branch)
        //588	simple.exe:0x2a07 CBR to simple.exe:0x2a02
        //Address targetAddr = parseAddr(args[4]);
        // We saw that a call and return can be in different modules:  assert targetAddr.module.equals(addr.module);
      } else
        assert false : type;
    }
  }
}
