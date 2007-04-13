package binary_variables;

import utilMDE.UtilMDE;

import java.io.*;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.TreeSet;

/**
 * Date: 22/02/2007
 */
public final class Program {
  public static void main(String[] args) throws Throwable {
    //new TestModule();
    Program p = new Program();
    p.phase(null, UtilMDE.lineNumberFileReader(args[0]) );
    System.out.println("Finished phase 1");
    p.phase(UtilMDE.bufferedFileWriter(args[1]), UtilMDE.lineNumberFileReader(args[0]) );

    //p.phase(UtilMDE.bufferedFileWriter(args[1].replace("gz","txt")), UtilMDE.lineNumberFileReader(args[0]) );
    System.out.println("Finished phase 2, and ended program");
  }


  private HashMap<Integer, ArrayList<Line>> thread2bb = new HashMap<Integer, ArrayList<Line>>();
  private Execution execution = new Execution();
  public static boolean useDominators = false;
  public void phase(Writer out, BufferedReader input) throws Throwable {
    if (out!=null) {
      // second phase: writing the output
      out.write("\n" +
          "VarComparability\n" +
          "none\n" +
          "\n" +
          "ListImplementors\n" +
          "java.util.List\n" +
          "\n");

      int max_var_num = 0;
      for (BasicBlock bb : execution.address2BB.values()) {
        out.write("DECLARE\n"+bb.getPPTName()+"\n");
        max_var_num = Math.max(max_var_num, bb.binaryVariables.size());

        TreeSet<BasicBlock> dominators = useDominators ? bb.dominators : new TreeSet<BasicBlock>();
        if (!useDominators) dominators.add(bb);
        for (BasicBlock d : dominators)
          for (BinaryVariable bv : d.binaryVariables)
            out.write(bv.getFullName(d)+"\nint\nint\n22\n");
        out.write("\n");
      }
      System.out.println("max_var_num="+max_var_num);
      execution.setOutput(out);
    }
    int lineNo = 0;
    String line=null;
    try {
      while ( (line=input.readLine())!=null ) {
        lineNo++;
        line = line.trim();
        if (!line.equals("") && !line.startsWith("@") && !line.startsWith("#"))
          readLine(line);
      }
    } catch(Throwable e) {
      System.err.println("Error in line "+lineNo+": '"+line+"'");
      throw e;
    }
    input.close();
    // adding all the left-over bbs
    for (Integer thread : thread2bb.keySet()) {
      ArrayList<Line> bb = thread2bb.get(thread);
      if (bb.size()>0) {
        addBB(null, bb, thread);
      }
    }
    if (out!=null) out.close();
    //System.out.println(execution);
  }
  private void addBB(Address addr, ArrayList<Line> bb, int thread) throws IOException {
    // we assert that all BVs are from this module
    TreeSet<BinaryVariable> binaryVariables = new TreeSet<BinaryVariable>();
    for (Line l : bb) {
      if (l.bv.value !=null) assert l.bv.value.equals(l.value);
      l.bv.value = l.value;
      binaryVariables.add(l.bv);
    }
    Address bbAddr = bb.size()==0 ? addr : new Address(bb.get(0).bv);
    execution.addBB(thread, bbAddr, binaryVariables);
    thread2bb.put(thread, new ArrayList<Line>());
  }
  private static Address parseAddr(String addr) {
    String[] arr = addr.split(":");
    return new Address(parseHex(arr[1]), arr[0]);
  }
  private static long parseHex(String addr) {
    if (addr.equals("???")) return 0; // todo: Sung should fix this!
    assert addr.startsWith("0x");
    return Long.parseLong(addr.substring(2), 16);
  }
  private static final long sysenterLocation = parseHex("0xeb8b");
  private static boolean isSysEnter(Addressable bv) { // There is an implicit return in this address (We can't see the code there since it is a system call)
    return bv.module.equalsIgnoreCase("ntdll.dll") && bv.address==sysenterLocation;
  }
  private void readLine(String line) throws IOException {
    // I work in two phases: first we discover the exact set of dominators and know if a BB will be split, then we output all the program points
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

    if (!thread2bb.containsKey(thread)) thread2bb.put(thread, new ArrayList<Line>());
    ArrayList<Line> bb = thread2bb.get(thread);

    if (bb.size()>0) {
      BinaryVariable last = bb.get(bb.size()-1).bv;

      if (isSysEnter(last) && !isSysEnter(addr)) {
        //System.out.println("Found sysenter on line="+line);
        addBB(addr, bb, thread);
        execution.returnFunction(thread, addr);
      }
    }

    if (type.equals("BV")) {
      //588	KERNEL32.dll:0x16fc6	BV	src_ebp	1245168
      String name = args[3];
      BinaryVariable bv = new BinaryVariable(name, addr);
      long value = parseHex(args[4]);
      bb.add( new Line(bv, value) );
    } else {
      //588	KERNEL32.dll:0x24fa	BB
      // Note that some basic blocks don't have a start tag because determina merges basic blocks together
      // ended a basic block
      if (bb.size()>0) addBB(addr, bb, thread);


      if (type.equals("CALL") || type.equals("ICALL")) {
        //1796	KERNEL32.dll:0xb3ca MBR to ntdll.dll:0x13171
        //2748	KERNEL32.dll:0x16fce ICALL to ntdll.dll:0xe642 ret KERNEL32.dll:0x16fd4
        Address funcAddr = parseAddr(args[4]);
        Address returnAddr = parseAddr(args[6]);
        execution.callFunction(thread, funcAddr, returnAddr);

      } else if (type.equals("BB")) {
        // nothing to do

      } else if (type.equals("RET")) {
        //2748	KERNEL32.dll:0x16fc0	RET to KERNEL32.dll:0x16fc0
        Address returnAddr = parseAddr(args[4]);
        execution.returnFunction(thread, returnAddr);
      } else if (type.equals("CBR") || type.equals("UBR") || type.equals("MBR")) {
        //588	simple.exe:0x2a07 CBR to simple.exe:0x2a02
        //Address targetAddr = parseAddr(args[4]);
        // We saw that a call and return can be in different modules:  assert targetAddr.module.equals(addr.module);
      } else
        assert false : type;
    }


  }

}
class Line {
  final BinaryVariable bv;
  final Object value;
  Line(BinaryVariable bv, Object value) {
    this.bv = bv;
    this.value = value;
  }
}
