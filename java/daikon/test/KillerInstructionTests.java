package daikon.test;

import java.util.ArrayList;
import java.util.List;

import asm.IInstruction;
import asm.KillerInstruction;
import asm.X86Instruction;
import junit.framework.TestCase;


public class KillerInstructionTests extends TestCase {
  
  public static void testKillerInstruction1() {
    
    List<IInstruction> path = new ArrayList<IInstruction>();
    addX86Instruction(path, "x.dll:0x01 pop eax ebx -> ecx edx");
    
    KillerInstruction i = new KillerInstruction(path);
    assertKills(i, "ecx", "edx");
    assertNotKills(i, "eax", "ebx");
    
    addX86Instruction(path, "x.dll:0x02 pop eax ebx ecx -> ecx eax");
    
    i = new KillerInstruction(path);
    assertKills(i, "ecx", "edx", "eax");
    assertNotKills(i, "ebx");

    addX86Instruction(path, "x.dll:0x03 pop eax ebx -> ecx eax");
    
    i = new KillerInstruction(path);
    assertKills(i, "ecx", "edx", "eax");
    assertNotKills(i, "ebx");

    addX86Instruction(path, "x.dll:0x04 pop exx eyx -> ezx eux");
    
    i = new KillerInstruction(path);
    assertKills(i, "ecx", "edx", "eax", "ezx", "eux");
    assertNotKills(i, "ebx", "exx", "eyx");    
    
    addX86Instruction(path, "x.dll:0x05 pop eax ebx exx -> ezx eux");
    
    i = new KillerInstruction(path);
    assertKills(i, "ecx", "edx", "eax", "ezx", "eux");
    assertNotKills(i, "ebx", "exx", "eyx");
    
  }
  
  private static void assertKills(KillerInstruction i, String... vars) {
    for (String v : vars) assertTrue(i.kills(v));
  }
  
  private static void assertNotKills(KillerInstruction i, String... vars) {
    for (String v : vars) assertFalse(i.kills(v));
  }

  private static void addX86Instruction(List<IInstruction> path, String string) {
    X86Instruction i = X86Instruction.parseInstruction(string);
    path.add(i);
  }

}
