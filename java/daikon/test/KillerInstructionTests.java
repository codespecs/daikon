package daikon.test;

import java.util.ArrayList;
import java.util.List;

import daikon.asm.IInstruction;
import daikon.asm.KillerInstruction;
import daikon.asm.X86Instruction;
import junit.framework.TestCase;

public class KillerInstructionTests extends TestCase {

  public static void testKillerInstruction1() {

    List<X86Instruction> path = new ArrayList<X86Instruction>();
    X86Instruction i1 = X86Instruction.parseInstruction("x.dll:0x01 pop eax ebx -> ecx edx");
    path.add(i1);

    KillerInstruction i = new KillerInstruction(path);
    assertKills(i, "ecx", "edx");
    assertNotKills(i, "eax", "ebx");

    i1 = X86Instruction.parseInstruction("x.dll:0x02 pop eax ebx ecx -> ecx eax");
    path.add(i1);

    i = new KillerInstruction(path);
    assertKills(i, "ecx", "edx", "eax");
    assertNotKills(i, "ebx");

    i1 = X86Instruction.parseInstruction("x.dll:0x03 pop eax ebx -> ecx eax");
    path.add(i1);

    i = new KillerInstruction(path);
    assertKills(i, "ecx", "edx", "eax");
    assertNotKills(i, "ebx");

    i1 = X86Instruction.parseInstruction("x.dll:0x04 pop esi edi -> esp ebp");
    path.add(i1);

    i = new KillerInstruction(path);
    assertKills(i, "ecx", "edx", "eax", "esp", "ebp");
    assertNotKills(i, "ebx", "esi", "edi");

    i1 = X86Instruction.parseInstruction("x.dll:0x05 pop eax ebx esi -> esp ebp");
    path.add(i1);

    i = new KillerInstruction(path);
    assertKills(i, "ecx", "edx", "eax", "esp", "ebp");
    assertNotKills(i, "ebx", "esi", "edi");
  }

  private static void assertKills(KillerInstruction i, String... vars) {
    for (String v : vars) {
      assert i.kills(v);
    }
  }

  private static void assertNotKills(KillerInstruction i, String... vars) {
    for (String v : vars) {
      assert !i.kills(v);
    }
  }

  private static void addX86Instruction(List<IInstruction> path, String string) {
    X86Instruction i = X86Instruction.parseInstruction(string);
    path.add(i);
  }
}
