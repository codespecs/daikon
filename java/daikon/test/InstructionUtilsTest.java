package daikon.test;

import daikon.asm.IInstruction;
import daikon.asm.InstructionUtils;
import daikon.asm.KillerInstruction;
import daikon.asm.X86Instruction;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import junit.framework.TestCase;
import org.checkerframework.checker.nullness.qual.KeyFor;

public class InstructionUtilsTest extends TestCase {

  public static void testComputeRedundantVars1() {
    List<IInstruction> path = new ArrayList<IInstruction>();
    addX86Instruction(path, "x.dll:0x01 pop eax ebx -> ecx edx");

    Map<String, String> reds = InstructionUtils.computeRedundantVars(path);

    assert 0 == reds.size() : reds.toString();

    addX86Instruction(path, "x.dll:0x02 pop eax ebx -> ecx edx");

    reds = InstructionUtils.computeRedundantVars(path);

    assert 2 == reds.size() : reds.toString();
    assertRedundants(reds, "bv:0x01:eax", "bv:0x02:eax");
    assertRedundants(reds, "bv:0x01:ebx", "bv:0x02:ebx");
  }

  private static void assertRedundants(
      Map<String, String> redMap, String leader, String... redundantVars) {
    if (redundantVars.length == 0) {
      return;
    }
    Set<String> redExpected = new LinkedHashSet<String>(Arrays.<String>asList(redundantVars));
    Set<String> redActual = new LinkedHashSet<String>(); // redMap.get(leader);
    for (Map.Entry<@KeyFor("redMap") String, String> e : redMap.entrySet()) {
      if (e.getValue().equals(leader)) redActual.add(e.getKey());
    }
    assert redExpected.equals(redActual) : leader;
  }

  public static void testComputeRedundantVars1a() {
    List<IInstruction> path = new ArrayList<IInstruction>();
    addX86Instruction(path, "x.dll:0x01 pop eax ebx -> ecx edx");
    addX86Instruction(path, "x.dll:0x02 pop ecx edx");

    Map<String, String> reds = InstructionUtils.computeRedundantVars(path);

    assert 0 == reds.size() : reds;
  }

  public static void testComputeRedundantVars1b() {
    List<IInstruction> path = new ArrayList<IInstruction>();
    addX86Instruction(path, "x.dll:0x01 pop eax ebx -> ecx edx");
    addX86Instruction(path, "x.dll:0x02 pop ebx eax");

    Map<String, String> reds = InstructionUtils.computeRedundantVars(path);

    assert 2 == reds.size() : reds;
    assertRedundants(reds, "bv:0x01:eax", "bv:0x02:eax");
    assertRedundants(reds, "bv:0x01:ebx", "bv:0x02:ebx");
  }

  public static void testComputeRedundantVars2() {
    List<IInstruction> path = new ArrayList<IInstruction>();
    addX86Instruction(path, "x.dll:0x01 pop eax ebx -> ecx edx");
    addX86Instruction(path, "x.dll:0x02 pop eax ebx -> ecx edx");
    addX86Instruction(path, "x.dll:0x03 pop eax ebx -> eax ebx");

    Map<String, String> reds = InstructionUtils.computeRedundantVars(path);

    assert 4 == reds.size() : reds;
    assertRedundants(reds, "bv:0x01:eax", "bv:0x02:eax", "bv:0x03:eax");
    assertRedundants(reds, "bv:0x01:ebx", "bv:0x02:ebx", "bv:0x03:ebx");
  }

  public static void testComputeRedundantVars3() {
    List<IInstruction> path = new ArrayList<IInstruction>();
    addX86Instruction(path, "x.dll:0x01 pop eax ebx -> ecx edx");
    addX86Instruction(path, "x.dll:0x02 pop eax ebx ecx -> ecx eax");
    addX86Instruction(path, "x.dll:0x03 pop eax ebx -> ecx eax");
    addX86Instruction(path, "x.dll:0x04 pop esi edi -> ebp esp");
    addX86Instruction(path, "x.dll:0x05 pop eax ebx esi -> ebp esp");

    Map<String, String> reds = InstructionUtils.computeRedundantVars(path);

    assert 5 == reds.size() : reds;
    assertRedundants(reds, "bv:0x01:eax", "bv:0x02:eax");
    assertRedundants(reds, "bv:0x01:ebx", "bv:0x02:ebx", "bv:0x03:ebx", "bv:0x05:ebx");
    assertRedundants(reds, "bv:0x04:esi", "bv:0x05:esi");
  }

  public static void testComputeRedundantVars4() {
    List<IInstruction> path = new ArrayList<IInstruction>();
    addX86Instruction(path, "x.dll:0x01 pop eax");
    addX86Instruction(path, "x.dll:0x02 pop eax");
    addX86Instruction(path, "x.dll:0x03 pop eax");
    addX86Instruction(path, "x.dll:0x04 pop esi");
    addX86Instruction(path, "x.dll:0x05 pop eax");

    Map<String, String> reds = InstructionUtils.computeRedundantVars(path);

    assert 3 == reds.size() : reds;
    assertRedundants(reds, "bv:0x01:eax", "bv:0x02:eax", "bv:0x03:eax", "bv:0x05:eax");
  }

  public static void testComputeRedundantVars5() {

    // This killer instruction should have no effect because it
    // kills variables that are not in any real instruction.
    X86Instruction i = X86Instruction.parseInstruction("x.dll:0x00 pop eax ebx -> ax bx");
    new ArrayList<X86Instruction>().add(i);
    KillerInstruction killer = new KillerInstruction(new ArrayList<X86Instruction>());

    List<IInstruction> path = new ArrayList<IInstruction>();
    path.add(killer);
    addX86Instruction(path, "x.dll:0x01 pop eax ebx -> ecx edx");
    path.add(killer);
    addX86Instruction(path, "x.dll:0x02 pop eax ebx ecx -> ecx eax");
    path.add(killer);
    addX86Instruction(path, "x.dll:0x03 pop eax ebx -> ecx eax");
    path.add(killer);
    addX86Instruction(path, "x.dll:0x04 pop esi edi -> ebp esp");
    path.add(killer);
    addX86Instruction(path, "x.dll:0x05 pop eax ebx esi -> ebp esp");
    path.add(killer);

    Map<String, String> reds = InstructionUtils.computeRedundantVars(path);

    assert 5 == reds.size() : reds;
    assertRedundants(reds, "bv:0x01:eax", "bv:0x02:eax");
    assertRedundants(reds, "bv:0x01:ebx", "bv:0x02:ebx", "bv:0x03:ebx", "bv:0x05:ebx");
    assertRedundants(reds, "bv:0x04:esi", "bv:0x05:esi");
  }

  public static void testComputeRedundantVars6() {

    // Should be like previous test, but no redundant ebx or eax variables.
    List<X86Instruction> instrsForKiller = new ArrayList<X86Instruction>();
    X86Instruction i = X86Instruction.parseInstruction("x.dll:0x00 pop eax ebx -> ebx ax");
    instrsForKiller.add(i);
    i = X86Instruction.parseInstruction("x.dll:0x00 pop eax ebx -> eax");
    instrsForKiller.add(i);
    KillerInstruction killer = new KillerInstruction(instrsForKiller);

    List<IInstruction> path = new ArrayList<IInstruction>();
    path.add(killer);
    addX86Instruction(path, "x.dll:0x01 pop eax ebx -> ecx edx");
    path.add(killer);
    addX86Instruction(path, "x.dll:0x02 pop eax ebx ecx -> ecx eax");
    path.add(killer);
    addX86Instruction(path, "x.dll:0x03 pop eax ebx -> ecx eax");
    path.add(killer);
    addX86Instruction(path, "x.dll:0x04 pop esi edi -> ebp esp");
    path.add(killer);
    addX86Instruction(path, "x.dll:0x05 pop eax ebx esi -> ebp esp");
    path.add(killer);

    Map<String, String> reds = InstructionUtils.computeRedundantVars(path);

    assert 1 == reds.size() : reds;
    assertRedundants(reds, "bv:0x04:esi", "bv:0x05:esi");
  }

  // A kill of any memor location makes any deref var non-redundant.
  public static void testComputeRedundantVars7() {
    List<IInstruction> path = new ArrayList<IInstruction>();
    addX86Instruction(path, "x.dll:0x01 pop eax [4+ebx] -> ecx");
    addX86Instruction(path, "x.dll:0x02 pop esi edi -> [0+ecx]");
    addX86Instruction(path, "x.dll:0x03 push [4+ebx] ebx ->  ebx");

    Map<String, String> reds = InstructionUtils.computeRedundantVars(path);

    assert 1 == reds.size() : reds;
    assertRedundants(reds, "bv:0x01:[4+ebx]", "bv:0x03:[4+ebx]");
  }

  public static void testComputeRedundantVars8() {
    List<IInstruction> path = new ArrayList<IInstruction>();
    addX86Instruction(path, "x.dll:0x01 pop eax [4+ebx] -> ecx");
    addX86Instruction(path, "x.dll:0x02 pop esi edi -> ebx");
    addX86Instruction(path, "x.dll:0x03 push [4+ebx] ebx ->  ebx");

    Map<String, String> reds = InstructionUtils.computeRedundantVars(path);

    assert 0 == reds.size() : reds;
  }

  private static void addX86Instruction(List<IInstruction> path, String string) {
    X86Instruction i = X86Instruction.parseInstruction(string);
    path.add(i);
  }
}
