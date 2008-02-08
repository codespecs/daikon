package daikon.test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import junit.framework.TestCase;
import asm.IInstruction;
import asm.InstructionUtils;
import asm.KillerInstruction;
import asm.X86Instruction;

public class InstructionUtilsTest extends TestCase {

  public static void testComputeRedundantVars1() {
    List<IInstruction> path = new ArrayList<IInstruction>();
    addX86Instruction(path, "x.dll:0x01 pop eax ebx -> ecx edx");
    
    Map<String, Set<String>> reds = InstructionUtils.computeRedundantVars(path);
    
    assertEquals(reds.toString(), 0, reds.size());
    
    addX86Instruction(path, "x.dll:0x02 pop eax ebx -> ecx edx");
    
    reds = InstructionUtils.computeRedundantVars(path);
    
    assertEquals(reds.toString(), 2, reds.size());
    assertRedundants(reds, "0x01:eax", "0x02:eax");
    assertRedundants(reds, "0x01:ebx", "0x02:ebx");
  }

  private static void assertRedundants(Map<String, Set<String>> redMap,
      String leader, String... redundantVars) {
    if (redundantVars.length == 0) return;
    Set<String> redExpected = new LinkedHashSet<String>(Arrays.asList(redundantVars));
    Set<String> redActual = redMap.get(leader);
    assertEquals(leader, redExpected, redActual);
  }

  public static void testComputeRedundantVars1a() {
    List<IInstruction> path = new ArrayList<IInstruction>();
    addX86Instruction(path, "x.dll:0x01 pop eax ebx -> ecx edx");
    addX86Instruction(path, "x.dll:0x02 pop ecx edx");

    
    Map<String, Set<String>> reds = InstructionUtils.computeRedundantVars(path);
    
    assertEquals(reds.toString(), 0, reds.size());
  }
  
  public static void testComputeRedundantVars1b() {
    List<IInstruction> path = new ArrayList<IInstruction>();
    addX86Instruction(path, "x.dll:0x01 pop eax ebx -> ecx edx");
    addX86Instruction(path, "x.dll:0x02 pop ebx eax");

    
    Map<String, Set<String>> reds = InstructionUtils.computeRedundantVars(path);
    
    assertEquals(reds.toString(), 2, reds.size());
    assertRedundants(reds, "0x01:eax", "0x02:eax");
    assertRedundants(reds, "0x01:ebx", "0x02:ebx");
  }
  
  public static void testComputeRedundantVars2() {
    List<IInstruction> path = new ArrayList<IInstruction>();
    addX86Instruction(path, "x.dll:0x01 pop eax ebx -> ecx edx");
    addX86Instruction(path, "x.dll:0x02 pop eax ebx -> ecx edx");
    addX86Instruction(path, "x.dll:0x03 pop eax ebx -> eax ebx");

    
    Map<String, Set<String>> reds = InstructionUtils.computeRedundantVars(path);
    
    assertEquals(reds.toString(), 2, reds.size());
    assertRedundants(reds, "0x01:eax", "0x02:eax", "0x03:eax");
    assertRedundants(reds, "0x01:ebx", "0x02:ebx", "0x03:ebx");
  }

  public static void testComputeRedundantVars3() {
    List<IInstruction> path = new ArrayList<IInstruction>();
    addX86Instruction(path, "x.dll:0x01 pop eax ebx -> ecx edx");
    addX86Instruction(path, "x.dll:0x02 pop eax ebx ecx -> ecx eax");
    addX86Instruction(path, "x.dll:0x03 pop eax ebx -> ecx eax");
    addX86Instruction(path, "x.dll:0x04 pop exx eyx -> ezx eux");
    addX86Instruction(path, "x.dll:0x05 pop eax ebx exx -> ezx eux");

    
    Map<String, Set<String>> reds = InstructionUtils.computeRedundantVars(path);
    
    assertEquals(reds.toString(), 3, reds.size());
    assertRedundants(reds, "0x01:eax", "0x02:eax");
    assertRedundants(reds, "0x01:ebx", "0x02:ebx", "0x03:ebx", "0x05:ebx");
    assertRedundants(reds, "0x04:exx", "0x05:exx");
  }
  
  public static void testComputeRedundantVars4() {
    List<IInstruction> path = new ArrayList<IInstruction>();
    addX86Instruction(path, "x.dll:0x01 pop eax");
    addX86Instruction(path, "x.dll:0x02 pop eax");
    addX86Instruction(path, "x.dll:0x03 pop eax");
    addX86Instruction(path, "x.dll:0x04 pop exx");
    addX86Instruction(path, "x.dll:0x05 pop eax");

    
    Map<String, Set<String>> reds = InstructionUtils.computeRedundantVars(path);
    
    assertEquals(reds.toString(), 1, reds.size());
    assertRedundants(reds, "0x01:eax", "0x02:eax", "0x03:eax", "0x05:eax");
  }

  public static void testComputeRedundantVars5() {
    
    // This killer instruction should have no effect because it
    // kills variables that are not in any real instruction.
    List<IInstruction> instrsForKiller = new ArrayList<IInstruction>();
    addX86Instruction(instrsForKiller, "x.dll:0x00 pop eax ebx -> a3 a4");
    KillerInstruction killer = new KillerInstruction(instrsForKiller);
    
    List<IInstruction> path = new ArrayList<IInstruction>();
    path.add(killer);
    addX86Instruction(path, "x.dll:0x01 pop eax ebx -> ecx edx");
    path.add(killer);
    addX86Instruction(path, "x.dll:0x02 pop eax ebx ecx -> ecx eax");
    path.add(killer);
    addX86Instruction(path, "x.dll:0x03 pop eax ebx -> ecx eax");
    path.add(killer);
    addX86Instruction(path, "x.dll:0x04 pop exx eyx -> ezx eux");
    path.add(killer);
    addX86Instruction(path, "x.dll:0x05 pop eax ebx exx -> ezx eux");
    path.add(killer);

    
    Map<String, Set<String>> reds = InstructionUtils.computeRedundantVars(path);
    
    assertEquals(reds.toString(), 3, reds.size());
    assertRedundants(reds, "0x01:eax", "0x02:eax");
    assertRedundants(reds, "0x01:ebx", "0x02:ebx", "0x03:ebx", "0x05:ebx");
    assertRedundants(reds, "0x04:exx", "0x05:exx");
  }
  
 public static void testComputeRedundantVars6() {
    
    // Should be like previous test, but no redundant ebx or eax variables.
    List<IInstruction> instrsForKiller = new ArrayList<IInstruction>();
    addX86Instruction(instrsForKiller, "x.dll:0x00 pop eax ebx -> ebx a4");
    addX86Instruction(instrsForKiller, "x.dll:0x00 pop eax ebx -> eax");
    KillerInstruction killer = new KillerInstruction(instrsForKiller);
    
    List<IInstruction> path = new ArrayList<IInstruction>();
    path.add(killer);
    addX86Instruction(path, "x.dll:0x01 pop eax ebx -> ecx edx");
    path.add(killer);
    addX86Instruction(path, "x.dll:0x02 pop eax ebx ecx -> ecx eax");
    path.add(killer);
    addX86Instruction(path, "x.dll:0x03 pop eax ebx -> ecx eax");
    path.add(killer);
    addX86Instruction(path, "x.dll:0x04 pop exx eyx -> ezx eux");
    path.add(killer);
    addX86Instruction(path, "x.dll:0x05 pop eax ebx exx -> ezx eux");
    path.add(killer);

    
    Map<String, Set<String>> reds = InstructionUtils.computeRedundantVars(path);
    
    assertEquals(reds.toString(), 1, reds.size());
    assertRedundants(reds, "0x04:exx", "0x05:exx");
  }
  
  private static void addX86Instruction(List<IInstruction> path, String string) {
    X86Instruction i = X86Instruction.parseInstruction(string);
    path.add(i);
  }
  
}
