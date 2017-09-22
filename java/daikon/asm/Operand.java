package daikon.asm;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/*>>>
import org.checkerframework.checker.regex.qual.*;
*/

/** Utility methods for operands of instructions. */
public class Operand {

  private static Set<String> registers8Bit;

  static {
    registers8Bit = new LinkedHashSet<String>();
    registers8Bit.add("ah");
    registers8Bit.add("bh");
    registers8Bit.add("ch");
    registers8Bit.add("dh");
    registers8Bit.add("al");
    registers8Bit.add("bl");
    registers8Bit.add("cl");
    registers8Bit.add("dl");
  }

  static boolean is8BitReg(String s) {
    return registers8Bit.contains(s);
  }

  private static Set<String> registers16Bit;

  static {
    registers16Bit = new LinkedHashSet<String>();
    registers16Bit.add("ax");
    registers16Bit.add("bx");
    registers16Bit.add("cx");
    registers16Bit.add("dx");
    registers16Bit.add("si");
    registers16Bit.add("di");
    registers16Bit.add("sp");
    registers16Bit.add("bp");
  }

  static boolean is16BitReg(String s) {
    return registers16Bit.contains(s);
  }

  private static Set<String> registers32Bit;
  private static Pattern registers32BitRegExp;

  static {
    registers32Bit = new LinkedHashSet<String>();
    StringBuilder b = new StringBuilder();
    registers32Bit.add("eax");
    b.append("eax");
    registers32Bit.add("ebx");
    b.append("|ebx");
    registers32Bit.add("ecx");
    b.append("|ecx");
    registers32Bit.add("edx");
    b.append("|edx");
    registers32Bit.add("esi");
    b.append("|esi");
    registers32Bit.add("edi");
    b.append("|edi");
    registers32Bit.add("esp");
    b.append("|esp");
    registers32Bit.add("ebp");
    b.append("|ebp");

    @SuppressWarnings("regex") // StringBuilder
    /*@Regex*/ String regex = b.toString();
    registers32BitRegExp = Pattern.compile(regex);
  }

  static boolean isExtendedReg(String s) {
    return registers32Bit.contains(s);
  }

  private static Set<String> registersFPU;

  static {
    registersFPU = new LinkedHashSet<String>();
    registersFPU.add("st0");
    registersFPU.add("st1");
    registersFPU.add("st2");
    registersFPU.add("st3");
    registersFPU.add("st4");
    registersFPU.add("st5");
    registersFPU.add("st6");
    registersFPU.add("st7");
  }

  static boolean isFPUReg(String s) {
    return registersFPU.contains(s);
  }

  static boolean isRegister(String s) {
    if (is8BitReg(s)) return true;
    if (is16BitReg(s)) return true;
    if (isExtendedReg(s)) return true;
    if (isFPUReg(s)) return true;
    return false;
  }

  static List<String> getExtendedRegisters(String expr) {
    Matcher m = registers32BitRegExp.matcher(expr);
    List<String> result = new ArrayList<String>();
    while (m.find()) {
      result.add(m.group());
    }
    return result;
  }

  static boolean isConstant(String s) {
    return (s.startsWith("$"));
  }

  public static boolean isDeref(String s) {
    return (s.startsWith("[") && s.endsWith("]"));
  }

  // Looks something like [32991844+]
  static boolean isDerefdNumber(String s) {

    if (!isDeref(s)) return false;

    String withoutBrackets = s.substring(1, s.length() - 1);
    // TODO a regexp would be more succint.
    for (int i = 0; i < withoutBrackets.length(); i++) {
      char c = withoutBrackets.charAt(i);
      if ((Character.isDigit(c)) || (c == '+')) {
        continue;
      }
      return false;
    }
    return true;
  }
}
