package daikon.test;

import junit.framework.*;
import daikon.*;

public class ProglangTypeTester extends TestCase {

  private static ProglangType CHAR = ProglangType.parse("char");

  public static void main(String[] args) {
    junit.textui.TestRunner.run(new TestSuite(ProglangTypeTester.class));
  }

  public ProglangTypeTester(String name) {
    super(name);
  }

  public void testIsIntegral() {
    if (Daikon.compare_byte_char_to_integer) {
      Assert.assertEquals(true, CHAR.isIntegral());
    } else {
      Assert.assertEquals(false, CHAR.isIntegral());
    }
  }

}
