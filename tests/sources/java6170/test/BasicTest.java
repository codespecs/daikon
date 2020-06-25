package java6170.test;

import junit.framework.*;
import org.junit.Test;

public class BasicTest extends TestCase {
  public BasicTest(String name) {
    super(name);
  }

  @Test
  public void testSuccess() {
    System.out.println("This test should pass");
  }

  @Test
  public void testFailure() {
    assertTrue("This test should fail", false);
  }

  @Test
  public void testTimeout() {
    while (true) ;
  }

  @Test
  public void testExit() {
    System.exit(1);
  }

  @Test
  public void testError() {
    throw new Error("Doh!");
  }

  @Test
  public void testException() throws Exception {
    throw new Exception("I take exception to that!");
  }
}
