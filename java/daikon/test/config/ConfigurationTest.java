package daikon.test.config;

import junit.framework.*;
import daikon.config.*;

public class ConfigurationTest
  extends TestCase
{

  public static void main(String[] args) {
    junit.textui.TestRunner.run(new TestSuite(ConfigurationTest.class));
  }

  public ConfigurationTest(String name) {
    super(name);
  }

  // Mostly useful to check that our resource files are bound correctly
  public void testGetInstance() {
    Configuration gotten = Configuration.getInstance();
  }

}
