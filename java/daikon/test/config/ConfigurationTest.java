package daikon.test.config;

import daikon.config.*;
import junit.framework.*;
import org.junit.Test;

public class ConfigurationTest {

  // Mostly useful to check that our resource files are bound correctly
  @Test
  public void testGetInstance() {
    // Executed for side effect.
    Configuration.getInstance();
  }
}
