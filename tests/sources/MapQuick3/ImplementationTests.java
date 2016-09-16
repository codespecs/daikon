package MapQuick3;

import junit.framework.*;


/**
 * ImplementationTest is a simple test suite to test the
 * implementation of each problem set.  You do not need to modify this
 * file for problem set 2.
 */
public final class ImplementationTests extends TestSuite {

    public ImplementationTests() {
        this("Problem Set 2 ImplementationTests");
    }

    public ImplementationTests(String name) {
        super(name);
    }

    public static Test suite() {
        return new ImplementationTests();
    }
}
