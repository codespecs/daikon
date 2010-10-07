package MapQuick;

import junit.framework.*;

public class PS6PublicTest
  extends TestSuite
{
  public PS6PublicTest()
  {
    this("Problem Set 6 Public Test");
  }

  public PS6PublicTest(String s)
  {
    super(s);
  }

  public static Test suite()
  {
    TestSuite suite = new TestSuite();

    suite.addTest(new PS6TestCase("testLoadDatabase"));

    suite.addTest(new PS6TestCase(ValidateQueries.test52WauwinetTo64Wauwinet,
				  PS6TestCase.MODE_PROGRAMMATIC_ADT));
    suite.addTest(new PS6TestCase(ValidateQueries.test32WauwinetTo64Wauwinet,
				  PS6TestCase.MODE_PROGRAMMATIC_ADT));
    suite.addTest(new PS6TestCase(ValidateQueries.test44MillbrookTo200Madaket,
				  PS6TestCase.MODE_PROGRAMMATIC_ADT));
    suite.addTest(new PS6TestCase(ValidateQueries.test111SomersetTo48EelPoint,
				  PS6TestCase.MODE_PROGRAMMATIC_ADT));

    suite.addTest(new PS6TestCase(ValidateQueries.testBadZip,
				  PS6TestCase.MODE_PROGRAMMATIC_ADT));
    suite.addTest(new PS6TestCase(ValidateQueries.testBadStreet,
				  PS6TestCase.MODE_PROGRAMMATIC_ADT));
    suite.addTest(new PS6TestCase(ValidateQueries.testBadNum,
				  PS6TestCase.MODE_PROGRAMMATIC_ADT));

    suite.addTest(new PS6TestCase(ValidateQueries.test111SomersetTo48EelPoint,
				  PS6TestCase.MODE_PROGRAMMATIC_STRING));
    suite.addTest(new PS6TestCase(ValidateQueries.testBadZip,
				  PS6TestCase.MODE_PROGRAMMATIC_STRING));

    return suite;
  }

}
