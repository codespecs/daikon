package MapQuick;

import junit.framework.*;

public class DirectionsFinderTest
  extends TestSuite
{
  public DirectionsFinderTest()
  {
    this("DirectionsFinder Tests");
  }

  public DirectionsFinderTest(String s)
  {
    super(s);

    // This was helpful for 6170 students, but just wastes time for
    // PAG users.
    // addTest(new PS6TestCase("testLoadDatabase"));

    addTest(new DFADTTestCase("SameSegment",
                              PrivateQueries.test52WauwinetTo64Wauwinet));
    addTest(new DFADTTestCase("SameStreet",
                              PrivateQueries.test32WauwinetTo64Wauwinet));
    addTest(new DFADTTestCase("TwoStreets",
                              PrivateQueries.test44MillbrookTo200Madaket));
    addTest(new DFADTTestCase("ManyStreets",
                              PrivateQueries.test111SomersetTo48EelPoint));

    addTest(new DFADTTestCase("BadZip", PrivateQueries.testBadZip));
    addTest(new DFADTTestCase("BadStreet", PrivateQueries.testBadStreet));
    addTest(new DFADTTestCase("BadNum", PrivateQueries.testBadNum));

    addTest(new DFStringTestCase("ManyStreets",
                                 PrivateQueries.test111SomersetTo48EelPoint));
    addTest(new DFStringTestCase("BadZip", PrivateQueries.testBadZip));
    addTest(new DFStringTestCase("BadStreet", PrivateQueries.testBadStreet));
    addTest(new DFStringTestCase("BadNum", PrivateQueries.testBadNum));

    addTest(new DFADTTestCase("SameSegLeftLeft",
                              PrivateQueries.test1ParsonLnTo3ParsonLn));
    addTest(new DFADTTestCase("SameSegRightRight",
                              PrivateQueries.test3ParsonLnTo1ParsonLn));
    addTest(new DFADTTestCase("SameSegLeftRight",
                              PrivateQueries.test1ParsonLnTo4ParsonLn));
    addTest(new DFADTTestCase("SameSegRightLeft",
                              PrivateQueries.test4ParsonLnTo1ParsonLn));
    addTest(new DFADTTestCase("AnotherWorking",
                              PrivateQueries.test3GladlandsAveTo29NonatumAve));

    addTest(new DFADTTestCase("BadNumTwo",
                              PrivateQueries.testBadNumber46WauwinetRd02554));
    addTest(new DFADTTestCase("BadStreetTwo",
                              PrivateQueries.testBadStreet42StupidRd02554));
    addTest(new DFADTTestCase("BadZipTwo",
                              PrivateQueries.testBadZip42WauwinetRd01111));

    addTest(new DFADTTestCase("BadNumThree",
                              PrivateQueries.testBadNumber133SomersetRd02554));
    addTest(new DFADTTestCase("BadStreetThree",
                              PrivateQueries.testBadStreet111SomersetSt02554));
    addTest(new DFADTTestCase("BadZipThree",
                              PrivateQueries.testBadZip111SomersetRd02556));

    addTest(new DFADTTestCase("BadNumFour",
                              PrivateQueries.testBadNumber1003SankatyRd02554));
    addTest(new DFADTTestCase("BadStreetFour",
                              PrivateQueries.testBadStreet103Sankati02554));
    addTest(new DFADTTestCase("BadZipFour",
                              PrivateQueries.testBadZip103Sankati02555));
  }

  // Added for PAG usability

  public static Test suite() { return new DirectionsFinderTest(); }

  public static void main(String[] args) {
    junit.textui.TestRunner.main(new String[] {
      "MapQuick.DirectionsFinderTest",
    });
  }

}
