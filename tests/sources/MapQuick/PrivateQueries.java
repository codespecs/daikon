package MapQuick;

/**
 * PrivateQueries contains static fields which hold candidate test
 * queries
 */
public final class PrivateQueries
{
  
  public static final TestRecord test52WauwinetTo64Wauwinet = 
    TestRecord.makeDirections(new Address(52, "Wauwinet Rd", "02554"),
			      new Address(64, "Wauwinet Rd", "02554"),
			      new String[] {
				"Turn left onto Wauwinet Rd and go 0.2 miles.",
				"64 Wauwinet Rd 02554 is on your left",
			      },
			      0.15474957);
  
  public static final TestRecord test32WauwinetTo64Wauwinet = 
    TestRecord.makeDirections(new Address(32, "Wauwinet Rd", "02554"),
			      new Address(64, "Wauwinet Rd", "02554"),
			      new String[] {
				"Turn left onto Wauwinet Rd and go 0.7 miles.",
				"64 Wauwinet Rd 02554 is on your left",
			      },
			      0.66725347);
  
  public static final TestRecord test44MillbrookTo200Madaket = 
    TestRecord.makeDirections(new Address(44, "Millbrook Rd", "02554"),
			      new Address(200, "Madaket Rd", "02554"),
			      new String[] {
				"Turn left onto Millbrook Rd and go 0.7 miles.",
				"Turn slight left onto Milbrook Rd and go 0.2 miles.",
				"Turn left onto Madaket Rd and go 3.1 miles.",
				"200 Madaket Rd 02554 is on your left",
			      },
			      4.02566169);

  public static final TestRecord test111SomersetTo48EelPoint = 
    TestRecord.makeDirections(new Address(111, "Somerset Rd", "02554"),
			      new Address(48, "Eel Point Rd", "02554"),
			      new String[] {
				"Turn left onto Somerset Rd and go 0.4 miles.",
				"Turn left onto Sumerset Ln and go 0.5 miles.",
				"Turn left onto Hummock Pond Rd and go 0.2 miles.",
				"Turn right onto Millbrook Rd and go 1.0 miles.",
				"Turn slight left onto Milbrook Rd and go 0.2 miles.",
				"Turn left onto Madaket Rd and go 0.5 miles.",
				"Turn slight right onto Eel Point Rd and go 1.0 miles.",
				"48 Eel Point Rd 02554 is on your left",
			      },
			      3.79086397);


  // added queries
  public static final TestRecord test1ParsonLnTo3ParsonLn = 
    TestRecord.makeDirections(new Address(1,"Parson Ln", "02564"),
			      new Address(3,"Parson Ln", "02564"),
			      new String[] {
				"Turn left onto Parson Ln and go 0.2 miles.",
				"3 Parson Ln 02564 is on your left"
			      },
			      0.24407356);
  

  public static final TestRecord test3ParsonLnTo1ParsonLn = 
    TestRecord.makeDirections(new Address(3,"Parson Ln", "02564"),
			      new Address(1,"Parson Ln", "02564"),
			      new String[] {
				"Turn right onto Parson Ln and go 0.2 miles.",
				"1 Parson Ln 02564 is on your right"
			      },
			      0.24407356);

    public static final TestRecord test1ParsonLnTo4ParsonLn = 
    TestRecord.makeDirections(new Address(1,"Parson Ln", "02564"),
			      new Address(4,"Parson Ln", "02564"),
			      new String[] {
				"Turn left onto Parson Ln and go 0.2 miles.",
				"4 Parson Ln 02564 is on your right"
			      },
			      0.24407356);
  

  public static final TestRecord test4ParsonLnTo1ParsonLn = 
    TestRecord.makeDirections(new Address(4,"Parson Ln", "02564"),
			      new Address(1,"Parson Ln", "02564"),
			      new String[] {
				"Turn left onto Parson Ln and go 0.2 miles.",
				"1 Parson Ln 02564 is on your right"
			      },
			      0.24407356);


  public static final TestRecord test3GladlandsAveTo29NonatumAve =
    TestRecord.makeDirections(new Address(3,"Gladlands Ave","02554"),
			      new Address(29,"Nonatum Ave","02584"),
			      new String[] {
				"Turn left onto Gladlands Ave and go 0.1 miles.",
				"Turn right onto Surfside Rd and go 0.4 miles.",
				"Turn left onto Nonatum Ave and go 0.1 miles.",
				"29 Nonatum Ave 02584 is on your right"
			      },0.58573671);


  // 46 Stupid Rd 01111
  public static final TestRecord testBadNumber46WauwinetRd02554 =
    TestRecord.makeBadAddress(new Address(42, "Wauwinet Rd", "02554"),
			      new Address(46, "Wauwinet Rd", "02554"),
			      "No such number: 46 Wauwinet Rd 02554");
  
  
  public static final TestRecord testBadStreet42StupidRd02554 =
    TestRecord.makeBadAddress(new Address(42, "Wauwinet Rd", "02554"),
			      new Address(42, "Stupid Rd", "02554"),
			      "No such street: 42 Stupid Rd 02554");


  public static final TestRecord testBadZip42WauwinetRd01111 =
    TestRecord.makeBadAddress(new Address(42, "Wauwinet Rd", "02554"),
			      new Address(42, "Wauwinet Rd", "01111"),
			      "No such zipcode: 42 Wauwinet Rd 01111");

  // 133 Somerset St 02556
  public static final TestRecord testBadNumber133SomersetRd02554 =
    TestRecord.makeBadAddress(new Address(111, "Somerset Rd", "02554"),
			      new Address(133, "Somerset Rd", "02554"),
			      "No such number: 133 Somerset Rd 02554");
  
  
  public static final TestRecord testBadStreet111SomersetSt02554 =
    TestRecord.makeBadAddress(new Address(111, "Somerset Rd", "02554"),
			      new Address(111, "Somerset St", "02554"),
			      "No such street: 111 Somerset St 02554");


  public static final TestRecord testBadZip111SomersetRd02556 =
    TestRecord.makeBadAddress(new Address(111, "Somerset Rd", "02554"),
			      new Address(111, "Somerset Rd", "02556"),
			      "No such zipcode: 111 Somerset Rd 02556");

  // 1003 Sankaty 02555
  
  public static final TestRecord testBadNumber1003SankatyRd02554 =
    TestRecord.makeBadAddress(new Address(103, "Sankati Ave", "02554"),
			      new Address(1003, "Sankati Ave", "02554"),
			      "No such number: 1003 Sankati Ave 02554");
  
  
  public static final TestRecord testBadStreet103Sankati02554 =
    TestRecord.makeBadAddress(new Address(103, "Sankati Ave", "02554"),
			      new Address(103, "Sankati", "02554"),
			      "No such street: 103 Sankati 02554");


  public static final TestRecord testBadZip103Sankati02555 =
    TestRecord.makeBadAddress(new Address(103, "Sankati Ave", "02554"),
			      new Address(103, "Sankati Ave", "02555"),
			      "No such zipcode: 103 Sankati Ave 02555");

  // 66666 AllYourBase Rd 66666
  public static final TestRecord testBadZip =
    TestRecord.makeBadAddress(new Address(52, "Wauwinet Rd", "66666"),
			      new Address(52, "Wauwinet Rd", "66666"),
			      "No such zipcode: 52 Wauwinet Rd 66666");
  
  public static final TestRecord testBadStreet =
    TestRecord.makeBadAddress(new Address(52, "AllYourBase Rd", "02554"),
			      new Address(52, "AllYourBase Rd", "02554"),
			      "No such street: 52 AllYourBase Rd 02554");
  
  public static final TestRecord testBadNum = 
    TestRecord.makeBadAddress(new Address(66666, "Wauwinet Rd", "02554"),
			      new Address(66666, "Wauwinet Rd", "02554"),
			      "No such number: 66666 Wauwinet Rd 02554");

}











