package MapQuick;

/**
 * ValidateQueries contains static fields which hold candidate test
 * queries
 */
public final class ValidateQueries
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
