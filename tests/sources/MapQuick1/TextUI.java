package MapQuick1;

import MapQuick.*;
import java.io.*;
import java.util.*;

public class TextUI {
  
  public static void main(String[] args) 
  {
    /** effects:
     *  A user runs TextUI by typing the following command at the command
     *   promp:
     *
     *      java ps6.TextUI database-directory <zipcode1, ..., zipcodeN>
     *
     *  where:
     * database-directory: the program loads all the valid TigerDatabase
     *                     files from the specified directory with the
     *                     following extension: .zip
     * <zipcode1, ..., zipcodeN>:
     *                     the program filters out all the streetSegments
     *                     that are not in one of the givenzipcodes from
     *                     the database files
     *                     If no zipcode is given as an argument, there is
     *                     no filtering
     *  unless: the attempt to load fails (no .zip file in the specified
     *          directory, or one of the .zip files is not a valid
     *          TigerDatabase)
     *  => prints:
     *               Database error
     *
     *  The program then enters an interactive loop:
     *  -  it prompts the user for a starting number
     *  - it prompts the user for a starting street
     *    unless: starting number is 0
     *            => the program exits
     *  - it prompts the user for a starting street
     *  - it prompts the user for a starting zipcode
     *  - it prompts the user for a destination number
     *  - it prompts the user for a destination street
     *    unless: destination number is 0
     *            => the program exits
     *  - it prompts the user for a destination zipcode
     *  - it outputs a direction in the format specified in
     *    ElementaryRoute.directions(heading)
     *    unless: the starting zipcode doesn't appear in the database
     *            => prints:
     *                        No such zipcode: <i>number street zipcode </i>
     *
     *    unless: the starting street doesn't appear in the database
     *            => prints:
     *                        No such street: <i>number street zipcode </i>
     *
     *    unless: the starting number doesn't appear in the database
     *            => prints:
     *                        No such number: <i>number street zipcode </i>
     *
     *    unless: the destination zipcode doesn't appear in the database
     *            => prints:
     *                        No such zipcode: <i>number street zipcode </i>
     *
     *    unless: the destination street doesn't appear in the database
     *            => prints:
     *                        No such street: <i>number street zipcode </i>
     *
     *    unless: the destination number doesn't appear in the database
     *            => prints:
     *                        No such number: <i>number street zipcode </i>
     *
     */
    
    //Vector to store the zipcodes given as arguments
    Vector zc = new Vector(); 
    BufferedReader In = new BufferedReader(new InputStreamReader(System.in));
    DirectionsFinder df = null;
    final String askForStartingNum = "starting number? ";
    final String askForStartingStreet = "starting street? ";
    final String askForStartingZc = "starting zipcode? ";
    final String askForDestinationNum = "destination number? ";
    final String askForDestinationStreet = "destination street? ";
    final String askForDestinationZc = "destination zipcode? ";

	
    // check that the user has specified the databse directory
    if(args.length == 0){
      System.err.println("error, wrong number of arguments:\n Java ps6.TextUI database-directory <zipcode1> ... <zipcodeN>");  
    }
    else{

      // e.g. TextUI /mit/6.170/tigerdb/small --dump Hale
      String dumpPrefix = null;
      int i = 1;
      if (args.length >= 3 && args[i].equals("--dump")) {
	dumpPrefix = args[2];
	i = 3;
      }

      // add the zipcodes to zc
      for(; i<args.length; i++)
	zc.add(args[i]);
      try{
	df = DirectionsFinder.getDirectionsFinder(args[0],zc,dumpPrefix);
      }catch(InvalidDatabaseException e){
	System.out.println("Database Error");
	System.exit(-1);
      }

	
      boolean stepAsk = true;

      
      while(stepAsk){
	
	// Strings to store the user input
	String snInput, ssInput, szInput;
	String dnInput, dsInput, dzInput;

	// 1. ask for starting number
	
	System.out.print(askForStartingNum);	
	try{
	  snInput = In.readLine();
	  //check to make sure that the starting number is not 0
	  try{
	    Integer sni = new Integer(snInput);
	    if(sni.intValue() == 0){
	      stepAsk = false;
	      break;
	    }
	  }catch(NumberFormatException e){
	    // will be handled later in getDirections
	  }

	  // 2. ask for starting street
	  
	  System.out.print(askForStartingStreet);	
	  ssInput = In.readLine();
	
	  // 3. ask for starting zipcode
	  
	  System.out.print(askForStartingZc);	
	  szInput = In.readLine();

	  // 4. ask for destination number
	
	  System.out.print(askForDestinationNum);	
	  
	  dnInput = In.readLine();
	  //check to make sure that the starting number is not 0
	  try{
	    Integer dni = new Integer(dnInput);
	    if(dni.intValue() == 0){
	      stepAsk = false;
	      break;
	    }
	  }catch(NumberFormatException e){
	    // will be handled later in getDirections()
	  }
	  
	  // 5. ask for destination street
	  
	  System.out.print(askForDestinationStreet);	
	  dsInput = In.readLine();
	  
	  // 6. ask for destination zipcode
	  
	  System.out.print(askForDestinationZc);	
	  dzInput = In.readLine();
	    
	  // 7. Output the directions or error messages
	  
	  
	  String directions = df.getDirections(snInput,ssInput,szInput,                                                       dnInput,dsInput,dzInput);
	  System.out.print(directions);
	  
	  continue;
	
	}catch(IOException e){
	  System.err.println("there was an I/O problem while reading your input. Please try again");
	  
	}
      }
    }
  }
}
