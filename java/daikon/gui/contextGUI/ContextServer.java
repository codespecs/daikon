//
// This is code for the database of invariants that are
// queried and displayed in the Daikon Context GUI.
//

// ************************************************************************
// Copyright © 2001 The Regents of the University of California.
// All rights reserved.
// This code is part of the UCSD Daikon Context GUI and has been authored by
// Gregory Jay and William Griswold.
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following
// conditions are met:
//
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
// 3. Neither the name of the University nor the names of its contributors
//    may be used to endorse or promote products derived from this software
//    without specific prior written permission.
//
// For permission to use this software for commercial purposes, contact
// William G. Griswold (wgg@cs.ucsd.edu) or send U.S. Mail to:
// William G. Griswold
// Department of Computer Science and Engineering, 0114
// University of California, San Diego
// La Jolla, CA 92093-0114
// ************************************************************************

package daikon.gui.contextGUI;
import java.io.*;
import java.util.*;

// This is a database for the invariants. To store the invariants, methods,
// and classes in the database, I use 2 layers of hashmaps pointing to a
// list of invariants. The top most hashmap is map with the classes as
// keys and the following layer is then a hashmap with methods as keys.
// Currently, to parse through the invariants of the method takes
// iterating through a list, which for most cases should be fine since
// the user will probably will not narrow the search on the invariants.
// However, if this becomes problematic, I may want to change it to a
// faster system.
public class ContextServer
{
	private HashMap db;
	private TreeSet filelist;

	// Starts up GUI with no files loaded initially
	// Creates new hashmap for the invariants to go in the database
	// Create a tree to store the list of files read into the database
	public ContextServer()
	{
		db = new HashMap();
		filelist = new TreeSet();
	}

	// Does same thing as no argument construct pluse it adds the given
	// file into the database.
	public ContextServer(String path) throws FileNotFoundException
	{
		db = new HashMap();
		filelist = new TreeSet();

		addNewFile(path);
	}

	// Returns an iterator of the hashmap of classes.
	public Iterator getClasses()
	{
		return (db.keySet().iterator());
	}

	// Returns an iterator for the hashmap that the entry in the class
	// hashmap contains.
	public Iterator getMethods(String class_name)
	{
		if (db.containsKey(class_name))
			return ((HashMap)db.get(class_name)).keySet().iterator();

		return null;
	}

	// Extracts out the context data from our data structure using the method name and
	// a hash map for the class. Then it is stored in another datastructure and we find
	// the certain data from that structure defined by the ContextualFieldRetriever.
	// When we find that data we add to another structure and return that data.
	private Iterator getInformationIterator(String class_name, String method_name, ContextualFieldRetriever cfr)
	{
		HashMap temp_map = (HashMap)db.get(class_name);

		LinkedList list_temp;

		// Get a list containing all the invariant objects we want for all the methods
		// specified, where null means all.
		if (method_name != null)
			list_temp = ((ContextMethodData)temp_map.get(method_name)).getList();
		else
		{
			list_temp = new LinkedList();
			Iterator temp_iter = temp_map.keySet().iterator();

			while (temp_iter.hasNext())
			{
				ContextMethodData dmdtemp = (ContextMethodData)temp_map.get(temp_iter.next());
				list_temp.addAll(dmdtemp.getList());
			}
		}

		TreeSet temp_vect = new TreeSet();

		for (int i = 0; i < list_temp.size(); i++)
		{
			if (list_temp.get(i) instanceof String)
				continue;

			ContextInvData dtemp = (ContextInvData)(list_temp.get(i));
			String chk_string;

			if ((chk_string = cfr.getTheField(dtemp)) != null)
				temp_vect.add(chk_string);
		}

		return temp_vect.iterator();
	}

	// Gets an iterator to a list of variables for a given class and method. This
	// basically builds or extracts the invariant objects for a method, or if no methods
	// specified then all the methods' invariant objects for the class are added. These
	// are then put into a tree, so that when we return an iterator to the tree, it
	// returns the variables in alphabetical order.
	public Iterator getVariables(String class_name, String method_name)
	{
		return getInformationIterator(class_name, method_name, new VariableNameRetriever());
	}

	// We are trying to get a set of all the unique program points we have invariants for
	// based on a given class, method, and/or variable name. Then we return an iterator for
	// the set which the class above will iterate through. By using a tree set, the program
	// points will be in alphabetical order.
	public Iterator getPlaces(String class_name, String method_name, String variable_name)
	{
		return getInformationIterator(class_name, method_name, new PPTNameRetriever(variable_name));
	}

	// Checking if the program point is global or has been specified.
	private String getPPTName(ContextInvData cd)
	{
		String temp_ppt = cd.getPlace();
		if (temp_ppt == null)
			return "GLOBAL";

		return temp_ppt;
	}

	// method defined that the user only has to specify the class
	// and it uses the default values for the rest.
	public Iterator getIterator(String iter_class)
	{
		return (getIterator(iter_class, null, null, null));
	}

	// method defined that the user only has to specify the class/method
	// and it uses the default values for the rest.
	public Iterator getIterator(String iter_class, String iter_meth)
	{
		return (getIterator(iter_class, iter_meth, null, null));
	}

	// method defined that the user only has to specify the class, method
	// and variable, and it uses the default values for the rest.
	public Iterator getIterator(String iter_class, String iter_meth, String iter_var)
	{
		return (getIterator(iter_class, iter_meth, iter_var, null));
	}

	// Returns an iterator to a list of the invariants, specified by
	// the arguments below.
	public Iterator getIterator(String iter_class, String iter_meth, String iter_var, String iter_place)
	{
		Iterator temp = new ContextIterator(iter_class, iter_meth, iter_var, iter_place);
		return (temp);
	}

	// Clears the hashmap of classes and then iterates through the
	// tree of files and adds it to the new database.
	public void rebuild()
	{
		TreeSet tree_temp = filelist;
		Iterator iter_temp = tree_temp.iterator();

		db = new HashMap();
		filelist = new TreeSet();

		while (iter_temp.hasNext())
			addNewFile((String)iter_temp.next());
	}

	// Clears the invariants out of the database and clears the list
	// of files in the database.
	public void clear()
	{
		db = new HashMap();
		filelist = new TreeSet();
	}

	// This adds a invariants from a file to the database. There could be more invariants
	// for a class already in the database added to it. We may at one point just want only
	// one set of invariants for a class.
	public String addNewFile(String pathname)
	{
		String new_filename = null;
		File cache_file = null;
		File fo_inv = new File(pathname);

		if (!fo_inv.exists())
			return "File " + fo_inv.getName() + " does not exist!";

		if (pathname.endsWith(".inv") || pathname.endsWith(".inv.gz"))
		{
			new_filename = ConvertInvToDci.getOutputFilename(pathname);

			String [] pass_array = new String[2];
			pass_array[0] = pathname;
			pass_array[1] = new_filename;

			cache_file = new File(new_filename);
			if (!cache_file.exists())
				ConvertInvToDci.converting(pass_array);
			else
			{
				if (fo_inv.lastModified() != cache_file.lastModified())
				{
					cache_file.delete();
					new_filename = ConvertInvToDci.converting(pass_array);

					cache_file = null;
				}
			}
		}
		else if (pathname.endsWith(".dci"))
		{
			new_filename = pathname;
		}

		if (new_filename == null)
			return "Format Error with " + fo_inv.getName();

		// Don't want to add duplicate file so we check the file name
		if (filelist.contains(pathname))
			return "File " + fo_inv.getName() + " Already in Database";
		else
			filelist.add(pathname);

		BufferedReader in = null;

		// opening up the file
		try
		{
			in = new BufferedReader(new FileReader(new_filename));
		}
		catch (FileNotFoundException fe)
		{
			// return "File " + pathname + " Not Found";
			return "Error in reading cache file for " + fo_inv.getName();
		}

		// line buffer for reading from file.
		String line = null;

		// temp variable for holding the position
		// of things found in strings.
		int place_find = 0;

		try {

		// Ignoring lines until we find the first set of invariants, designated by a line
		// of equals signs.
		while (!in.readLine().startsWith("===")) ;

		while (in.ready())
		{
			String inv_class = in.readLine();

			HashMap class_hash = null;
			ContextMethodData meth_class = null;

			String inv_method = in.readLine();
			String inv_ppt;

			// Checks if the invariant is a global invariant for
			// the class/object. Otherwise we have to extract the class
			// name.
			if (inv_method.startsWith("OBJECT"))
			{
				inv_method = "<OBJECT>";
				inv_ppt = null;
			}
			else if (inv_method.startsWith("CLASS"))
			{
				inv_method = "<CLASS>";
				inv_ppt = null;
			}
			else
			{
				inv_ppt = in.readLine();
			}

			// Pulls up the class we are going to store the new invariants for.
			// If the class doesnt exist, then we are going to a new hashmap for the
			// class.
			if (db.containsKey(inv_class))
			{
				class_hash = (HashMap)db.get(inv_class);
                		if (class_hash == null)
                    			System.err.println("Class Null Problem when trying to add new file!");
            		}
			else
			{
				class_hash = new HashMap();
				db.put(inv_class, class_hash);
			}

			// Check if the method maps to the list of invariants and method data.
			// If it doesnt, we create a new one.
			if (class_hash.containsKey(inv_method))
			{
				meth_class = (ContextMethodData)class_hash.get(inv_method);
				if (meth_class == null)
					System.err.println("Method mapped to empty value!");
			}
			else
			{
				meth_class = new ContextMethodData(null, inv_method);
				class_hash.put(inv_method, meth_class);
			}

			// After we get the header information for the group of invariants, we keep
			// getting invariants until we hit the next group of invariants.
			while (in.ready())
			{
				line = in.readLine();

				// We hit the next group of invariants
				if (line.startsWith("==="))
					break;

				place_find = line.indexOf(' ');

				if (place_find == -1)
					throw new IOException("Incorrect File Format!");

				String var = line.substring(0, place_find);

				// extracts the variable name from the invariant line
				while (var.startsWith("("))
				{
					var = var.substring(1);

					// We do - 2 because we want to chop off the trailing
					// parenthesis and a string is like an array of chars.
					if (var.endsWith(")"))
						var = var.substring(0, var.length() - 2);
				}

				meth_class.add(new ContextInvData(var, line, inv_ppt));

			}
			meth_class = null;
			class_hash = null;
		}

		} catch (IOException ioe){
			return("Error reading in file: " + ioe.toString());
		}
		catch (NullPointerException npe)
		{
			return("Error in reading in file: " + npe.toString());
		}
		finally
		{
			try {
				in.close();
			} catch (IOException eie) {}

			if (cache_file == null)
			{
				cache_file = new File(new_filename);
			}

			if (cache_file.setLastModified(fo_inv.lastModified()) == false)
			{
				return "Cannot change file properties in directory!";
			}

			return "File " + fo_inv.getName() + " Added Successfully";
		}
	}

	// Function not used anymore, but left in if I ever brought this file format back.
	public String addFiletoData(String pathname) // throws FileNotFoundException
	{
		if (filelist.contains(pathname))
			return "File Already in Database";
		else
			filelist.add(pathname);

		BufferedReader in;

		try
		{
			in = new BufferedReader(new FileReader(pathname));
		}
		catch (FileNotFoundException fe)
		{
			return "File Not Found";
		}

		String line = null;
		String inv_method = null;
		String inv_place = null;
		String inv_class = null;
		String inv_params[] = null;
		String param_list = null;
		String inv_return = null;

		int place_find = 0;

		ContextMethodData meth_class = null;
		HashMap class_hash = null;

		try
		{
			// Find where we want to start looking for invariants
			while ((line = in.readLine()) != null)
			{
				if (line.startsWith("=="))
					break;
			}

			// Search for invariants and invariant headers
			while (!(line = in.readLine()).equals("Exiting"))
			{
				// Invariant with no data
				if (line.startsWith("[No"))
					place_find = -1;
				else
					place_find = line.indexOf(":::");

				// Invariant header found
				if (place_find != -1)
				{
					if (line.indexOf("CLASS") != -1)
					{
						inv_place = null;
						inv_method = "<CLASS>";
						inv_class = line.substring(0, place_find);
						inv_params = null;
						inv_return = null;

						if (db.containsKey(inv_class))
							class_hash = (HashMap)db.get(inv_class);
						else
						{
							class_hash = new HashMap();
							db.put(inv_class, class_hash);
						}

						if (class_hash.containsKey(inv_method))
							meth_class = (ContextMethodData)db.get(inv_method);
						else
						{
							meth_class = new ContextMethodData(null, inv_method);
							class_hash.put(inv_method, meth_class);
						}
					}
					else if (line.indexOf("OBJECT") != -1)
					{
						inv_place = null;
						inv_method = "<OBJECT>";
						inv_class = line.substring(0, place_find);
						inv_params = null;
						inv_return = null;

						if (db.containsKey(inv_class))
							class_hash = (HashMap)db.get(inv_class);
						else
						{
							class_hash = new HashMap();
							db.put(inv_class, class_hash);
						}

						if (class_hash.containsKey(inv_method))
							meth_class = (ContextMethodData)db.get(inv_method);
						else
						{
							meth_class = new ContextMethodData(null, inv_method);
							class_hash.put(inv_method, meth_class);
						}
					}
					else
					{
						if (line.indexOf("EXIT") != -1)
							inv_place = "EXIT";
						else if (line.indexOf("ENTER") != -1)
							inv_place = "ENTER";

						line = line.substring(0, place_find);
						place_find = line.indexOf('.');

						if (place_find == -1)
							throw new IOException("Incorrect File Format!");

						inv_class = line.substring(0, place_find);
						line = line.substring(place_find + 1);

						place_find = line.indexOf('(');

						if (place_find == -1)
							throw new IOException("Incorrect File Format!");

						inv_method = line.substring(0, place_find);
						line = line.substring(place_find + 1);

						place_find = line.indexOf(')');

						if (place_find == -1)
							throw new IOException("Incorrect File Format!");


						if (place_find > 0)
						{

							param_list = line.substring(0, place_find);
							line = line.substring(place_find + 1);

							int param_start = 0;
							int num_param = 0;
							int i = 0;

							place_find = param_list.indexOf(',', param_start);

							// Count the number of parameters
							while (place_find != -1)
							{
								num_param++;
								param_start = place_find + 1;
								place_find = param_list.indexOf(',', param_start);
							}

							inv_params = new String[num_param + 1];

							for (i = 0; i < num_param; i++)
							{
								place_find = param_list.indexOf(',');
								inv_params[i] = param_list.substring(0, place_find);
								param_list = param_list.substring(place_find + 1);
							}

							inv_params[i] = param_list;


						}
						else
						{
							line = line.substring(place_find + 1);
							inv_params = null;
						}

						inv_return = null;
					}



					/*********************************************
					System.out.println("Class: " + inv_class);
					System.out.println("Method: " + inv_method);
					System.out.println("Return: " + inv_return);
					System.out.println("Place: " + inv_place);


					if (inv_params != null)
					{
						for (int i = 0; i < inv_params.length; i++)
							System.out.println("Argument #" + i + ": " + inv_params[i]);
					}
					else
						System.out.println("No Arguments!");
					System.out.println("");
					**********************************************/

					if (db.containsKey(inv_class))
						class_hash = (HashMap)db.get(inv_class);
					else
					{
						class_hash = new HashMap();
						db.put(inv_class, class_hash);
					}

					String meth_temp = inv_method;

					if (inv_params != null)
					{
						for (int i = 0; i < inv_params.length; i++)
							meth_temp += " " + inv_params[i];
					}

					if (class_hash.containsKey(meth_temp))
						meth_class = (ContextMethodData)class_hash.get(meth_temp);
					else
					{
						meth_class = new ContextMethodData(inv_return, meth_temp);
						class_hash.put(meth_temp, meth_class);
					}

				}
				else if (line.startsWith("==="))
				{
					if ((class_hash != null) && (inv_class != null))
						db.put(inv_class, class_hash);

					inv_class = null;
					inv_method = null;
					inv_return = null;
					inv_place = null;
					inv_params = null;
				}
				else if (!line.startsWith("Exiting"))
				{
					place_find = line.indexOf(' ');
					if (place_find == -1)
						throw new IOException("Incorrect File Format!");

					String var = line.substring(0, place_find);

					meth_class.add(new ContextInvData(var, line, inv_place));
				}
			}
		}
		catch (IOException e)
		{
			System.err.println(e);
		}
		return "File Added Successfully";
	}

	// This is a class that will contain information for the method, which is the list of
	// invariants for the method and information such as the return type. The return type is
	// included because that field may be used at a later time. For the arguments to the method,
	// I just put them as Strings with the method in this format: method arg1 arg2 ..., which
	// is easy to use String Tokenizer to extract. For certain parts of this program, it was
	// easiest to get the method information if i just added the method name as the first element
	// of the list and then i can use instanceof String to find, because the invariants should
	// be in a ContextInvData object.
	private class ContextMethodData
	{
		private String return_type;
		LinkedList data_list;

		// Constructor to set the information about the method. This is the only time
		// which this information can be set.
		public ContextMethodData(String new_return_type, String new_methodname)
		{
			return_type = new_return_type;
			data_list = new LinkedList();
			data_list.add(new_methodname);
		}

		// Adds and invariant to the list for the method in this object.
		public void add(ContextInvData list_item)
		{
			data_list.add(list_item);
		}

		public String getReturnType()
		{
			return return_type;
		}

		// retrieves the lists of invariants
		public LinkedList getList()
		{
			return data_list;
		}

		public String getMethodName()
		{
			return (String)data_list.get(0);
		}
	}

	// This class will contain information about a specific invariant for a method, such as the variable,
	// the porgram point, and the actual invariant itself. I keep the variable, that way I reduce the time
	// searching for a particular invariant takes, even though it takes up more space. Otherwise you would,
	// have to do extra string functions every time.
	private class ContextInvData
	{
		private String var; // varible name for invariant
		private String invariant;
		private String place; // program point

		// Constructor, for which the invariant information can only be set.
		ContextInvData(String new_var, String new_invariant, String new_place)
		{

			place = new_place;
			var = new_var;
			invariant = new_invariant;
		}

		public String getPlace()
		{
			return place;
		}

		public String getVariable()
		{
			return var;
		}

		public String getInvariant()
		{
			return invariant;
		}
	}

	private class ContextIterator implements Iterator
	{
		private String iter_class;
		private String iter_var;
		private String iter_place;
		private String iter_meth;
		private LinkedList iter_list;
		private int iter_position;
		private boolean valid_iter;
		private boolean wild_card;

		// This is the constructor for the iterator, that will gather a list of possible invariants
		// based on the criteria passed to the iterator. Then it will move the iterator position to
		// the first invariant in the list that meets all the requirements. Unfortunately, when
		// the list is built, it only adds invariants based on class and method. So there is some
		// time wasted skipping unwanted invariants.
		public ContextIterator(String new_iter_class, String new_iter_meth, String new_iter_var, String new_iter_place)
		{
			iter_class = new_iter_class;
			iter_meth = new_iter_meth;
			iter_var = new_iter_var;
			iter_place = new_iter_place;
			wild_card = false;

			HashMap tempmap = null;
			ContextMethodData tempdmd = null;
			ContextInvData dtemp = null;

			valid_iter = true;

			// If the class doesnt exist, we want to identify that there is no invariants
			// for it when the user asks for the next().
			if (!db.containsKey(iter_class))
				valid_iter = false;
			else
			{
				// get the map for the class
				tempmap = (HashMap)db.get(iter_class);

				// If the method has been specified, we just have to get the list for the
				// method, but if the method has not been specified, we must get all the
				// lists for all the methods of the class.
				if (iter_meth != null)
				{
					String temp_meth = iter_meth;

					if (tempmap.containsKey(temp_meth))
					{
						tempdmd = (ContextMethodData)tempmap.get(temp_meth);
						iter_list = tempdmd.getList();

						if ((iter_place != null) && (iter_place.equals("GLOBAL")))
							iter_place = null;
					}
					else
						valid_iter = false;
				}
				else
				{
					iter_list = new LinkedList();

					// Did the user really just reqeust Global invariants?
					if ((iter_place != null) && (iter_place.equals("GLOBAL")))
					{
						if (tempmap.containsKey("<OBJECT>"))
						{
							tempdmd = (ContextMethodData)tempmap.get("<OBJECT>");
							iter_list.addAll(tempdmd.getList());
						}

						if (tempmap.containsKey("<CLASS>"))
						{
							tempdmd = (ContextMethodData)tempmap.get("<CLASS>");
							iter_list.addAll(tempdmd.getList());
						}
						iter_place = null;
					}
					else {

						Iterator iter_temp = tempmap.keySet().iterator();

						while (iter_temp.hasNext())
						{
							tempdmd = (ContextMethodData)tempmap.get(iter_temp.next());
							iter_list.addAll(tempdmd.getList());
						}
					}
				}
			}

			iter_position = 0;

			if (valid_iter)
				moveToNext();
		}

		// Checks to see if we are within the bounds of the list and we have a class set
		// meaning we have
		public boolean hasNext()
		{
			if ((iter_class == null) || (!valid_iter) || (iter_position >= iter_list.size()))
				return false;
			return true;
		}

		// If we are stopped at an element before the end, we want to return it. If this happens
		// to be a string, we send it back else if invariant we send the invariant information.
		// Then we want to stop at the next invariant.
		public Object next()
		{
			if (!hasNext())
				return null;
			Object otemp = iter_list.get(iter_position);

			String stemp = "";
			ContextInvData dtemp = null;

			if (otemp instanceof String )
			{
				stemp = "\n" + otemp.toString() + ":";
			}
			else
			{
				dtemp = (ContextInvData)otemp;
				stemp = dtemp.getPlace() + "\n";
				stemp += "\t" + dtemp.getInvariant();
			}

			iter_position++;

			moveToNext();

			return stemp;
		}


		// This moves the iterator to the next element in the list of invariants.
		// Strings mean that we are at the start of a new section(method) of invariants,
		// and we want to stop looking still next time. Otherwise we keep searching until
		// we find an invariant that means the user's requirements.
		private void moveToNext()
		{
			ContextInvData dtemp = null;

			while (iter_position < iter_list.size())
			{
				if (iter_list.get(iter_position) instanceof String)
					break;

				dtemp = (ContextInvData)iter_list.get(iter_position);

				if ((iter_var != null) && (iter_place != null))
				{
					if (dtemp.getPlace() != null)
						if ((iter_var.equals(dtemp.getVariable())) && (iter_place.toUpperCase().equals(dtemp.getPlace().toUpperCase())))
							break;
				}
				else if (iter_var != null)
				{
					if (iter_var.equals(dtemp.getVariable()))
						break;
				}
				else if (iter_place != null)
				{
					if (dtemp.getPlace() != null)
						if (iter_place.toUpperCase().equals(dtemp.getPlace().toUpperCase()))
							break;

				}
				else
					break;

				iter_position++;
			}
		}

		// we don't want to remove anything from the invariant lists
		public void remove()
		{
			throw new UnsupportedOperationException();
		}
	}

	private interface ContextualFieldRetriever
	{
		public String getTheField(ContextInvData cd);
	}

	private class PPTNameRetriever implements ContextualFieldRetriever
	{
		private String variable_name;

		public PPTNameRetriever(String new_variable)
		{
			variable_name = new_variable;
		}

		public String getTheField(ContextInvData cd)
		{
			if (variable_name == null)
				return getPPTName(cd);
			else
			{
				if (cd.getVariable().equals(variable_name))
					return getPPTName(cd);
			}

			return null;
		}
	}

	private class VariableNameRetriever implements ContextualFieldRetriever
	{
		public String getTheField(ContextInvData cd)
		{
				return cd.getVariable();
		}
	}
}




/*
 * Local Variables:
 * c-basic-offset:	8
 * End:
 */
