//
// This is an implementation of the ContextLinker interface, and is the method
// of communicating with the database from the Daikon Context GUI.  Its
// implementation uses ContextServer as the backend database.
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
import java.applet.Applet;
import java.awt.*;
import java.io.*;
import java.util.*;

// This is a class built for the GUI to talk to the database, and allows
// the extensibility of allowing different types of databases or different
// ways of communicating. Right now the setiterator and the getdata functions
// are seperated to give the user more flexibility, because someone might
// want to set it, but not necessarily get the data right away, etc...
public class ContextMiddle implements ContextLinker
{
	// This will iterate through a list of invariants
	// that the server end has.
	Iterator iter;

	// this will iterate through a list of classes in
	// the database
	Iterator class_iter;

	// This will iterate through a list of methods that
	// for a given class
	Iterator meth_iter;

	// This will iterate through a list of variables
	// for a given class and method
	Iterator var_iter;

	// This will iterate through a list of program points
	// for a given class, method/variable.
	Iterator pt_iter;

	// Object for the invariant server
	ContextServer ds;

	// The constructor starts up the database
	public ContextMiddle()
	{
		ds = new ContextServer();
	}

	// Trys to clean up the database as best as possible
	public void destroyDatabase()
	{
		ds.clear();
		ds = null;
	}

	// Adds a new file to the database, basically just a pass through
	// function to the database
	public String addFile(String filename)
	{
		return(ds.addNewFile(filename));
	}

	// This sets the iterator for getting invariants based on a certain
	// criteria, meaning class, method, variable, or program point. The
	// returning of the string was orginally used for debugging on
	// the console, however now it is really just ignored.
	public String setIterator(String class_name, String method_name, String var_name, String place_name)
	{
		String display_string = "";
		String args[] = null;

		if (class_name != null)
			display_string = "Class: " + class_name + "\n";

		if (method_name != null)
			display_string += "Method: " + method_name + "\n";

		if (var_name != null)
			display_string += "Variable: " + var_name + "\n";

		if (place_name != null)
			display_string += "Place: " + place_name + "\n";

		display_string += "====================\n";

		iter = ds.getIterator(class_name, method_name, var_name, place_name);

		return display_string;
	}

	// This gets the invariants to send to the GUI as one large
	// string, so the GUI will have to parse it.
	public String getInvariants()
	{
		String return_string = "";

		if (iter == null)
			return null;

		while(iter.hasNext())
			return_string += (String)iter.next() + "\n";

		return return_string;
	}

	// This will get one invariant at a time so the GUI side does not
	// have to parse the data. Either way of getting invariants has
	// disadvantages and advantages. For right now, I am mostly using
	// this one.
	public String getIndividualInvariants()
	{
		String return_string = "";

		if (iter == null)
			return null;

		if(iter.hasNext())
			return_string = (String)iter.next();
		else
			return null;

		return return_string;
	}

	// Sets iterator that will get method invariants.
	public void setMethodIterator(String class_name)
	{
		meth_iter = ds.getMethods(class_name);
	}

	// This iterates through a list of methods, one at a time.
	// Does not do anything if nothing more to iterate over.
	public String getMethods()
	{
		if (meth_iter.hasNext())
			return (String)meth_iter.next();

		meth_iter = null;
		return null;
	}

	// Sets the iterator for variables based on a class and method.
	public void setVariableIterator(String class_name, String method_name)
	{
		var_iter = ds.getVariables(class_name, method_name);

		if (var_iter == null)
			System.err.println("Setting the variable iterator failed");
	}

	// This gets the variables one at a time, defined by the set
	// variable iterator function.
	public String getVariables()
	{
		if (var_iter == null)
		{
			System.err.println("Null error!");
			return null;
		}

		if (var_iter.hasNext())
			return var_iter.next().toString();

		var_iter = null;
		return null;
	}

	// This sets the iterator to get a list of program points based on
	// class name, method name and variable name.
	public void setPlaceIterator(String class_name, String method_name, String variable_name)
	{
		pt_iter = ds.getPlaces(class_name, method_name, variable_name);

	}

	// This gets the program points one at a time, defined by the set
	// place iterator function.
	public String getPlaces()
	{
		if (pt_iter.hasNext())
			return pt_iter.next().toString();

		pt_iter = null;
		return null;
	}

	// This sets the iterator to get the list of classes.
	public void setClassIterator()
	{
		class_iter = ds.getClasses();
	}

	// This gets the classes one at a time, defined by the set
	// class iterator function.
	public String getClasses()
	{
		if (class_iter.hasNext())
			return (String)class_iter.next();

		class_iter = null;

		return null;
	}

	// Calls the database function to re read in the data
	// from the files.
	public void rebuild()
	{
		ds.rebuild();
	}

	// Calls the database function to clear the data out.
	public void clear()
	{
		ds.clear();
	}
}



/*
 * Local Variables:
 * c-basic-offset:	8
 * End:
 */
