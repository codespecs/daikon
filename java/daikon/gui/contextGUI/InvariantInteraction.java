//
// This is a class of static methods that the beanshell for Emacs uses
// to communicate with the Daikon Context GUI and the invariant database.
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

public class InvariantInteraction
{
	// Object of the gui, that will be used to perform actions on it
	private static ContextGUI dg = null;

	// Used to determine if the gui is visible. Not necessarily the program has
	// started or stopped. This way we can keep the gui object and not lose the
	// database and yet prevent the coder from trying to use the gui when it is
	// not visible.
	private static boolean gui_visible = false;

	/*
	 * These variables set a buffer to check if the GUI needs refreshing
	 * because emacs will keep sending information however long the timer
	 * is set for.
	 */
	private static String last_class = null;
	private static String last_method = null;
	private static String last_var = null;
	private static String last_point = null;

	/*
	 * This function will start the gui and load files from a specified path
	 * into the GUI.
	 */
	public static void startGui(String path)
	{
		if (!gui_visible)
		{
			gui_visible = true;

			// Checks if the Context GUI has been started and stopped.
			boolean dbprot = (dg == null);

			if (dbprot)
				dg = new ContextGUI();

			dg.startGui();

			if (dbprot)
			{
				// Loads up all the .dci files in the given path
				File file_dir = new File(path);
				File filelist[] = file_dir.listFiles(new FileInvarFilter());
				if (filelist != null)
				{
					for (int i = 0; i < filelist.length; i++)
					{
						String pathName = filelist[i].getAbsolutePath();
						dg.addFile(pathName);
					}
				}
			}
		}
	}

	/*
	 * This function resets the buffers of the GUI because the database is
	 * changing.
	 */
	private static void resetVars()
	{
		last_class = null;
		last_method = null;
		last_var = null;
		last_point = null;
	}

	// This function turns off the GUI, which should be turned off when the
	// mode is turned off in Emacs.
	public static void endGui()
	{
		gui_visible = false;
		dg.endGui();
		//dg = null;
		resetVars();
	}

	// This function is called from Emacs to change the current display on the GUI. If the name
	// of the class is the same as the method, then it will change the method name to say Constructor.
	// For reporting methods and its arguments, they should be sent to the method_name string in this
	// format: method_name arg1 arg2 ...
	public static void input(String class_name, String method_name, String var_name, String point_name)
	{
		if (gui_visible)
		{
			if (method_name != null)
			    method_name = method_name.trim();

			// Checking for constructor
			if ((method_name != null) && (!method_name.equals("null")))
			{
				StringTokenizer tokens = new StringTokenizer(method_name);
				String temp = ""; // temp string to hold the method_name w/o the args.
				if (tokens.hasMoreTokens())
					temp = tokens.nextToken();

				// Rebuidling the method_name with constructor instead.
				if (temp.equals(class_name))
				{
					method_name = "Constructor";
					while (tokens.hasMoreTokens())
						method_name += " " + tokens.nextToken();
				}
			}

			// Checking buffer string for a duplicate request
			boolean same_last_class = sameString(last_class, class_name);
			boolean same_last_method = sameString(last_method, method_name);
			boolean same_last_var = sameString(last_var, var_name);
			boolean same_last_point = sameString(last_point, point_name);

			// Loading up new request and setting buffer strings.
			if (!(same_last_class && same_last_method && same_last_var && same_last_point))
			{
				dg.inputChange(class_name, method_name, var_name, point_name);
				last_class = class_name;
				last_method = method_name;
				last_var = var_name;
				last_point = point_name;
			}
		}
	}

	/** Return true if both arguments are null, or if they are equal Strings. */
	private static boolean sameString(String a, String b)
	{
		return ((a == b) || ((a != null) && a.equals(b)));
	}

	// This passes through all the layers to get to the database to add a file
	// to the database.
	public static void addFile(String filename)
	{
		if (gui_visible)
		{
			dg.addFile(filename);
			resetVars();
		}
	}

	// This passes through all the layers to get to the database to reload
	// all the files in the database.
	public void rebuild()
	{
		if (gui_visible)
		{
		    	dg.rebuild();
			resetVars();
		}
	}

	// This passes through the layers of the program to tell the database to
	// clear all the data currently being stored.
	public void clear()
	{
		if (gui_visible)
		{
			dg.clear();
			resetVars();
		}
	}

}

// This class will filter out files with only the .dci extension so as
// to only load these up into the gui automatically.
// ^^ This comment is on crack.
class FileInvarFilter implements FilenameFilter
{
	public boolean accept(File dir, String name)
	{
		if (name.endsWith(".inv") || name.endsWith(".inv.gz"))
			return true;

		return false;
	}
}
