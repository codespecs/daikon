//
// This program converts a Daikon invariant file (.inv file) into a Daikon
// Context GUI invariant file (.dci file). This can be used without the GUI
// or within the GUI.
//
// To compile this file, the daikon classes/jar file must be within the
// classpath.
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
import daikon.*;
import daikon.inv.*;
import utilMDE.*;
import java.util.*;
import java.io.*;

public class ConvertInvToDci
{
	public static String error_msg = null;

	public static void main(String args[])
	{
	        daikon.Logger.setupLogs (daikon.Logger.INFO);
		String return_val = converting(args);
		if (return_val == null)
			System.err.println(error_msg);
		else
			System.out.println("File has been converted!");
	}

	// Build caching/ouput file name.
	public static String getOutputFilename(String inputfilename)
	{
		String output_file;
		File fo = new File(inputfilename);
		output_file = "." + fo.getName() + ".dci";

		String parent_path = fo.getParent();
		if (parent_path != null)
			output_file = parent_path + "/" + output_file;

		return output_file;
	}

	// Returns a message indicating successful completion, or an error message.
	public static String converting(String args[])
	{
		if ((args.length > 2) || (args.length < 1))
		{
			error_msg = "Incorrect usage: convert <input>";
			return null;
		}

		String output_file;

		if (args.length == 2)
		{
			output_file = args[1];
		}
		else
		{
			output_file = getOutputFilename(args[0]);
		}

		String return_state = null;
		BufferedWriter out_info = null;

		try {


		out_info = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(output_file)));
		PptMap ppt = FileIO.read_serialized_pptmap(new File(args[0]),
							   true // use saved config
							   );
		for (Iterator iter = ppt.nameStringSet().iterator(); iter.hasNext(); ) {
			out_info.write("==========");
			out_info.newLine();

			String name = (String) iter.next();
			PptName pptName = new PptName(name);

			String meth_args = null;
			int place_find = name.indexOf('(');
			if (place_find != -1)
			{
				meth_args = name.substring(place_find);
				place_find = meth_args.indexOf(')');
				if (place_find != -1)
					meth_args = meth_args.substring(0, place_find+1);
				else
					meth_args = null;
			}

			if (meth_args != null)
			{
				if (meth_args.equals("()"))
					meth_args = null;
				else
				{
					try {

					meth_args = UtilMDE.arglistFromJvm(meth_args);
					meth_args = meth_args.substring(1);
					meth_args = meth_args.substring(0, meth_args.length() - 1);

					} catch (Error e) {
						error_msg = e.toString();
						return null;
					}


					StringTokenizer tokens = new StringTokenizer(meth_args, ", ");
					meth_args = "";
					while(tokens.hasMoreTokens())
					{
						String strip_arg = tokens.nextToken();
						strip_arg = ContextUtils.clearOutInfo(strip_arg, ".");
						/*place_find = strip_arg.lastIndexOf(".");
						if (place_find != -1)
							strip_arg = strip_arg.substring(place_find+1);*/

						strip_arg = ContextUtils.clearOutInfo(strip_arg, "$");
						/*place_find = strip_arg.lastIndexOf("$");
						if (place_find != -1)
							strip_arg = strip_arg.substring(place_find+1);*/

						meth_args += strip_arg + " ";
					}
					meth_args = meth_args.trim();
				}
			}

			// Extracts out the method, class and point
			String className = pptName.getShortClassName();
			String methodName = pptName.getShortMethodName();
			String point = pptName.getPoint();
			if (methodName == null) {
				methodName = pptName.getPoint();
				point = null;
			}
			if (methodName.equals("<init>"))
				methodName = "Constructor";
			//System.out.println("className is " + className + ", MethodName is " + methodName);

			className = ContextUtils.clearOutInfo(className, "$");
			methodName = ContextUtils.clearOutInfo(methodName, "$");

			out_info.write(className);
			out_info.newLine();
			out_info.write(methodName);
			if (meth_args != null)
				out_info.write(" " + meth_args);
			out_info.newLine();
			if (point != null) {
				if (point.startsWith("EXIT"))
					point = "EXIT";
				else if (point.startsWith("ENTER"))
					point = "ENTER";
				out_info.write(point);
				out_info.newLine();
			}

			// Extracts out the invariants for the method, class, and point
			PptTopLevel pptvalues = (PptTopLevel)ppt.get(pptName);
			for (Iterator iter1 = pptvalues.invariants_vector().iterator(); iter1.hasNext(); ) {
				Invariant inv = (Invariant)iter1.next();
				//System.out.println(inv.format());
				out_info.write(inv.format());
				out_info.newLine();
			}
			out_info.flush();
		}

		//System.out.println("");
		}
		catch (IOException e)
		{
			return_state = e.getMessage();
		}
		catch (Error e)
		{
			return_state = e.getMessage();
		}
		finally
		{
			try {

			out_info.flush();
			out_info.close();

			}
			catch(IOException e) {
			}

			if (return_state != null)
			{
				error_msg = return_state;
				return null;
			}

			return output_file;
		}
	}

}



/*
 * Local Variables:
 * c-basic-offset:	8
 * End:
 */
