// Utilities for the Daikon Context GUI, mainly for parsing string when converting/
// adding files to the db.

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

class ContextUtils
{
	// This weeds out unwanted parts of the string for the display that are marked by
	// some search string.
	public static String clearOutInfo(String fix_txt, String search_string)
	{
		int find_position = fix_txt.lastIndexOf(search_string);

		// returns shortened string
		if (find_position != -1)
			return fix_txt.substring(find_position + 1);

		// didn't find string to fix
		return fix_txt;
	}
}
