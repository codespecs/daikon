//
// This code is a wrapper/adapter interface for communicating with a database.
// This allows for different implementations of the database or different ways
// of getting to the database, such as a remote database (e.g., serve as a proxy).
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

public interface ContextLinker
{
	public String addFile(String filename);
	public String getInvariants();
	public String getIndividualInvariants();
	public String setIterator(String class_name, String method_name, String var_name, String place_name);
	public void setClassIterator();
	public String getClasses();
	public void setMethodIterator(String class_name);
	public String getMethods();
	public void setVariableIterator(String class_name, String method_name);
	public String getVariables();
	public void setPlaceIterator(String class_name, String method_name, String place_name);
	public String getPlaces();
	public void rebuild();
	public void clear();
	public void destroyDatabase();
	public void setCFile(boolean new_CFILE);
}
