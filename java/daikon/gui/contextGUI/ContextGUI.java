//
// This is the main entry point for the Daikon Context GUI.  It contains all
// the Java Swing code for the Context GUI, as well as all the needed out
// calls to a ContextMiddle instance to perform functions on the database
// of invariants.  It is also provides the ability to invoke the
// daikon.gui.contextGUI.ConvertInvToDci class in order to convert .inv
// (Daikon's standard invariant format) to .dci files (a compact format).
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
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.table.*;
import javax.swing.border.*;
import java.util.*;
import java.io.*;
import javax.swing.JTable;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;

public final class ContextGUI extends JApplet implements ActionListener
{
	// Container for the gui.
	private Container content;

	// Labels for the different things in the gui
	private JLabel jlclass;
	private JLabel jlmethod;
	private JLabel jlvariable;
	private JLabel jlplace;
	private JLabel jlinvars;

	// Labels for displaying the current status of the program
	// in between non visible actions
	private JLabel jlstatus;
	private JLabel jlstatus_msg;

	// Combo boxes used to choose the specifics of the invariants you
	// want to look at.
	private JComboBox jcclass;
	private JComboBox jcmeth;
	private JComboBox jcvariable;
	private JComboBox jcplace;

	// The table will be put on top of the scroll pane for which
	// display the invariants based on the search criterias.
	private JTable jtinvars;
	private JScrollPane scrollPane;

	// This is the model for how to display invariants in the table
	private MyTableModel mtm;

	// This is the object for communicating to the Database
	private ContextLinker dfc;

	// Used when we set the iterator and then used to check when we are getting
	// the data from the iterator to see if we need to worry about that column.
	private boolean no_method;
	private boolean no_place;
	private boolean no_variable;

	// This is the action protection variable, which basically means when
	// I change a combo box I do not want to set the change off as an
	// action that has been performed.
	private boolean action_pro = false;

	// The frame for the application
	private static JFrame app;

	// Contstructor that creates our connection to the database.
	public ContextGUI()
	{
		dfc = new ContextMiddle();
	}

	// This handles all the actions for the GUI
	public void actionPerformed(ActionEvent e)
	{
		// This string will get set with what get sets to the status message
		String display_string = "";

		// This is for a change in the class combo box
		// We turn off all further actions and update the other combo boxes
		// with the info for the new class and get the new invariants.
		// Then we restore actions for the GUI.
		if (e.getSource() == jcclass)
		{
			if (!action_pro)
			{
				action_pro = true;
				updateMethodCombo();
				updateVariablesCombo();
				updatePlacesCombo();
				go_invars();
				action_pro = false;
			}
		}
		// This is for a change in the method combo box
		// We turn off further actions and update the variables and places combo
		// and get invariants and then restore actions.
		else if (e.getSource() == jcmeth)
		{
			if (!action_pro)
			{
				action_pro = true;
				updateVariablesCombo();
				updatePlacesCombo();
				go_invars();
				action_pro = false;
			}
		}
		// This is for change in the variables combo box
		// We turn off further actions and update the places combo and get invariants
		// Then we return actions.
		else if (e.getSource() == jcvariable)
		{
			if (!action_pro)
			{
				action_pro = true;
				updatePlacesCombo();
				go_invars();
				action_pro = false;
			}
		}
		// This is for a change in the program point combo box
		// We turn off further actions until done updating and get the invariants.
		else if (e.getSource() == jcplace)
		{
			if (!action_pro)
			{
				action_pro = true;
				go_invars();
				action_pro = false;
			}
		}
		// This is when the Add File command is choosen from the menu.
		// This will pop up a file chooser box and add the file to the
		// the user selects into the database. If the user cancel the
		// file chooser, then we don't do anything.
		else if (e.getActionCommand().equals("Add File"))
		{
			JFileChooser chooser = new JFileChooser(new File("."));
			ExampleFileFilter filter = new ExampleFileFilter();
			//filter.addExtension("dci");
			filter.addExtension("inv");

			filter.setDescription("Invariant Files");
	    		chooser.setFileFilter(filter);
	    		int returnVal = chooser.showOpenDialog(this);

    			if(returnVal == JFileChooser.APPROVE_OPTION)
				addFile(chooser.getSelectedFile().getPath());

		}
		// This is when the Clear Database option is choosen from the menu.
		// This will shut off further actions clearing the database and then
		// the combo boxes of all content. Then we turn back on actions.
		else if (e.getActionCommand().equals("Clear File Database"))
		{
			action_pro = true;
			clear();
			updateClassCombo();
			updateMethodCombo();
			updateVariablesCombo();
			updatePlacesCombo();
			mtm.setDataVector(new String[0]);
			mtm.reDraw();
			action_pro = false;
		}
		// This is when the Refresh Database option is choosen from the menu.
		// This will shut off further actions clearing the database and then
		// rebuilding the combo box information. Then is turns back on further
		// actions.
		else if (e.getActionCommand().equals("Refresh File Database"))
		{
			action_pro = true;
			rebuild();
			updateClassCombo();
			updateMethodCombo();
			updateVariablesCombo();
			updatePlacesCombo();
			go_invars();
			action_pro = false;
		}
		// Quits the program if the user hits quit.
		else if (e.getActionCommand().equals("Quit"))
		{
			System.exit(0);
		}

		// Redraw changed components on screen.
		if (!action_pro)
		{
			validate();
			repaint();
		}
	}

	// This will set the iterator in the database with the data from the combo boxes and
	// and then call the getInvariants function to get the invariants and display them in
	// the table.
	private void go_invars()
	{
		String method_pass = jcmeth.getSelectedItem().toString();

		if (method_pass.equals("All"))
			method_pass = null;

		String variable_pass = jcvariable.getSelectedItem().toString();
		if (variable_pass.equals("All"))
			variable_pass = null;

		String place_pass = jcplace.getSelectedItem().toString();
		if (place_pass.equals("All"))
			place_pass = null;

		setIterator(jcclass.getSelectedItem().toString(), method_pass, variable_pass, place_pass);
		getInvariants(null);
	}

	// This function will update the list of classes in the class combo box.
	// This is only called when data in the database has been changed, such as
	// adding a file.
	private void updateClassCombo()
	{
		// We want to get the last selected item
		// before we remove anything.
		String last_item = jcclass.getSelectedItem().toString();

		// We remove everything.
		jcclass.removeAllItems();

		// Add all the classes to our vector using the iterator
		String classtemp;
		Vector classlist = new Vector();
		dfc.setClassIterator();
		while ((classtemp = dfc.getClasses()) != null)
			classlist.add(classtemp);

		// this is if the database does not have any classes
		if (classlist.size() == 0)
			classlist.add("None");

		boolean item_exists = false; // this is used to tell if the last item exists in the new list


		// This looks through each item in the vector and then if it was the last item, then
		// it sets a boolean value so as to set that as the selected item. Then it adds the item
		// to the combo box.
		for (int i = 0; i < classlist.size(); i++)
		{
			if (classlist.get(i).equals(last_item))
				item_exists = true;

			jcclass.addItem(classlist.get(i));
		}

		if (item_exists)
			jcclass.setSelectedItem(last_item);
	}

	// This function updates the combox box containing methods after a change to
	// the class combo box has been made.
	private void updateMethodCombo()
	{
		// This will remove the all the items in the method combo box, saving the
		// the last selected item first, and putting in the All option.
		String last_item = (String)(jcmeth.getSelectedItem());
		jcmeth.removeAllItems();
		jcmeth.addItem("All");


		// this only runs when class combo has a valid selection.
		if (!jcclass.getSelectedItem().toString().equals("None"))
		{
			dfc.setMethodIterator(jcclass.getSelectedItem().toString());

			// This will get our methods for the class in the vector and
			// then we will iterate through the vector checking to see if
			// it was the last item selected to keep smooth browsing for the
			// user. Then we add the item to the list.`
			Vector classlist = new Vector();
			String classtemp;

			while ((classtemp = dfc.getMethods()) != null)
				classlist.add(classtemp);

			// this is used to tell if the last item exists in the new list
			boolean item_exists = false;

			for (int i = 0; i < classlist.size(); i++)
			{
				if (classlist.get(i).equals(last_item))
					item_exists = true;

				jcmeth.addItem(classlist.get(i));
			}

			// this is if we have found the new item in the new list of methods.
			if (item_exists)
				jcmeth.setSelectedItem(last_item);
		}

	}

	// This function updates the combox box containing variables after a change to
	// the methods combo box or class has been made.
	private void updateVariablesCombo()
	{
		// This will remove the all the items in the variable combo box, saving the
		// the last selected item first, and putting in the All option.
		String last_item = jcvariable.getSelectedItem().toString();

		jcvariable.removeAllItems();
		jcvariable.addItem("All");


		if (!jcclass.getSelectedItem().toString().equals("None"))
		{
			// This gets the currently selected method name or if all
			// then we don't care about the method name and we are going
			// to send the iterator a null
			String method_pass = jcmeth.getSelectedItem().toString();
			if (method_pass.equals("All"))
				method_pass = null;

			dfc.setVariableIterator(jcclass.getSelectedItem().toString(), method_pass);

			// Stores the variables in a vector received from the iterator.
			Vector variablelist = new Vector();
			String classtemp;
			while ((classtemp = dfc.getVariables()) != null)
				variablelist.add(classtemp);

			// this is used to tell if the last item exists in the new list
			boolean item_exists = false;

			// We will iterate through the vector checking to see if
			// it was the last item selected to keep smooth browsing for the
			// user. Then we add the item to the list.
			for (int i = 0; i < variablelist.size(); i++)
			{
				if (variablelist.get(i).equals(last_item))
					item_exists = true;

				jcvariable.addItem(variablelist.get(i));
			}

			if (item_exists)
				jcvariable.setSelectedItem(last_item);
		}

	}

	// This function updates the combox box containing program points after a change to
	// the methods combo box, variables combo box, or class combo box has been made.
	private void updatePlacesCombo()
	{
		// This will remove the all the items in the places combo box, saving the
		// the last selected item first, and putting in the All option.
		String last_item = jcplace.getSelectedItem().toString();

		jcplace.removeAllItems();
		jcplace.addItem("All");


		if (!jcclass.getSelectedItem().toString().equals("None"))
		{
			// This gets the currently selected method name or if all
			// then we don't care about the method name and we are going
			// to send the iterator a null
			String method_pass = jcmeth.getSelectedItem().toString();
			if (method_pass.equals("All"))
				method_pass = null;

			// This gets the currently selected variable name or if all
			// then we don't care about the variable name and we are going
			// to send the iterator a null
			String variable_pass = jcvariable.getSelectedItem().toString();
			if (variable_pass.equals("All"))
				variable_pass = null;

			dfc.setPlaceIterator(jcclass.getSelectedItem().toString(), method_pass, variable_pass);

			Vector placelist = new Vector();
			String classtemp;

			while ((classtemp = dfc.getPlaces()) != null)
				placelist.add(classtemp);

			// this is used to tell fi the last item exists in the new list
			boolean item_exists = false;

			// We will iterate through the vector checking to see if
			// it was the last item selected to keep smooth browsing for the
			// user. Then we add the item to the list.
			for (int i = 0; i < placelist.size(); i++)
			{
				if (placelist.get(i).equals(last_item))
					item_exists = true;

				jcplace.addItem(placelist.get(i));
			}

			if (item_exists)
				jcplace.setSelectedItem(last_item);
		}

	}

	// This will start the program by itself creating an object of this class
	// and then start the GUI.
	public static void main(String args[])
	{
		ContextGUI dgui = new ContextGUI();

		dgui.startGui();
	}

	// Starts up the SWING Gui in a new Frame and sets it visible.
	public void startGui()
	{
		app = new JFrame();
		app.setSize(300,1000);

		app.addWindowListener(new CloseWindowExit());

		content = app.getContentPane();

		init();
		start();

		app.setVisible(true);
	}

	// This just hides the gui from the user.
	public void endGui()
	{
		//dfc.destroyDatabase();
		//dfc = null;
		app.setVisible(false);
		app.dispose();
		app = null;
	}

	// This loads up all the components of the GUI and adds the action listeners
	// and other characteristics to those components.
	public void init()
	{
		content.setLayout(new BorderLayout());

		// This sets up the menu for the GUI
		JMenuBar jmb = new JMenuBar();
		JMenu file = new JMenu("File");
		JMenuItem item = new JMenuItem("Add File");
		JMenuItem item3 = new JMenuItem("Clear File Database");
		JMenuItem item4 = new JMenuItem("Refresh File Database");
		JMenuItem item5 = new JMenuItem("Quit");
		file.add(item);
		file.addSeparator();
		file.add(item3);
		file.add(item4);
		file.addSeparator();
		file.add(item5);

		jmb.add(file);
		app.setJMenuBar(jmb);
		item.addActionListener(this);
		item3.addActionListener(this);
		item4.addActionListener(this);
		item5.addActionListener(this);

		// Defining the labels to go on the GUI
		jlclass = new JLabel("Class: ");
		jlmethod = new JLabel("Method/Globals: ");
		jlvariable = new JLabel("Variable:");
		jlplace = new JLabel("Point:");
		jlinvars = new JLabel("Invariants:");

		jlstatus = new JLabel("Status: ");
		jlstatus_msg = new JLabel("");

		// Gets the classes to go in the combo box or sets the default value
		Vector classlist = new Vector();
		dfc.setClassIterator();

		String classtemp;
		while ((classtemp = dfc.getClasses()) != null)
			classlist.add(classtemp);

		if (classlist.size() == 0)
			classlist.add("None");
		jcclass = new JComboBox(classlist);

		// Sets up the rest of the combo boxes based on the class combo box
		jcmeth = new JComboBox();
		updateMethodCombo();

		jcvariable = new JComboBox();
		jcvariable.addItem("All");

		jcplace = new JComboBox();
		jcplace.addItem("All");

		// Sets up the table of invariants and gets the invariants.
		String data[][] = {};
		String columnNames[] = {"Method", "Pt.", "Invar."};

		mtm = new MyTableModel(data,columnNames);
		//jtinvars = new JTable(mtm);
		TableSorter sorter = new TableSorter(mtm);
		jtinvars = new JTable(sorter);
		sorter.addMouseListenerToHeaderInTable(jtinvars);

		scrollPane = new JScrollPane(jtinvars);

		// setting layouts for the panels
		JPanel north_panel = new JPanel(new GridLayout(4,2));

		JPanel south_panel = new JPanel(new FlowLayout(FlowLayout.LEFT));

		// sets the coloring for the different components
		Color mycolorscheme = new Color(75, 125, 200);
		jlclass.setForeground(Color.black);
		jlmethod.setForeground(Color.black);
		jlvariable.setForeground(Color.black);
		jlplace.setForeground(Color.black);
		jlstatus.setForeground(Color.black);
		jlstatus_msg.setForeground(Color.black);
		north_panel.setBackground(mycolorscheme);
		south_panel.setBackground(mycolorscheme);
		jcclass.setBackground(mycolorscheme);
		jcmeth.setBackground(mycolorscheme);
		jcplace.setBackground(mycolorscheme);
		jcvariable.setBackground(mycolorscheme);
		jmb.setBackground(mycolorscheme);
		file.setBackground(mycolorscheme);
		item.setBackground(mycolorscheme);
		item3.setBackground(mycolorscheme);
		item4.setBackground(mycolorscheme);
		item5.setBackground(mycolorscheme);
		jtinvars.getTableHeader().setBackground(mycolorscheme);

		// adding the components to the window
		north_panel.add(jlclass);
		north_panel.add(jcclass);

		north_panel.add(jlmethod);
		north_panel.add(jcmeth);

		north_panel.add(jlvariable);
		north_panel.add(jcvariable);

		north_panel.add(jlplace);
		north_panel.add(jcplace);

		content.add(north_panel, BorderLayout.NORTH);

		content.add(scrollPane, BorderLayout.CENTER);

		south_panel.add(jlstatus);
		south_panel.add(jlstatus_msg);

		content.add(south_panel, BorderLayout.SOUTH);

		// adding action listeners for the combo boxes
		jcclass.addActionListener(this);
		jcmeth.addActionListener(this);
		jcvariable.addActionListener(this);
		jcplace.addActionListener(this);
	}

	// sends msg to the database to rebuild the database
	public void rebuild()
	{
		dfc.rebuild();
	}

	// sends msg to the database to clear out all the invariants
	public void clear()
	{
		dfc.clear();
	}

	// Adds a dile to the database and then fixes the combo boxes.
	public String addFile(String filename)
	{
		String display_string = "";

		display_string = dfc.addFile(filename);
		display_string = display_string.trim();


		// Fixing combo boxes
		if (!display_string.endsWith("Not Found"))
		{
			action_pro = true;
			updateClassCombo();
			updateMethodCombo();
			updateVariablesCombo();
			updatePlacesCombo();
			go_invars();
			action_pro = false;
		}

		jlstatus_msg.setText(display_string);
		validate();
		repaint();
		return display_string;
	}

	// This is the method that the Invariant Interaction class calls to update the combo boxes and
	// table with the invariants the Emacs client is asking to see.
	public void inputChange(String class_name, String method_name, String var_name, String place_name)
	{
		boolean input_good = false;

		// turning off action performed
		action_pro = true;

		// Looking through list for item that matches the input class name.
		// If found, it sets input good, and updates the rest of the combo
		// boxes with the information about the class.
		for (int i = 0; i < jcclass.getItemCount(); i++)
		{
			if (class_name.equals(jcclass.getItemAt(i)))
			{
				jcclass.setSelectedIndex(i);
				input_good = true;
				updateMethodCombo();
				updateVariablesCombo();
				updatePlacesCombo();
				break;
			}
		}

		// If we couldnt find the class, we want to let the user know that
		// and get out of this method.
		if (!input_good)
		{
			jlstatus_msg.setText("Incorrect Input Attempt! (no class)");
			action_pro = true;
			return;
		}

		// Want to change the no specific names to seeing everything
		if (method_name == null)
			method_name = "All";
		if (var_name == null)
			var_name = "All";
		if (place_name == null)
			place_name = "All";

		// Now we search for the method in the list of method names
		// and again, set input good to true if found and update the other
		// combo boxes.
		input_good = false;
		for (int i = 0; i < jcmeth.getItemCount(); i++)
		{
			if (method_name.equals(jcmeth.getItemAt(i)))
			{
				jcmeth.setSelectedIndex(i);
				updateVariablesCombo();
				updatePlacesCombo();
				input_good = true;
				break;
			}
		}

		// If we couldnt find the method name, we want to let the user know that
		// and get out of this method.
		if (!input_good)
		{
			jlstatus_msg.setText("Incorrect Input Attempt! (no method)");
			action_pro = true;
			return;
		}

		// we can turn action back on since we are done changing combo boxes
		action_pro = false;

		// getting the invariants for the table
		go_invars();
	}

	// We are setting the database's iterator based on a search criteria to get the invariants
	// desired by the user/Emacs.
	private String setIterator(String class_name, String method_name, String var_name, String place_name)
	{
		// We are marking these variables if the element is null, because when we
		// get the invariants we need to know this information
		no_method = false;

		if (method_name != null)
		{
			no_method = true;

			if (method_name.equals("<OBJECT>") || method_name.equals("<CLASS>"))
				no_place = true;
		}


		no_variable = false;
		if (var_name != null)
			no_variable = true;

		no_place = false;
		if (place_name != null)
			no_place = true;

		// Set iterator and set the status that this has been done.
		String ret_val = dfc.setIterator(class_name, method_name, var_name, place_name);

		String display_string = "";
		display_string = "Iterator set!";

		jlstatus_msg.setText(display_string);

		validate();
		repaint();

		return ret_val;
	}

	// This will read in the invariants using the iterator from the intermediate class
	// reading one in at a time and building the list of invariants to display in the
	// table.
	public void getInvariants(String header)
	{
		// Holds the information received from the iterator to be put
		// in the vectors
		String display_string = "";
		String display_method = null;
		String display_place = null;

		// This is a temp variable, that i used to store
		// tempory data received from finding text in a string.
		int place_find = -1;

		// These vectors will contain the data that goes into the table
		Vector vect_head = new Vector();
		Vector vect_col = new Vector();
		Vector vect_method = new Vector();
		Vector vect_place = new Vector();
		Vector vect_invars = new Vector();

		while(true)
		{
			display_string = dfc.getIndividualInvariants();

			if (display_string == null)
				break;

			// The Invariant returned contains a : then it means,
			// that this is really the start of invariants for the
			// method. So we can add all those invariants as part of
			// that method.
			place_find = display_string.indexOf(":");

			if (place_find != -1)
			{
				display_method = display_string.substring(1, place_find);
			}
			else
			{
				// Pulling out the different parts of the invariant, which
				// is the invariant and the program point.
				StringTokenizer tokens = new StringTokenizer(display_string);

				display_place = tokens.nextToken();
				if (display_place.equals("null"))
					display_place = "GLOBAL";

				place_find = display_string.indexOf("\t");

				display_string = display_string.substring(place_find + 1);

				vect_method.add(display_method);
				vect_place.add(display_place);
				vect_invars.add(display_string);
			}
		}

		// Add which columns are going to appear in the table and their
		// header names.
		if (!no_method)
			vect_head.add("Method");

		if (!no_place)
			vect_head.add("Pt.");

		vect_head.add("Invariant");

		// We are changing this so it goes by row in the vector for
		// every certain amount of entries. This is done because of how
		// the table handles the data.
		for (int i = 0; i < vect_method.size(); i++)
		{
			Vector temp_vect = new Vector();

			if (!no_method)
				temp_vect.add(vect_method.get(i));

			if (!no_place)
			{
				String temp_place = (String)(vect_place.get(i));
				if (temp_place.equals("EXIT"))
					temp_vect.add("X");
				else if(temp_place.equals("ENTER"))
					temp_vect.add("E");
				else
					temp_vect.add("G");
			}

			temp_vect.add(vect_invars.get(i));

			vect_col.add(temp_vect);
		}

		// Now we convert this to an array
		String [] str_temp = new String[vect_head.size()];

		for (int s = 0; s < str_temp.length; s++)
			str_temp[s] = (String)vect_head.elementAt(s);

		// set the info in the table and draw it
		mtm.setColumnsVector(str_temp);
		mtm.setDataVector(vect_col.toArray());

		mtm.reDraw();

		try {

		// Trying to get this field as small as possible to save
		// room on screen
		if (!no_place)
		{
			jtinvars.getColumn("Pt.").setMinWidth(0);
			jtinvars.getColumn("Pt.").setMaxWidth(20);
			jtinvars.getColumn("Pt.").setPreferredWidth(30);
		}

		} catch (IllegalArgumentException iae) {
			System.err.println("Problem with setting Pt. field: " + iae);
		}

		jlstatus_msg.setText("Displaying Invariants");
		validate();
		repaint();
	}
}

// This is used to close the program when the user tries to close the window
// using windowing functionality
class CloseWindowExit extends WindowAdapter
{
	public void windowClosing(WindowEvent e)
	{
		System.exit(0);
	}
}

// The table model for where the invariants are going to be shown.
class MyTableModel extends AbstractTableModel
{

	Object[][] data;
	String[] columnNames;

	// Constructor
	public MyTableModel()
	{
		super();
	}

	// Constructor initializing the table with the data and the
	// column names.
	public MyTableModel(Object[] new_data, String[] newNames)
	{
		super();

		data = new Object[new_data.length][];

		for (int i = 0; i < new_data.length; i++)
		{
			Vector temp = (Vector)new_data[i];
			data[i] = temp.toArray();
		}

		columnNames = newNames;
	}

	public Object[][] getDataVector()
	{
		return data;
	}

	public String[] getColumnVector()
	{
		return columnNames;
	}

	// Convert an array of vectors to array of arrays for storing.
	public void setDataVector(Object[] new_data)
	{
		data = new Object[new_data.length][];

		for (int i = 0; i < new_data.length; i++)
		{
			Vector temp = (Vector)new_data[i];
			data[i] = temp.toArray();
		}
	}

	// setting the column headers.
	public void setColumnsVector(String [] new_columnNames)
	{
		columnNames = new_columnNames;
	}

	public int getColumnCount()
	{
		return columnNames.length;
	}

	public int getRowCount()
	{
		return data.length;
	}

	public String getColumnName(int col)
	{
		return (String)(columnNames[col]);
	}

	public Object getValueAt(int row, int col)
	{
		if (row >= data.length)
			return new String("");

		Object temp[] = data[row];

		if (col >= temp.length)
			return new String ("");

		return data[row][col];
	}

	public Class getColumnClass(int c)
	{
		return getValueAt(0, c).getClass();
	}

	public boolean isCellEditable(int row, int col)
	{
		return false;
	}

	public void setValueAt(Object value, int row, int col)
	{
		data[row][col] = value;
	}

	// set off the table to redraw it with new data
	public void reDraw()
	{
		fireTableStructureChanged();
	}
}

// this is used so when the user hits the table header, it will sort according to that
// column, part of the code has been taken from a tutorial page from SUN Microsystems.
// Some code has been added and changed around to reduce the actual amount of code in
// this class.
class TableSorter extends TableMap
{
	int indexes[];
	Vector sortingColumns = new Vector();
	boolean ascending = true;
	int compares;

	// To prevent from errors
	public TableSorter()
	{
		indexes = new int[0];
	}

	// to put this on top of the model.
	public TableSorter(TableModel model)
	{
		setModel(model);
	}

	// send the model to the Map for the table, and fix the table
	// for the new elements
    	public void setModel(TableModel model)
	{
		super.setModel(model);
		reallocateIndexes();
	}

	// This is used to sort the table based on the column the user selects.
	public void sort(Object sender)
	{
		checkModel();

		compares = 0;
		Integer [] IntArray = new Integer[indexes.length];
		for(int i = 0; i < indexes.length; i++)
			IntArray[i] = new Integer(indexes[i]);
		Arrays.sort(IntArray, new tableCompare());
		for(int i = 0; i < indexes.length; i++)
			indexes[i] = IntArray[i].intValue();
	}

	// Comparator used to the sort the columns based on the selection
	class tableCompare implements Comparator
	{
		public int compareRowsByColumn(int row1, int row2, int column)
		{
			Class type = model.getColumnClass(column);
			TableModel data = model;

			// Check for nulls.

			Object o1 = data.getValueAt(row1, column);
			Object o2 = data.getValueAt(row2, column);

			// If both values are null, return 0.
			if (o1 == null && o2 == null)
				return 0;
			else if (o1 == null) // Define null less than everything.
				return -1;
			else if (o2 == null)
				return 1;

			/*
			 * We copy all returned values from the getValue call in case
			 * an optimised model is reusing one object to return many
			 * values.  The Number subclasses in the JDK are immutable and
			 * so will not be used in this way but other subclasses of
			 * Number might want to do this to save space and avoid
			 * unnecessary heap allocation.
			 */

			if (type.getSuperclass() == java.lang.Number.class) {
				Number n1 = (Number)data.getValueAt(row1, column);
				double d1 = n1.doubleValue();
				Number n2 = (Number)data.getValueAt(row2, column);
				double d2 = n2.doubleValue();

				if (d1 < d2) {
					return -1;
				} else if (d1 > d2) {
					return 1;
				} else {
					return 0;
				}
			} else if (type == String.class) {
				String s1 = (String)data.getValueAt(row1, column);
				String s2    = (String)data.getValueAt(row2, column);
				return s1.compareTo(s2);
			} else {
				Object v1 = data.getValueAt(row1, column);
				String s1 = v1.toString();
				Object v2 = data.getValueAt(row2, column);
				String s2 = v2.toString();
				return s1.compareTo(s2);
			}
		}

		public int compare(int row1, int row2)
		{
			compares++;

			for (int level = 0; level < sortingColumns.size(); level++)
			{
				Integer column = (Integer)sortingColumns.elementAt(level);
				int result = compareRowsByColumn(row1, row2, column.intValue());
				if (result != 0)
				{
					return ascending ? result : -result;
				}
			}

			return 0;
		}

		public int compare(Object row1, Object row2)
		{
			Integer it1 = (Integer)row1;
			Integer it2 = (Integer)row2;

			return compare(it1.intValue(), it2.intValue());
		}
	}

	// Fix the table for the new data
	public void reallocateIndexes()
	{
		int rowCount = model.getRowCount();

		// Set up a new array of indexes with the right number of elements
		// for the new data model.
		indexes = new int[rowCount];

		// Initialise with the identity mapping.
		for (int row = 0; row < rowCount; row++)
			indexes[row] = row;
	}

	public void tableChanged(TableModelEvent e)
	{
		reallocateIndexes();
		super.tableChanged(e);
	}

	public void checkModel()
	{
		if (indexes.length != model.getRowCount()) {
			System.err.println("Sorter not informed of a change in model.");
		}
	}


	// The mapping only affects the contents of the data rows.
	// Pass all requests to these rows through the mapping array: "indexes".

	public Object getValueAt(int aRow, int aColumn)
	{
		checkModel();
		return model.getValueAt(indexes[aRow], aColumn);
	}

	public void setValueAt(Object aValue, int aRow, int aColumn)
	{
		checkModel();
		model.setValueAt(aValue, indexes[aRow], aColumn);
	}

	public void sortByColumn(int column)
	{
		sortByColumn(column, true);
	}

	public void sortByColumn(int column, boolean ascending)
	{
		this.ascending = ascending;
		sortingColumns.removeAllElements();
		sortingColumns.addElement(new Integer(column));
		sort(this);
		super.tableChanged(new TableModelEvent(this));
	}

	// There is no-where else to put this.
	// Add a mouse listener to the Table to trigger a table sort
	// when a column heading is clicked in the JTable.
	public void addMouseListenerToHeaderInTable(JTable table)
	{
		final TableSorter sorter = this;
		final JTable tableView = table;
		tableView.setColumnSelectionAllowed(false);
		MouseAdapter listMouseListener = new MouseAdapter()
		{
			public void mouseClicked(MouseEvent e)
			{
				TableColumnModel columnModel = tableView.getColumnModel();
				int viewColumn = columnModel.getColumnIndexAtX(e.getX());
				int column = tableView.convertColumnIndexToModel(viewColumn);

				if (e.getClickCount() == 1 && column != -1) {
					int shiftPressed = e.getModifiers()&InputEvent.SHIFT_MASK;
					boolean ascending = (shiftPressed == 0);
					sorter.sortByColumn(column, ascending);
				}
			}
		};

		JTableHeader th = tableView.getTableHeader();
		th.addMouseListener(listMouseListener);
	}
}

// This function just adds more functionality to the exising table model,
// so as to change the order of the data when the user hits the header
// to be in alphabetical order based on the column hit. This code is also
// taken from a tutorial on SUN Microsystems website.
class TableMap extends AbstractTableModel implements TableModelListener
{
	protected TableModel model;

	public TableModel getModel()
	{
		return model;
	}

	public void setModel(TableModel model)
	{
		this.model = model;
		model.addTableModelListener(this);
	}

	// By default, implement TableModel by forwarding all messages
	// to the model.

	public Object getValueAt(int aRow, int aColumn)
	{
		return model.getValueAt(aRow, aColumn);
	}

	public void setValueAt(Object aValue, int aRow, int aColumn)
	{
		model.setValueAt(aValue, aRow, aColumn);
	}

	public int getRowCount()
	{
		return (model == null) ? 0 : model.getRowCount();
	}

	public int getColumnCount()
	{
		return (model == null) ? 0 : model.getColumnCount();
	}

	public String getColumnName(int aColumn)
	{
		return model.getColumnName(aColumn);
	}

	public Class getColumnClass(int aColumn)
	{
		return model.getColumnClass(aColumn);
	}

	public boolean isCellEditable(int row, int column)
	{
		return model.isCellEditable(row, column);
	}
	//
	// Implementation of the TableModelListener interface,
	//
	// By default forward all events to all the listeners.
	public void tableChanged(TableModelEvent e)
	{
		fireTableChanged(e);
	}
}

