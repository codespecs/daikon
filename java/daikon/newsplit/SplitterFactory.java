package daikon.newsplit;

import utilMDE.*;
import org.apache.oro.text.regex.*;

import daikon.*;
import daikon.split.*;
import java.io.*;
import java.util.*;
import java.lang.reflect.*;


public class SplitterFactory {
  //This class creates splitters from a .spinfo file. The public method is create_splitters
  //which returns a vector containing program point names/splitter array pairs.
  
  private static SplitterLoader loader; 
  private static FileCompiler mycompiler;
  private static String tempdir;
  private static PatternMatcher re_matcher = Global.regexp_matcher;
  private static PatternCompiler re_compiler = Global.regexp_compiler;
  private static PptMap all_ppts;
  
  public SplitterFactory(PptMap all) {
    loader = new SplitterLoader();
    mycompiler = new FileCompiler();
    all_ppts = all;
    
    //try to get the path of the temp directory
    tempdir = getTempdirName();      
  }
  
  /*
   * Reads the splitter info, creates the splitter classes, and returns
   * a vector holding the contents
   * (Pptname, Splitter[], Pptname, Splitter[], ...) 
   * ie. Pptnames and their associated splitters
   * @param <filename>.spinfo
   * @return a Vector holding the pptnames and their associated splitters
   */
  public Vector read_spinfofile(String infofile) throws IOException, FileNotFoundException {
    LineNumberReader reader = UtilMDE.LineNumberFileReader(infofile);
    
    Vector conds = new Vector(); // a vector holding the conditions to be tested at that ppt.
    
    Vector splitters_and_names = new Vector(); //the return vector
    
    String line = reader.readLine();
    StringTokenizer tokenizer;
    Vector replace = new Vector();
    String arguments;
    while (line != null) {
      if (line.equals("")) {
	line = reader.readLine(); 
	continue;
      }
      
      if (line.startsWith("REPLACE")) {
	tokenizer = new StringTokenizer(line, "#");
	tokenizer.nextToken();
	while(tokenizer.hasMoreTokens()){
	    replace.addElement(tokenizer.nextToken().trim());
	}
	line = reader.readLine();
	line.trim();
      }
      
      if (line.startsWith("PPT_NAME")) {
	tokenizer = new StringTokenizer(line, "#");
	tokenizer.nextToken(); 
	String function_name = tokenizer.nextToken().trim();
	line = reader.readLine(); 
	line.trim();
	tokenizer = new StringTokenizer(line, "#");
	Assert.assert(line.startsWith("CONDITIONS"),
		      "Incorrect format in .spinfo file\n");
	tokenizer.nextToken(); 
	while (tokenizer.hasMoreTokens()) {
	  conds.addElement(tokenizer.nextToken());
	}
	line = reader.readLine(); 
	//line.trim();
	
	
	//compile and load the splitters for this Pptname here
	splitters_and_names.addElement(function_name);
	Splitter[] tempsplitters = write_compile_load(function_name, conds, replace);
	splitters_and_names.addElement(tempsplitters);       
      }
      conds.clear();
    }
    return splitters_and_names;
  }
    
  private Splitter[] write_compile_load(String function_name, Vector conds, Vector replace) {
    /* This function is used to write, compile and load all the splitters for 
     * the program point "function_name". The conditions are contained in the argument conds
     */
    Vector splitters = new Vector(); //contains the splitter objects
    Vector splitternames = new Vector(); //contains the paths of all the splitters created
        
    try {
      splitternames = write_function_splitters(function_name, conds, replace);
    }catch(IOException ioe) {
      debugPrint("Exception splitters for PptName " + function_name + "\n" + ioe.toString());
    }
    
    int siz = splitternames.size();
    if (siz > 0) {
      for (int i = 0; i < siz; i++) {
	String className = (String)splitternames.elementAt(i);
	Process proc = mycompiler.compile_Class(tempdir+className+".java");
	//careful: The compilation doesn't give back any output and doesn't tell you if the
	//compile failed or not. Will need to do some error checking here or somewhere else.
	//eg. probing the ErrorStream of the returned process: proc.getErrorStream()
	//However probing the ErrorStream after each compilation is time consuming
	if (i == siz - 1) {
	  //wait for the last compilation at each ppt to finish before continuing.
	  //This will hopefully allow all the compilations in that group to be
	  //finished.
	  try {
	    proc.waitFor ();
	  }catch(InterruptedException e) {
	    debugPrint("Interrupted Process while compiling " + className + " splitter");
	  }
	} 
      }
      
      //we've finished creating all the splitters for one program point. Load them
      for (int i = 0; i < siz; i++) {
	Class temp;
	String className = (String) splitternames.elementAt(i);
	try {
	  temp = loader.load_Class(className, tempdir + className + ".class");
	}catch(ClassFormatError ce) {
	  debugPrint("Error while loading " + className + "\n" + ce.toString());
	  continue;
	}
	if (temp != null) {
	  try {
	    Splitter tempsplitter = (Splitter) temp.newInstance();
	    if (tempsplitter != null) splitters.addElement(tempsplitter);
	  }catch(InstantiationException ie) {
	    debugPrint("Error while instantiating " + className + "\n" + ie);
	  }catch(IllegalAccessException iae) {
	    debugPrint("Error while instantiating " + className + "\n" + iae);
	  }
	}
      }
    }
   
    return (Splitter[]) splitters.toArray(new Splitter[0]);
  }
  
  ////////////
  private Vector write_function_splitters(String ppt_name, Vector conditions, Vector replace) throws IOException  {     
    //this function writes all the splitters at a program point. The conditions are in the 
    //argument named conditions
                    
    Vector splitternames = new Vector();
    PptTopLevel ppt = find_corresponding_ppt(ppt_name);    
    if (ppt == null) {
      debugPrint("No program point corresponds to " + ppt_name);
      return splitternames;
    }
    
    Vector[] params_and_types = get_params_and_types(ppt);
    Vector parameters = params_and_types[0];
    Vector typs = params_and_types[1];
    
    if (parameters.size() > 0 && typs.size() > 0) {
     String[] all_params = (String[])parameters.toArray(new String[0]);
     String[] types = (String[])typs.toArray(new String[0]);
      int len = all_params.length;
      
      //The splitter variable names corresponding to the variables
      //at the program point
      String[] param_names = new String[len];
      for (int i = 0; i < len; i++) {
	// declared variable names in the splitter class cannot have 
	// a "." in them. Change, for example, "node.parent" to "node_parent"
	param_names[i] = all_params[i].replace('.','_');
	if (param_names[i].equals("return")) param_names[i] =  "return_daikon";
      }
      
      //get the function names and argument names of functions to be 
      //replaced. For example, if the function "max(int a, int b)"
      //is to be replaced by the expression "(a > b) ? a : b",
      //then the return value of this function call is a vector containing
      // the function name (max), an array of the argument names [ a b ], and 
      //the replacement expression (a > b) ? a : b 
      Vector replace_data = get_fnames_and_args(replace);
           
      //write a splitter class for each condition:
      // The splittername will be used as names for the classes. For example if the ppt_name is 
      // Foo.bar, the splittername will be Foo_bar
      String splittername = ppt_name.replace('.','_');
      for (int numconds = 0; numconds < conditions.size(); numconds++) {
	
	String condition = (String)conditions.elementAt(numconds);
	//the String 'condition' is returned in this splitter's public method 'condition()'
	//the String 'test_string' is used to do the splitting in the 'test()' method
	String test_string = condition;
	String splitter_fname = splittername + "_" + numconds;
	splitternames.addElement(splitter_fname);
	//a different set of parameters will be used for each condition
	String[] params = (String[])all_params.clone(); 
	
	//substitute the occurence of "== null" or "!= null" with "== 0" in the condition
	test_string = perform_null_substitution(test_string);
	
	//if the condition has a function call in it which needs to be replaced
	//with the function body: eg. isEmpty -> myArray.length == 0,
	//do the replacement now
	test_string = replace_condition(test_string, replace_data);
	
	// ensure that the names of the declared variable names in the splitter
	// match the variable names in the test string.
	// for example: if the condition tests for "this.mylength == 0",
	// the variable corresponding to "this.mylength" in the splitter
	// is declared as "this_mylength". Therefore change the condition
	// to "this_mylength == 0"
	String class_name = ppt_name.substring(0, ppt_name.indexOf('.')+1); 
	test_string = fix_variable_names_in_teststring(params,param_names, test_string, class_name);
		
	// look for all variables which are used as array accessors
	// and change their type to "int_index". This is important 
	// for getting the value from the VarInfo.
	fix_array_index_variable_types(condition, params, types);
	
	StringBuffer file_string = new StringBuffer("package daikon.split; \n\n");
	file_string.append("import daikon.*; \n");
	
	file_string.append("public final class " + splitter_fname + " extends Splitter { \n\n");
	
	//print the condition() method
	file_string.append("\tpublic String condition () { return\"" + condition + "\" ;} \n\n"); 
	
	//print the parameters
	file_string.append("");
	
	for (int i = 0; i < len; i++) {
	  String par = "", typ = "";
	  if (params[i].equals("**remove**")) continue;
	  
	  try {
	    par = param_names[i].trim();
	    typ = types[i].trim();
	  }catch(ArrayIndexOutOfBoundsException aob) {
	    debugPrint("Error: Please check args and types in splitter file");
	    debugPrint(aob.toString());
	  }
	  
	  if (typ.equals("int")) {
	    file_string.append("\tVarInfo " + par + "_varinfo; \n");
	    file_string.append("\tlong " + par + "; \n");
	  }else if (typ.equals("int_index")) {
	    file_string.append("\tint " + par + "; \n");
	    file_string.append("\tVarInfo " + par + "_varinfo; \n");
	  }else if (typ.equals("int[]")) {
	    file_string.append("\tVarInfo " + par + "_array_varinfo; \n");
	    file_string.append("\tlong[] " + par + "_array; \n");
	  }else if (typ.equals("String")) {
	    file_string.append("\tVarInfo " + par + "_varinfo; \n");
	    file_string.append("\tString " + par + "; \n");
	  }else if (typ.equals("String[]")) {
	    file_string.append("\tString[] " + par + "_array; \n");
	    file_string.append("\tVarInfo " + par + "_array_varinfo; \n");
	  }else if (typ.equals("boolean")) {
	    file_string.append("\tVarInfo " + par + "_varinfo; \n");
	    file_string.append("\tboolean " + par + "; \n");
	  }else{
	    debugPrint("Can't deal with this type " + typ + " declared in Splitter file"); 
	  }
	}
	
	
	// print the constructors
	file_string.append("\n\tpublic " + splitter_fname + "() { } \n");
	file_string.append("\tpublic " + splitter_fname + "(Ppt ppt) {  \n");
	for (int i = 0; i < len; i++) {
	  String param =  params[i];
	  if (param.equals("**remove**")) continue;
	  String param_name = param_names[i];
	  String typ = types[i];
	  if (typ.equals("int[]") || typ.equals("String[]")) {
	    file_string.append("\t\t" + param_name + "_array_varinfo = ppt.findVar(\"" + param + "[]\") ; \n");
	    
	    //try to attach a "_array" to the end of an array variable name anytime it's 
	    //being used as an array. (eg change "<arrayname>.length" to "<arrayname>_array.length"
	    //and <arrayname>[i] to <arrayname>_array[i] so that the array , and not the 
	    //hashcode of the array is used.
	    test_string = fix_array_variablename_in_teststring(param, test_string);
	    
	  }else{
	    file_string.append("\t\t" + param_name + "_varinfo = ppt.findVar(\"" + param + "\") ; \n");
	  }
	}
	
	//////
	file_string.append("\t}\n\n");
	
	// print the instantiate method.
	file_string.append("\tpublic Splitter instantiate(Ppt ppt) { \n\t\treturn new ");
	file_string.append(splitter_fname + "(ppt);\n\t} \n\n");
	
	// print the valid() method
	int num_param = 0;
	file_string.append("\tpublic boolean valid() { \n\t\treturn(");
	for (int i = 0; i < len; i++) {
	  if (params[i].equals("**remove**")) continue;
	  if (types[i].equals("int[]") || types[i].equals("String[]")) {
	    num_param++;
	    file_string.append((num_param > 1 ? " && ": "") + "(" + param_names[i] + "_array_varinfo != null)");
	  }else{
	    num_param++;
	    file_string.append((num_param > 1 ? " && ": "") + "(" + param_names[i] + "_varinfo != null)");
	  }
	}
	file_string.append((num_param > 0 ? " && " :"") + "true");
	file_string.append(");\n\t}\n\n");
	
	//print the test() method
	file_string.append("\tpublic boolean test(ValueTuple vt) { \n");
	for (int i = 0; i < len; i++) {
	  if (params[i].equals("**remove**")) continue;
	  String typ = types[i];
	  String param_name = param_names[i];
	  
	  if (typ.equals("int_index")) {
	    file_string.append("\t\t" + param_name + " = " + param_name + "_varinfo.getIndexValue(vt); \n"); 
	  }else if (typ.equals("int")) {
	    file_string.append("\t\t" + param_name + " = " + param_name + "_varinfo.getIntValue(vt); \n"); 
	  }else if (typ.equals("boolean")) {
	    file_string.append("\t\t" + param_name + " = (" + param_name + "_varinfo.getIntValue(vt) > 0 ? true : false ); \n");
	  }else if (typ.equals("int[]")) {
	    file_string.append("\t\t"+ param_name + "_array = " + param_name + "_array_varinfo.getIntArrayValue(vt); \n"); 
	  }else if (typ.equals("String")) {
	    file_string.append("\t\t" + param_name + " = " + param_name + "_varinfo.getStringValue(vt); \n"); 
	  }else if (typ.equals("String[]")) {
	    file_string.append("\t\t" + param_name + "_array = " + param_name + "_array_varinfo.getStringArrayValue(vt); \n"); 
	  }else{
	    debugPrint("Can't deal with this type! " + typ + " declared in Splitter File"); 
	  }
	}
	
	file_string.append("\t\treturn( " + test_string + " ); \n\t}\n}\n");
	
	//write to the file
	try { 
	  BufferedWriter writer = UtilMDE.BufferedFileWriter(tempdir+splitter_fname+".java");
	  writer.write(file_string.toString());
	  writer.flush();
	}catch(IOException ioe) {
	  debugPrint("Error while writing splitter file " + tempdir+splitter_fname+".java \n");
	  debugPrint(ioe.toString());
	}
      }
    }
    return splitternames;
  }
  
  
  private PptTopLevel find_corresponding_ppt(String ppt_name) {
    //given a regex corresponding to a program point, finds a corresponding program point.
    PptTopLevel return_ppt;
    Iterator ppt_itor = all_ppts.iterator();
    //we are looking for a matching exit ppt. This is because the exit ppt usually has
    //more relevant variables in scope (eg. return) than the enter.
    ppt_name = ppt_name + ".*EXIT";  
    try {
      Pattern ppt_pattern = re_compiler.compile(ppt_name);
      Pattern object_pattern = re_compiler.compile("OBJECT");
      while (ppt_itor.hasNext()) {
	String name = ((PptTopLevel)ppt_itor.next()).name;
	if (re_matcher.contains( name, ppt_pattern)) {
	  return all_ppts.get(name);
	}else if (re_matcher.contains( ppt_name, object_pattern) && re_matcher.contains(name, object_pattern)) {
	  //this is for the OBJECT program point
	  return all_ppts.get(name);
	}
      }
    } catch (Exception e) {
      debugPrint(e.toString() + " while matching " + ppt_name);
    }   
    return null;
  }
  
  private Vector[] get_params_and_types(PptTopLevel ppt) {
    //gets the parameters and their corresponding types from the program point
    System.err.println(ppt.name);
    Vector parameters = new Vector();
    Vector types = new Vector();
    VarInfo[] var_infos = ppt.var_infos;
    int num_return = 0;
    for (int i = 0; i < var_infos.length; i++) {
      String temp = var_infos[i].name.name().trim();
      if (temp.endsWith(".class") || temp.startsWith("orig")) continue;
      if (temp.equals("return")){
	num_return ++;
	if (num_return > 1) continue;
      }
      if (temp.endsWith("[]")) { 
	temp = temp.substring(0, temp.length() - 2);
      }
      parameters.addElement(temp);
      if ((var_infos[i].type.format().trim()).equals("boolean")) {
	types.addElement(new String("boolean"));
      }else{
	types.addElement(var_infos[i].rep_type.format().trim());
      }
    }
    Vector[] return_vector = {parameters, types};
    return return_vector;
  }

  private Vector get_fnames_and_args(Vector replace) {
    
    Vector replace_data = new Vector();
    try {
      //try to find out the names of the arguments and the function name

      //look for the pattern functionname(xxxx) and extract the argument list
      Pattern arg_pattern = re_compiler.compile("\\s*(\\S*)\\s*\\((.*)\\)\\s*"); 
      for (int i = 0; i < replace.size(); i+=2) {
	String replace_expr = (String)replace.elementAt(i);
	PatternMatcherInput replace_expr_pattern = new PatternMatcherInput(replace_expr);
	if(re_matcher.contains(replace_expr_pattern, arg_pattern)){
	  MatchResult result = re_matcher.getMatch();
	  String function_name = result.group(1); 
	  String arguments = result.group(2);
	  //the arguments are in the form "arg1 name1, arg2 name2, arg3 name3, ..."
	  StringTokenizer split_args = new StringTokenizer(arguments.trim(), ",");
	  Vector tempargs = new Vector();
	  while (split_args.hasMoreTokens()) {
	    String arg = split_args.nextToken().trim(); 
	    //each argument is now of the form "arg name" after splitting using ","
	    StringTokenizer extract_argname = new StringTokenizer(arg);
	    extract_argname.nextToken(); // the type of the argument
	    tempargs.addElement(extract_argname.nextToken().trim()); //the argument name
	  }
	  replace_data.addElement(function_name);
	  replace_data.addElement((String[])tempargs.toArray(new String[0]));
	  replace_data.addElement(replace.elementAt(i+1));
	}
      }
    }catch(Exception e){
      System.out.println(e.toString());
    }
    
    
    for (int i = 0; i < replace_data.size(); i+=3) {
      String fname = (String) replace_data.elementAt(i);
      int num_args = ((String[])replace_data.elementAt(i+1)).length;
      fname = fname + "\\s*\\(\\s*";
      for (int j = 0; j < num_args; j++) {
	fname = fname + "\\s*(\\S*)";
	if (j+1 < num_args) {
	  fname = fname+"\\s*,";
	}
      }
      fname = fname + "\\s*\\)";
      replace_data.setElementAt(fname, i);
    }
    
    return replace_data;
  }
  
  private String replace_condition(String condition, Vector replace_data){
    //perform a replacement of a function call with the body of the function
    //the argument Vector 'replace' stores alternately the function name and the function body.

    Pattern replace_expr_pattern;
    for (int i = 0; i < replace_data.size(); i+=3) {
      try {
	String temp = (String)replace_data.elementAt(i);
	replace_expr_pattern = re_compiler.compile( temp );
	if (re_matcher.contains(new PatternMatcherInput(condition), replace_expr_pattern)) {
	  MatchResult result = re_matcher.getMatch(); 
	  //System.err.println(condition + " contains " + replace_expr_pattern.getPattern().toString());
	  Perl5Substitution temp_subst = new Perl5Substitution("#", Perl5Substitution.INTERPOLATE_ALL);
	  condition = Util.substitute(re_matcher, replace_expr_pattern, temp_subst, condition, Util.SUBSTITUTE_ALL);
	  String[] arguments = (String[])replace_data.elementAt(i+1);
	  for(int j = 1; j < result.groups(); j++){
	    Perl5Substitution arg_subst = new Perl5Substitution(result.group(j)); 
	    Pattern arg_pattern = re_compiler.compile(arguments[j-1]);
	    String replacement = (String) replace_data.elementAt(i+2);
	    replace_data.setElementAt( Util.substitute(re_matcher, arg_pattern, arg_subst, replacement, Util.SUBSTITUTE_ALL), i+2);
	  }
	  
	  Perl5Substitution replace_subst = 
	    new Perl5Substitution((String) replace_data.elementAt(i+2) , Perl5Substitution.INTERPOLATE_ALL);
	  condition = Util.substitute(re_matcher, re_compiler.compile("#"), replace_subst, condition, Util.SUBSTITUTE_ALL);
	}else{
	  //System.err.println(condition + " does not contain " + replace_expr_pattern.getPattern().toString());
	}
      }catch(Exception e) {
	debugPrint( e.toString() +" while performing replacement on condition " + condition);
      }
    }
    return condition; 
    }

  private String fix_array_variablename_in_teststring (String varname, String test_string){
    //fix the variable names in the condition to match the declared variable
    //names in the splitter. Anytime you see a <varname>.length or <varname>[] being used
    // in a condition, add a "_array" to the varname.
    String dot_length; //The regular expression used to search for "<arrayname>.length"
    String bracket;  //the regular expression used to search for "<arrayname>[]"
    
    if (varname.startsWith("this.")) {
      bracket = "(" + varname + "|" + varname.substring(5) + ")" + "(\\s*\\[[^\\]]*)";
      dot_length = "(" + varname + "|" + varname.substring(5) + ")\\.length";
    }else{
      bracket = "(" + varname  + ")(\\s*\\[[^\\]]*)";
      dot_length = "(" + varname + ")\\.length";
    }
    
    //try performing the substitution on every condition
    Pattern brack_pattern, length_pattern;
    Perl5Substitution brack_subst, length_subst; 
    try {
      brack_pattern = re_compiler.compile(bracket);
      brack_subst = new Perl5Substitution("$1_array$2", Perl5Substitution.INTERPOLATE_ALL);
      length_pattern = re_compiler.compile(dot_length);
      length_subst = new Perl5Substitution("$1_array.length", Perl5Substitution.INTERPOLATE_ALL);
      
      if (re_matcher.contains(test_string, brack_pattern)) {
	test_string = 
	  Util.substitute(re_matcher, brack_pattern, brack_subst, test_string, Util.SUBSTITUTE_ALL);
      }
      if (re_matcher.contains(test_string, length_pattern)) {
	test_string = 
	  Util.substitute(re_matcher, length_pattern, length_subst, test_string, Util.SUBSTITUTE_ALL);
      }
    }catch(Exception e) {
      debugPrint(e.toString() + " while writing performing substitution on teststring " + test_string + "\n");
    }
    return test_string;
  }
  
  private String fix_variable_names_in_teststring(String[] params, String[] param_names, String test_string, String class_name ){
    // this function ensures that the variables being used in the test string match up with the 
    // declared variables in the condition.
    Pattern param_pattern;
    Perl5Substitution param_subst;
    
    try {
      for (int i = 0; i < params.length; i++) {
	
	//some instrumented variables start with "this." whereas the "this" is not always used
	//in the condition. Eg. the instrumented variable is "this.myArray", but the 
	//condition test is "myArray.length == 0". In such a situation, make sure that the 
	//names are consistent. this.myArray -> this_myArray and
	//myArray.length == 0 -> this_myArray.length == 0
	if (params[i].startsWith("this")) {
	  String params_minus_this = params[i].substring(5);
	  //for example for the variable 'this.myArray', we will be searching
	  //the condition for the regex "myArray|this.myArray"
	  param_pattern = re_compiler.compile(params[i] + "|" + params_minus_this);
	}else if (params[i].startsWith(class_name)) {
	  param_pattern = re_compiler.compile(params[i] + "|" + params[i].substring(class_name.length()));
	}else{
	  param_pattern = re_compiler.compile(params[i]);
	}
	
	param_subst = new Perl5Substitution(param_names[i], Perl5Substitution.INTERPOLATE_ALL);
	PatternMatcherInput input = new PatternMatcherInput(test_string);
	//remove any parameters which are not used in the condition
	if (re_matcher.contains(input, param_pattern)) {
	  test_string = Util.substitute(re_matcher, param_pattern, param_subst, test_string, Util.SUBSTITUTE_ALL);
	  while (re_matcher.contains(input, param_pattern)) {
	    test_string = Util.substitute(re_matcher, param_pattern, param_subst, test_string, Util.SUBSTITUTE_ALL);
	  }
	}else{
	  params[i] = "**remove**"; //this parameter is not needed in the test. ignore later
	}
      }
      /*
	Pattern return_pattern = re_compiler.compile("return");
	Perl5Substitution return_subst = new Perl5Substitution("return_daikon");
	PatternMatcherInput input = new PatternMatcherInput(test_string);
	if(re_matcher.contains(input, return_pattern)){
	test_string = Util.substitute(re_matcher, return_pattern, return_subst, test_string, Util.SUBSTITUTE_ALL);
	}
      */
    }catch(Exception e) {
      debugPrint(e.toString());
    }
    
    return test_string;
  }

  private void fix_array_index_variable_types(String condition, String[] params, String[] types){
    //find all occurences of arrayname[varname] in the test_string
    //and change the type of the  variable names in the brackets to "int_index"
    
    Vector arrayIndexVariables = new Vector();
    
    try {
      Pattern find_index_pattern = re_compiler.compile("\\[\\s*([^\\])]*)\\s*\\]");
      PatternMatcherInput input = new PatternMatcherInput(condition);
      while (re_matcher.contains(input, find_index_pattern)) {
	MatchResult result = re_matcher.getMatch();
	String possible_varname = result.group(1);
	//if we have say myarray[ i + j ], we have to identify both i and j
	//as array index variables and treat them as integers.
	Pattern operator_pattern = re_compiler.compile("[+=!><-]"); //split on this pattern
	Vector tempIndices = Util.split(re_matcher, operator_pattern, possible_varname);
	for (int i = 0; i < tempIndices.size(); i++) {
	  arrayIndexVariables.addElement(((String)tempIndices.elementAt(i)).trim());
	}
      }
    }catch(Exception e) {
      debugPrint(e.toString());
    }
    
    for (int i = 0; i < arrayIndexVariables.size(); i++) {
      for (int j = 0; j < params.length; j++) {
	if (((String) arrayIndexVariables.elementAt(i)).equals(params[j])) {
	  types[j] = "int_index";
	}
      }
    }
  }

  private String perform_null_substitution(String test_string){
    //substitute the occurence of "== null" or "!= null" with "== 0" in the condition
    try {
      Pattern null_pattern = re_compiler.compile("\\s*(!|=)\\s*=\\s*null");
      Perl5Substitution null_subst = new Perl5Substitution(" == 0", Perl5Substitution.INTERPOLATE_ALL);
      
      if (re_matcher.contains(test_string, null_pattern)) {
	test_string = 
	  Util.substitute(re_matcher, null_pattern, null_subst, test_string, Util.SUBSTITUTE_ALL);
      }
    }catch(Exception e) {
      debugPrint("Error performing subtitution of '== null' in " + test_string );
    }
    return test_string;
  }
  
  public void debugPrint(String s) {
    if (Global.debugPptSplit) {
      System.out.println(s);
    }
  }

  private String getTempdirName(){
    //this is where the splitter files are created
    try{
      File fi = File.createTempFile("test", null);
      tempdir = fi.getParent();
      fi.delete();
    }catch(IOException e) {
      debugPrint("Cannot find temp directory:");
      debugPrint("Trying to create files in current directory");
      tempdir = ".";
    }
    
    try {
      File splitdir = new File(tempdir + File.separator + "daikon_Split" + File.separator);
      splitdir.mkdirs();
      if (splitdir.exists()) tempdir = splitdir.getPath();
    }catch(Exception e) {
      debugPrint(e.toString());
    }
    return tempdir + File.separator;
  }
}
