package daikon.newsplit;

import utilMDE.*;
import org.apache.oro.text.regex.*;

import daikon.*;
import daikon.split.*;
import java.io.*;
import java.util.*;
import java.lang.reflect.*;

public class SplitterFactory {
    
  private static SplitterLoader loader; 
  private static FileCompiler mycompiler;
  private static String tempdir;
  private static PatternMatcher re_matcher = Global.regexp_matcher;
  private static PatternCompiler re_compiler = Global.regexp_compiler;
  private static PptMap all_ppts;
  
  public SplitterFactory(PptMap all){
    loader = new SplitterLoader();
    mycompiler = new FileCompiler();
    all_ppts = all;
    
    try{
      //try to get the path of the temp directory
      File fi = File.createTempFile("test", null);
      fi.deleteOnExit();
      tempdir = fi.getPath().substring(0, fi.getPath().lastIndexOf(File.separator)+1);
    }catch(IOException e){
      debugPrint("Cannot find temp directory:");
      debugPrint("Trying to create files in current directory");
      tempdir = ".";
    }
  
    try{
      File splitdir = new File(tempdir + File.separator + "daikon_Split" + File.separator);
      splitdir.mkdirs();
      if(splitdir.exists()){
	tempdir = splitdir.getPath() + File.separator;
      }
    }catch(Exception e){
      debugPrint(e.toString());
    }
  }
  
  //Reads the splitter info, creates the splitter classes, compiles them and 
  //returns a Vector holding the following contents:
  //(Pptname, Splitter[], Pptname, Splitter[], ...) ie. Pptnames and their
  //associated splitters
  public  Vector create_splitters(String infofile) throws IOException, FileNotFoundException {
    String line = "";
    StringTokenizer tokenizer;
    LineNumberReader reader = UtilMDE.LineNumberFileReader(infofile);
    String function_name;
    Vector conds = new Vector(); // a vector holding the conditions to be tested at that ppt.
    Vector replace = new Vector();
    Vector splitters_and_names = new Vector(); //the return vector
    
    line = reader.readLine();
    while(line != null){
      if(line .equals("")){
	line = reader.readLine(); 
	continue;
      }
      if(line.startsWith("PPT")){
	tokenizer = new StringTokenizer(line, ",");
	tokenizer.nextToken(); //first token is PPT_NAME
	function_name = (tokenizer.nextToken()).trim();
	line = reader.readLine(); line.trim();
	tokenizer = new StringTokenizer(line, ",");
	Assert.assert(line.startsWith("COND"));
	tokenizer.nextToken(); //first token is CONDITIONS
	while(tokenizer.hasMoreTokens()){
	  conds.addElement(tokenizer.nextToken());
	}
	line = reader.readLine(); line.trim();
	if(line.startsWith("REPL")){
	  tokenizer = new StringTokenizer(line, ",");
	  tokenizer.nextToken(); //first token is REPLACE
	  while(tokenizer.hasMoreTokens()){
	    replace.addElement(tokenizer.nextToken());
	  }
	  line = reader.readLine();line.trim();
	}
	
	//process the Splitters for this Pptname here
	splitters_and_names.addElement(function_name);
	Splitter[] tempsplitters = write_compile_load(function_name, conds, replace);
	splitters_and_names.addElement(tempsplitters);       
      }
      conds.clear();
      replace.clear();
    }
    return splitters_and_names;
  }
  
  private Splitter[] write_compile_load(String function_name, Vector conds, Vector replace){
    Vector splitternames = new Vector();
    Vector splitters = new Vector();
    String className;    
        
    try{
      splitternames = write_function_splitters(function_name, conds, replace);
    }catch(IOException ioe){
      debugPrint("Exception splitters for PptName " + function_name + "\n" + ioe.toString());
    }
    
    int siz = splitternames.size();
    if(siz > 0){
      for(int i = 0; i < siz; i++){
	className = (String)splitternames.elementAt(i);
	
	//careful: The compilation doesn't give back any output and doesn't tell you if the
	//compile failed or not. Will need to do some error checking here or somewhere else.
	//eg. probing the ErrorStream of the returned process: proc.getErrorStream()
	Process proc = mycompiler.compile_Class(tempdir+className+".java");
	if(i == siz - 1){
	  try{
	    proc.waitFor();
	  }catch(InterruptedException e){
	    debugPrint("Interrupted Process while compiling " + className + " splitter");
	  }
	} 
      }
      
      //we've finished creating all the splitters for one program point. Load them
      for(int i = 0; i < siz; i ++){
	Class temp;
	className = (String) splitternames.elementAt(i);
	try{
	  temp = loader.load_Class(className, tempdir + className + ".class");
	}catch(ClassFormatError ce){
	  debugPrint("Error while loading " + className + "\n" + ce.toString());
	  continue;
	}
	if(temp != null){
	  try{
	    Splitter tempsplitter = (Splitter) temp.newInstance();
	    if(tempsplitter != null) splitters.addElement(tempsplitter);
	  }catch(InstantiationException ie){
	    debugPrint("Error while instantiating " + className + "\n" + ie);
	  }catch(IllegalAccessException iae){
	    debugPrint("Error while instantiating " + className + "\n" + iae);
	  }
	}
      }
    }
   
    return (Splitter[]) splitters.toArray(new Splitter[0]);
  }
  
  ////////////
  private Vector write_function_splitters(String ppt_name, Vector conditions, Vector replace) throws IOException  { 
    
    Pattern brack_pattern, length_pattern, find_index_pattern, operator_pattern, param_pattern;
    Vector parameters,typs;
    Perl5Substitution brack_subst, length_subst, param_subst;
    MatchResult result;
    String class_name = ppt_name.substring(0, ppt_name.indexOf('.')+1) , splittername;
    int siz = conditions.size();
    Iterator ppt_itor = all_ppts.iterator();
    PptTopLevel ppt = null;
    String[] all_params, types;
    Vector splitternames = new Vector();
    String[] repl = (String[])replace.toArray(new String[0]);
    
    
    ppt = find_corresponding_ppt(ppt_name, ppt_itor);    
    if(ppt == null){
      debugPrint("No program point corresponds to " + ppt_name);
      return splitternames;
    }
    
    Vector[] params_and_types = get_params_and_types(ppt);
    parameters = params_and_types[0];
    typs = params_and_types[1];
    
    if(parameters.size() > 0 && typs.size() > 0){
      all_params = (String[])parameters.toArray(new String[0]);
      types = (String[])typs.toArray(new String[0]);
      int len = all_params.length;
      String[] param_names = new String[len];
      String[] params_minus_this = new String[len];
      
      for(int i = 0; i < len; i++){
	param_names[i] = all_params[i].replace('.','_');
	if(all_params[i].startsWith("this.")){
	  params_minus_this[i] = all_params[i].substring(5);
	}
      }
      
      //fix variable names in the condition string by replacing '.' with '_'
      //variable names instrumented in daikon could have dots in them
      //eg. this.current.right. After this substitution, a condition like
      //this.current.right > 0 would be transformed to 
      //this_current_right > 0, with a corresponding VarInfo this_current_right
      splittername = ppt_name.replace('.','_');
      
      //write a splitter class for each condition:
      for(int numconds = 0; numconds < siz; numconds++){
	String splitter_fname = splittername + "_" + numconds;
	String[]params = (String[])all_params.clone();
	splitternames.addElement(splitter_fname);
	BufferedWriter writer = UtilMDE.BufferedFileWriter(tempdir+splitter_fname+".java");
	String condition = (String)conditions.elementAt(numconds);
	String test_string = condition;
	
	//if the condition has a function call in it which needs to be replaced
	//with the function body: eg. isEmpty -> myArray.length == 0,
	//do the replacement now
	test_string = replace_condition(test_string, replace);
	
	for(int i = 0; i < len; i++){
	  try{
	    //some instrumented variables start with "this." whereas the "this" is not used
	    //in the condition. Eg. the instrumented variable is "this.myArray", but the 
	    //condition test is "myArray.length == 0". In this case, we are going to search
	    //in the condition for both "this.myArray" and "myArray"
	    if(params[i].startsWith("this.")){
	      param_pattern = re_compiler.compile(params[i] + "|" + params_minus_this[i]);
	    }else if(params[i].startsWith(class_name)){
	      param_pattern = re_compiler.compile(params[i] + "|" + params[i].substring(class_name.length()));
	    }else{
	      param_pattern = re_compiler.compile(params[i]);
	    }
	    
	    param_subst = new Perl5Substitution(param_names[i], Perl5Substitution.INTERPOLATE_ALL);
	    PatternMatcherInput input = new PatternMatcherInput(test_string);
	    //remove any parameters which are not used in the condition
	    if(re_matcher.contains(input, param_pattern)){
	      test_string = Util.substitute(re_matcher, param_pattern, param_subst, test_string, Util.SUBSTITUTE_ALL);
	      while(re_matcher.contains(input, param_pattern)){
		test_string = Util.substitute(re_matcher, param_pattern, param_subst, test_string, Util.SUBSTITUTE_ALL);
	      }
	    }else{
	      params[i] = "**remove**"; //this parameter is not needed in the test. ignore later
	    }
	  }catch(Exception e){
	    debugPrint(e.toString());
	  }
	}
	//////end fix variable names in condition
	
	
	///// find array index variables  ///////////////////////////////////////
	Vector arrayIndexVariables = new Vector();
	Vector tempIndices = new Vector();
	
	try{
	  //find all occurences of arrayname[varname] and treat varname as an
	  //array index (integer)
	  find_index_pattern = re_compiler.compile("\\[\\s*([^\\])]*)\\s*\\]");
	  operator_pattern = re_compiler.compile("[+=!><-]");
	  PatternMatcherInput input = new PatternMatcherInput(condition);
	  while(re_matcher.contains(input, find_index_pattern)){
	    result = re_matcher.getMatch();
	    String possible_varname = result.group(1);
	    //if we have say myarray[ i + j ], we have to identify both i and j
	    //as array indices and treat them as integers.
	    tempIndices = Util.split(re_matcher, operator_pattern, possible_varname);
	    for(int i = 0; i < tempIndices.size(); i++){
	      arrayIndexVariables.addElement(((String)tempIndices.elementAt(i)).trim());
	    }
	  }
	}catch(Exception e){
	  debugPrint(e.toString());
	}
	
	for(int i = 0; i < arrayIndexVariables.size(); i++){
	  for(int j = 0; j < len; j++){
	    if(((String) arrayIndexVariables.elementAt(i)).equals(params[j])){
	      types[j] = "int_index";
	    }
	  }
	}
	///// end find array index variables ////////////////////////////////////
	
	StringBuffer file_string = new StringBuffer("package daikon.split; \n\n");
	file_string.append("import daikon.*; \n");
	
	file_string.append("public final class " + splitter_fname + " extends Splitter { \n\n");
	
	//print the condition() method
	file_string.append("\tpublic String condition () { return\"" + condition + "\" ;} \n\n"); 
	
	//print the parameters
	file_string.append("");
	
	for(int i = 0; i < len; i++){
	  String par = "", typ = "";
	  if(params[i].equals("**remove**")) continue;
	  
	  try{
	    par = param_names[i].trim();
	    typ = types[i].trim();
	  }catch(ArrayIndexOutOfBoundsException aob){
	    debugPrint("Error: Please check args and types in splitter file");
	    debugPrint(aob.toString());
	  }
	  
	  if(typ.equals("int")){
	    file_string.append("\tVarInfo " + par + "_varinfo; \n");
	    file_string.append("\tlong " + par + "; \n");
	  }else if(typ.equals("int_index")){
	    file_string.append("\tint " + par + "; \n");
	    file_string.append("\tVarInfo " + par + "_varinfo; \n");
	  }else if(typ.equals("int[]")){
	    file_string.append("\tVarInfo " + par + "_array_varinfo; \n");
	    file_string.append("\tlong[] " + par + "_array; \n");
	  }else if(typ.equals("String")){
	    file_string.append("\tVarInfo " + par + "_varinfo; \n");
	    file_string.append("\tString " + par + "; \n");
	  }else if(typ.equals("String[]")){
	    file_string.append("\tString[] " + par + "_array; \n");
	    file_string.append("\tVarInfo " + par + "_array_varinfo; \n");
	  }else if(typ.equals("boolean")){
	    file_string.append("\tVarInfo " + par + "_varinfo; \n");
	    file_string.append("\tboolean " + par + "; \n");
	  }else{
	    debugPrint("Can't deal with this type " + typ + " declared in Splitter file"); 
	  }
	}
	
	
	// print the constructors
	file_string.append("\n\tpublic " + splitter_fname + "() { } \n");
	file_string.append("\tpublic " + splitter_fname + "(Ppt ppt) {  \n");
	for(int i = 0; i < len; i++){
	  String param =  params[i];
	  if(param.equals("**remove**")) continue;
	  String param_name = param_names[i];
	  String typ = types[i];
	  String temp1, temp2;
	  
	  if(typ.equals("int[]") || typ.equals("String[]")){
	    file_string.append("\t\t" + param_name + "_array_varinfo = ppt.findVar(\"" + param + "[]\") ; \n");
	    
	    //try to attach a "_array" to the end of an array variable name anytime it's 
	    //being used as an array. (eg change "arrayname.length" to "arrayname_array.length"
	    //and arrayname[i] to arrayname_array[i] so that the array , and not the 
	    //hashcode of the array is used.
	    
	    if(param.startsWith("this.")){
	      temp1 = "(" + param + "|" + param.substring(5) + ")" + "(\\s*\\[[^\\]]*)";
	      temp2 = "(" + param + "|" + param.substring(5) + ")\\.length";
	    }else{
	      temp1 = "(" + param  + ")(\\s*\\[[^\\]]*)";
	      temp2 = "(" + param + ")\\.length";
	    }
	    
	    //try performing the substitution on every condition
	    try{
	      brack_pattern = re_compiler.compile(temp1);
	      brack_subst = new Perl5Substitution("$1_array$2", Perl5Substitution.INTERPOLATE_ALL);
	      length_pattern = re_compiler.compile(temp2);
	      length_subst = new Perl5Substitution("$1_array.length", Perl5Substitution.INTERPOLATE_ALL);
	      
	      if(re_matcher.contains(test_string, brack_pattern)){
		test_string = 
		  Util.substitute(re_matcher, brack_pattern, brack_subst, test_string, Util.SUBSTITUTE_ALL);
	      }
	      if(re_matcher.contains(test_string, length_pattern)){
		test_string = 
		  Util.substitute(re_matcher, length_pattern, length_subst, test_string, Util.SUBSTITUTE_ALL);
	      }
	    }catch(Exception e){
	      debugPrint(e.toString() + " while writing splitter " + splitter_fname + "\n");
	    }
	    
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
	for(int i = 0; i < len; i++){
	  if(params[i].equals("**remove**")) continue;
	  if(types[i].equals("int[]") || types[i].equals("String[]")){
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
	for(int i = 0; i < len; i++){
	  if(params[i].equals("**remove**")) continue;
	  String typ = types[i];
	  String param_name = param_names[i];
	  
	  if(typ.equals("int_index")){
	    file_string.append("\t\t" + param_name + " = " + param_name + "_varinfo.getIndexValue(vt); \n"); 
	  }else if(typ.equals("int")){
	    file_string.append("\t\t" + param_name + " = " + param_name + "_varinfo.getIntValue(vt); \n"); 
	  }else if(typ.equals("boolean")){
	    file_string.append("\t\t" + param_name + " = (" + param_name + "_varinfo.getIntValue(vt) > 0 ? true : false ); \n");
	  }else if(typ.equals("int[]")){
	    file_string.append("\t\t"+ param_name + "_array = " + param_name + "_array_varinfo.getIntArrayValue(vt); \n"); 
	  }else if(typ.equals("String")){
	    file_string.append("\t\t" + param_name + " = " + param_name + "_varinfo.getStringValue(vt); \n"); 
	  }else if(typ.equals("String[]")){
	    file_string.append("\t\t" + param_name + "_array = " + param_name + "_array_varinfo.getStringArrayValue(vt); \n"); 
	  }else{
	    debugPrint("Can't deal with this type! " + typ + " declared in Splitter File"); 
	  }
	}
	
	//substitute the occurence of "== null" with "== 0" in the condition
	try{
	  Pattern null_pattern = re_compiler.compile("\\s*(!|=)\\s*=\\s*null");
	  Perl5Substitution null_subst = new Perl5Substitution(" == 0", Perl5Substitution.INTERPOLATE_ALL);
    
	  if(re_matcher.contains(test_string, null_pattern)){
	    test_string = 
	      Util.substitute(re_matcher, null_pattern, null_subst, test_string, Util.SUBSTITUTE_ALL);
	  }
	}catch(Exception e){
	  debugPrint("Error performing subtitution of '== null' in " + condition );
	}
	
	file_string.append("\t\treturn( " + test_string + " ); \n\t}\n}\n");
	
	//write to the file
	try{ 
	  writer.write(file_string.toString());
	  writer.flush();
	}catch(IOException ioe){
	  debugPrint("Error while writing splitter file " + tempdir+splitter_fname+".java \n");
	  debugPrint(ioe.toString());
	}
      }
    }
    return splitternames;
  }

  private PptTopLevel find_corresponding_ppt(String ppt_name, Iterator ppt_itor){
    PptTopLevel return_ppt = null;
    Pattern ppt_pattern = null;
    Pattern object_pattern = null;
    
    try{
      ppt_pattern = re_compiler.compile(ppt_name);
      object_pattern = re_compiler.compile("OBJECT");
    }catch(Exception e){
      debugPrint(e.toString() + " while matching " + ppt_name);
    }
    
    while (ppt_itor.hasNext()){
      String name = ((PptTopLevel)ppt_itor.next()).name;
      if(re_matcher.contains( name, ppt_pattern)){
	return_ppt = all_ppts.get(name);
	break;
      }else if(re_matcher.contains( ppt_name, object_pattern) && re_matcher.contains(name, object_pattern)){
	//this is for the OBJECT program point
	return_ppt = all_ppts.get(name);
	break;
      }
    }
    return return_ppt;
  }
  
  private Vector[] get_params_and_types(PptTopLevel ppt){
    Vector parameters = new Vector();
    Vector types = new Vector();
    VarInfo[] var_infos = ppt.var_infos;
    for(int i = 0; i < var_infos.length; i++){
      String temp = var_infos[i].name.name().trim();
      if(temp.endsWith(".class") || temp.equals("return") || temp.startsWith("orig")) continue;
      if(temp.endsWith("[]")) temp = temp.substring(0, temp.length() - 2);
      parameters.addElement(temp);
      if((var_infos[i].type.format().trim()).equals("boolean")){
	types.addElement(new String("boolean"));
      }else{
	types.addElement(var_infos[i].rep_type.format().trim());
      }
    }
    Vector[] return_vector = {parameters, types};
    return return_vector;
  }

  private String replace_condition(String condition, Vector replace){
    String cond = condition;
    Pattern replace_pattern;
    for(int i = 0; i < replace.size(); i+=2){
      try{
	String temp = (String)replace.elementAt(i) + "\\s*\\(.*\\)" ;
	replace_pattern = re_compiler.compile( temp );
	if(re_matcher.contains(cond, replace_pattern)){
	  Perl5Substitution replace_subst = 
	    new Perl5Substitution( (String)replace.elementAt(i+1), Perl5Substitution.INTERPOLATE_ALL);
	  cond = Util.substitute(re_matcher, replace_pattern, replace_subst, condition, Util.SUBSTITUTE_ALL);
	}
      }catch(Exception e){
	debugPrint( e.toString() +" while performing replacement on condition " + condition);
      }
    }
    return cond;
  }
  public void debugPrint(String s){
    if(Global.debugPptSplit){
      System.out.println(s);
    }
  }
}
