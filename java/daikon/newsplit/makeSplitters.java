package daikon.newsplit;

import utilMDE.*;

import java.io.*;
import java.util.*;
import java.lang.reflect.*;

public class makeSplitters {
    
  static final int NUMERIC = 0, NUMERIC_ARRAY = 1, NUMERIC_INDEX = 2, STRING = 3, STRING_ARRAY = 4;
  
  public static void main(String[] args){
    
    try{
      Vector splitters = read_splitterinfo("SplitterInfo");
    }catch(IOException e){
    }
  }

  //Reads the splitter info, creates the splitter classes and returns a Vector of
  //the class names.
    static Vector read_splitterinfo (String filename) throws IOException, FileNotFoundException {
    String line = "";
    StringTokenizer tokenizer;
    LineNumberReader reader = UtilMDE.LineNumberFileReader(filename);
    String function_name;
    Vector parameters = new Vector(); // parameter names
    Vector types = new Vector();  // types of parameters. Should be same length as parameters.
    Vector conds = new Vector(); // a vector holding the conditions to be tested at that ppt.
    Vector splitters = new Vector();
    while((line = reader.readLine()) != null){
      line.trim();
      if(line == "") break;
      if(line.startsWith("FUNC")){
	tokenizer = new StringTokenizer(line, ":");
	tokenizer.nextToken(); //first token is FUNCTION_NAME
	function_name = ((tokenizer.nextToken()).trim()).replace('.', '_');
	line = reader.readLine(); 
	Assert.assert(line.startsWith("PARA"));
	tokenizer = new StringTokenizer(line, ":");
	tokenizer.nextToken(); //first token is PARAMETER_NAMES
	while(tokenizer.hasMoreTokens()){
	  parameters.addElement((tokenizer.nextToken()).trim());
	}
	line = reader.readLine(); line.trim();
	Assert.assert(line.startsWith("TYPE"));
	tokenizer = new StringTokenizer(line, ":");
	tokenizer.nextToken(); //first token is TYPES
	while(tokenizer.hasMoreTokens()){
	  String typ = (tokenizer.nextToken()).trim();
	  types.addElement(new Integer(typ));
	}
	line = reader.readLine(); line.trim(); 
	Assert.assert(line.startsWith("COND"));
	tokenizer = new StringTokenizer(line, ":");
	tokenizer.nextToken(); //first token is CONDITIONS
	while(tokenizer.hasMoreTokens()){
	  conds.addElement(tokenizer.nextToken());
	}
	for(int i = 0; i < conds.size(); i++){
	  String fname = function_name + "_" + i;
	  write_splitter((String)conds.elementAt(i), fname , parameters, types);
	  splitters.addElement(function_name + ".java");
	}
      }
      conds.clear();
    }
    return splitters;
  }

  static void load_classes(Vector classnames){
  
  }

  static void write_splitter (String condition, String splitter_class, Vector parameters, Vector types) throws IOException { 
    BufferedWriter writer = UtilMDE.BufferedFileWriter(splitter_class+ ".java");
    int len = parameters.size();
    int i;
        
    String import_string = "package daikon.split.newsplit; \n\n";
    import_string += "import daikon.*; \n";
    import_string += "import daikon.split.*; \n\n";
        
    String class_decl_string = "public final class " + splitter_class + " extends Splitter { \n\n";
    
    String cond_string = "  public String condition () { return \"" + condition + "\" ;} \n\n"; 
    
    String param_string = "";
    for(i = 0; i < len; i++){
      param_string += "  VarInfo " + (String)parameters.elementAt(i) + "_varinfo; \n";
    }
    
    // print the constructors
    String constr_string = "\n  public " + splitter_class + "() { } \n";
    constr_string += "  public " + splitter_class + "(Ppt ppt) {  \n";
    for(i = 0; i < len; i++){
      String par =  (String)parameters.elementAt(i);
      constr_string += "    " + par + "_varinfo = ppt.findVar(\"" + par + "\") ; \n";
    }
    constr_string += "  }\n\n";
    
    // print the instantiate method
    String  inst_string = "  public Splitter instantiate(Ppt ppt) { return new ";
    inst_string += splitter_class + "(ppt); } \n\n";
    
    // print the valid() method
    String valid_string = "  public boolean valid() { \n    return(";
    for(i = 0; i < len; i++){
      valid_string += "(" + (String)parameters.elementAt(i) + "_varinfo != null)";
      if( i < len-1){	
	valid_string += " && ";
      }
    }
    
    valid_string += ") ;\n  }\n\n";
    
    //print the test() method
    String test_string = "  public boolean test(ValueTuple vt) { \n";
    for(i = 0; i < len; i++){
      String var =(String)parameters.elementAt(i);
      int typ = ((Integer)types.elementAt(i)).intValue();
      test_string += "    long " + var + " = " + var + "_varinfo"; 
      switch(typ){
      case NUMERIC_INDEX: test_string += ".getIndexValue(vt); \n"; break;
      case NUMERIC: test_string += ".getIntValue(vt); \n"; break;
      case NUMERIC_ARRAY: test_string += ".getIntArrayValue(vt); \n"; break;
      case STRING: test_string += "    String " + var + " = " + var + "_varinfo.getStringValue(vt); \n"; break;
      case STRING_ARRAY: test_string += "    String " + var + " = " + var + "_varinfo.getStringValue(vt); \n"; break;
      default: System.out.println("Can't deal with this type!"); //do some error checking here
      }
    }
    test_string += "    return ( " + condition + " ); \n  }\n}\n";
    
    writer.write(import_string + class_decl_string + cond_string + param_string);
    writer.write(constr_string + inst_string + valid_string + test_string);
    writer.flush();
  } 
}

