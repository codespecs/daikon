package daikon;

import java.io.*;
import java.util.*;
import java.util.zip.GZIPInputStream;
import utilMDE.*;
import daikon.gui.InvariantsGUI; // for InvariantsGUI.PLEASE_REPORT_ERROR_STRING, in getPptMapFromFile()

/** Maps from a name (a String) to a PptTopLevel. */
public class PptMap
  implements Serializable
{
  private final Map nameToPpt = new HashMap();

  public void add(PptTopLevel ppt)
  {
    nameToPpt.put(ppt.name, ppt);
  }

  public PptTopLevel get(String name)
  {
    return (PptTopLevel) nameToPpt.get(name);
  }

  public PptTopLevel get(PptName name)
  {
    return get(name.toString());
  }

  public boolean containsName(String name)
  {
    return nameToPpt.containsKey(name);
  }

  public Collection asCollection()
  {
    return Collections.unmodifiableCollection(nameToPpt.values());
  }

  public Collection nameStringSet()
  {
    return Collections.unmodifiableSet(nameToPpt.keySet());
  }

  public Iterator iterator()
  {
    return asCollection().iterator();
  }

  // // Is this of any interest?  Will I ever call it?
  // // This used to take a "String filename" initial argument.
  // void merge(PptMap other, int other_samples) {
  //   Set other_entries = other.entrySet();
  //   for (Iterator itor = other_entries.iterator() ; itor.hasNext() ;) {
  //     Map.Entry entry = (Map.Entry) itor.next();
  //     String ppt_name = (String) entry.getKey();
  //     // Do I really want to be using Ppt rather than IPpt here??
  //     PptTopLevel other_ppt_info = (PptTopLevel) entry.getValue();
  //     PptTopLevel this_ppt_info = (PptTopLevel) get(ppt_name);
  //     // Would it be acceptable, if (this_ppt_info == null), to
  //     // just do "ppt_map.put(ppt_name, other_ppt_info);" (without
  //     // then having to call merge?  Or do I want to make a copy (or make a
  //     // new object and then call merge) so that other_ppt_info doesn't
  //     // unexpectedly change?  For now, avoid the sharing, to be safe.
  //     if (this_ppt_info == null) {
  //       // This constructor is commented out; uncomment if I want to use it.
  //       this_ppt_info = new PptTopLevel(other_ppt_info);
  //       put(ppt_name, this_ppt_info);
  //     }
  //     this_ppt_info.merge(other_ppt_info);
  //   }
  // }


  // This is used by the GUI, to get the user-specified .inv or .inv.gz file.
  public static PptMap getPptMapFromFile ( String fileName ) throws IOException {
    try {
      InputStream istream = new FileInputStream( fileName );
      if (fileName.endsWith( ".gz" ))
	istream = new GZIPInputStream( istream );
      ObjectInputStream o = new ObjectInputStream( istream );
      PptMap pptMap = (PptMap) o.readObject();
      istream.close();
      return pptMap;
    } catch (Exception e) {
      String errorMessage = "";
      if (e.getClass() == FileNotFoundException.class)
	errorMessage = "Error: Invariants object file not found: " + fileName;
      else if (e.getClass() == StreamCorruptedException.class)
	errorMessage = "Error: Invariants object file is corrupted: " + fileName;
      else if (e.getClass() == InvalidClassException.class)
	errorMessage = "Error: Invalid invariants object file: " + fileName + "\nMake sure the file was made with your current version of Daikon.";
      else
	errorMessage = "Unknown error while reading invariants object file " + fileName + ": " + e.getClass() + InvariantsGUI.PLEASE_REPORT_ERROR_STRING;
      throw new IOException( errorMessage );
    }
  }
}

