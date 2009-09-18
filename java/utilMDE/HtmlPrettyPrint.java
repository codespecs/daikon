package utilMDE;

import java.io.*;

import nu.xom.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;

// To use:  java mde.HtmlPrettyPrint file.html > filepp.html

/**
 * Pretty-prints an HTML file, after converting it to valid XML.
 */
public class HtmlPrettyPrint {

  public static void main(String[] args) {

    for (String arg : args) {
      File f = new File(arg);
      String url = "file://" + f.getAbsolutePath();

      try {
        XMLReader tagsoup = XMLReaderFactory.createXMLReader("org.ccil.cowan.tagsoup.Parser");
        Builder parser = new Builder(tagsoup);

        // Parse the document
        Document document = parser.build(url);

        Serializer serializer = new Serializer(System.out);
        serializer.setIndent(2);
        serializer.setMaxLength(80);
        try {
          serializer.write(document);
        }
        catch (IOException ex) {
          System.err.println(ex);
        }
      }
      catch (ParsingException ex) {
        System.out.println(url + " is not well-formed.");
        throw new Error(ex);
      }
      catch (SAXException ex) {
        System.out.println("Could not load Xerces.");
        System.out.println(ex.getMessage());
      }
      catch (IOException ex) {
        System.out.println("IOException:  parser could not read " + url);
      }
    }
  }

}
