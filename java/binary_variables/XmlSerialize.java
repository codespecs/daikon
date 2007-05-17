package binary_variables;

/**
 * XmlSerialize represents a way to serialize objects into an XML file.
 * Instead of using Seriliazable (which uses a binary file which is not readable in editors),
 * I prefer using XML files for serialization.
 *
 * Date: 16/05/2007
 */
public interface XmlSerialize {
  public XmlWriter toXml();
}
