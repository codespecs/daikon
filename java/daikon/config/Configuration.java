package daikon.config;

import java.lang.reflect.Field;
import java.io.*;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import utilMDE.Assert;

/**
 * This class applies settings from a configuration file that lists
 * variable names and values (see "example-settings.txt" in this directory
 * for an example).  Multiple configuration files can be read, and the
 * results can be re-written to a new configuration file.
 *
 * <p> Important note: classes that have fields set via this
 * Configuration (dkconfig) interface may not reference daikon.Global
 * in their static initializers, since Global loads the default
 * configuration, which classloads that class, and we have a
 * classloading circularity.
 **/
public final class Configuration
  implements Serializable
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  // ============================== STATIC COMPONENT ==============================

  private static final String CONFIGURABLE_LIST = "configurable.txt";
  protected static final String PREFIX = "dkconfig_";

  /**
   * @return singleton instance of this class
   **/
  public static Configuration getInstance() {
    if (instance == null) {
      synchronized (Configuration.class) {
        if (instance == null) {
          instance = new Configuration();
        }
      }
    }
    return instance;
  }
  private static Configuration instance = null;

  /**
   * Load the defaults.
   *
   * In order to permit accurate reconstruction of the current
   * configuration (in a later run, say, or when loading state from a
   * file), the private "statements" list in this class should contain the
   * name and value of every configuration option.  Reading
   * configurable.txt file supplies that list with every configuration
   * option.  (Omitted configuration options receive their default values;
   * but the default might change from one version of Daikon to another.)
   *
   * As of 1/2003, the configurable.txt file is out of date.  It should be
   * automatically generated, or the list of all configuration options
   * should be obtained in some other way, or the problem regarding changes
   * to default values should be simply accepted (and documented).
   **/
  private Configuration() {
    InputStream stream = Configuration.class.getResourceAsStream(CONFIGURABLE_LIST);
    // System.out.println("CONFIGURABLE_LIST stream: " + stream);
    Assert.assertTrue(stream != null, "Cannot load list of configurable "
                      + "fields from '" + CONFIGURABLE_LIST + "'");
    try {

      LineNumberReader lines = new LineNumberReader(new BufferedReader(new InputStreamReader(stream)));
      String line;
      while ((line = lines.readLine()) != null) {
        line = line.trim();
        if (line.length() == 0) continue;    // skip blank lines
        if (line.charAt(0) == '#') continue; // skip comments

        int n = line.lastIndexOf('.');
        String classname = line.substring(0, n);
        String fieldname = line.substring(n+1);
        String unparsed;
        try {
          Class c = Class.forName(classname);
          Field f = c.getField(Configuration.PREFIX + fieldname);
          Object value = f.get(null);
          Assert.assertTrue(value != null);
          unparsed = String.valueOf(value);
        } catch (Exception e) {
          String message = CONFIGURABLE_LIST + ":" + lines.getLineNumber() + ": Error in \"" + line + "\" (warning: actual error may be elsewhere): " + e + daikon.Global.lineSep + utilMDE.UtilMDE.backTrace(new Throwable());
          throw new Error(message);
        } catch (Error e) {     // especially NoClassDefFoundError
          String message = CONFIGURABLE_LIST + ":" + lines.getLineNumber() + ": Error in \"" + line + "\" (warning: actual error may be elsewhere): " + e + daikon.Global.lineSep + utilMDE.UtilMDE.backTrace(new Throwable());
          throw new Error(message);
        }
        addRecord(classname, fieldname, unparsed);
      }

    } catch (IOException e) {
      throw new ConfigException("Cannot read from stream.\n" + e);
    }
  }

  public static class ConfigException extends RuntimeException {
    public ConfigException(String s) { super(s); }
    public ConfigException() { super(); }
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020130L;
  }

  // ============================== REPLAY ==============================

  public void replay() {
    // Make a copy of the statements, since apply mutates the list.
    List copy = new ArrayList(statements);
    Iterator iter = copy.iterator();
    while (iter.hasNext()) {
      String statement = (String) iter.next();
      apply(statement);
    }
    statements = copy;
  }

  /**
   * Take the settings given in the argument and call
   * this.apply(String) for each of them.  This essentially overlaps
   * the settings given in the argument over this (appending them to
   * this in the process).  This method is intended for loading a saved
   * configuration from a file, since calling this method with the
   * Configuration singleton makes no sense.
   **/
  public void overlap(Configuration config) {
    Assert.assertTrue(config != null);
    Iterator iter = config.statements.iterator();
    while (iter.hasNext()) {
      String statement = (String) iter.next();
      this.apply(statement);
    }
  }

  // ============================== ADT COMPONENT ==============================

  private List statements = new ArrayList();

  public void apply(InputStream input)
  {
    Assert.assertTrue(input != null);
    try {

      BufferedReader lines = new BufferedReader(new InputStreamReader(input));
      String line;
      while ((line = lines.readLine()) != null) {
        line = line.trim();
        if (line.length() == 0) continue;    // skip blank lines
        if (line.charAt(0) == '#') continue; // skip # comment lines
        apply(line);
      }

    } catch (IOException e) {
      throw new ConfigException("Cannot read from stream.\n" + e);
    }
  }

  public void apply(String line)
  {
    Assert.assertTrue(line != null);

    int eq = line.indexOf('=');
    if (eq <= 0) {
      throw new RuntimeException("Error, setting must contain \"=\": " + line);
    }

    String name = line.substring(0, eq).trim();
    String value = line.substring(eq+1).trim();

    apply(name, value);
  }

  public void apply(String name, String value)
  {
    Assert.assertTrue(name != null);
    Assert.assertTrue(value != null);

    int dot = name.lastIndexOf('.');
    Assert.assertTrue(dot >= 0, "Name must contain .");

    String classname = name.substring(0, dot);
    String fieldname = name.substring(dot+1);

    apply(classname, fieldname, value);
  }

  public void apply(String classname, String fieldname, String value)
  {
    Assert.assertTrue(classname != null);
    Assert.assertTrue(fieldname != null);
    Assert.assertTrue(value != null);

    Class clazz;
    try {
      clazz = Class.forName(classname);
    } catch (ClassNotFoundException e) {
      throw new ConfigException("No class " + classname);
    } catch (LinkageError e) {
      throw new ConfigException("No class (" + e + ") " + classname);
    }

    apply(clazz, fieldname, value);
  }

  public void apply(Class clazz, String fieldname, String value)
  {
    Assert.assertTrue(clazz != null);
    Assert.assertTrue(fieldname != null);
    Assert.assertTrue(value != null);

    Field field;
    try {
      field = clazz.getDeclaredField(PREFIX + fieldname);
    } catch (SecurityException e) {
      throw new ConfigException("No access to field " + fieldname + " in class " + clazz.getName());
    } catch (NoSuchFieldException e) {
      throw new ConfigException("No such field " + fieldname + " in class " + clazz.getName());
    }

    apply(field, value);
  }

  private void apply(Field field, String unparsed)
  {
    Assert.assertTrue(field != null);
    Assert.assertTrue(unparsed != null);

    Object value; // typed version of value
    Class type = field.getType();

    if (type.equals(Boolean.TYPE) || type.equals(Boolean.class)) {
      if (unparsed.equals("1") || unparsed.equalsIgnoreCase("true")) {
        value = Boolean.TRUE;
      } else {
        value = Boolean.FALSE;
      }
    } else if (type.equals(Integer.TYPE) || type.equals(Integer.class)) {
      try {
        // decode instead of valueOf to handle "0x" and other styles
        value = Integer.decode(unparsed);
      } catch (NumberFormatException e) {
        throw new ConfigException("Unsupported int " + unparsed);
      }
    } else if (type.equals(Long.TYPE) || type.equals(Long.class)) {
      try {
        // decode instead of valueOf to handle "0x" and other styles
        value = Long.decode(unparsed);
      } catch (NumberFormatException e) {
        throw new ConfigException("Unsupported long " + unparsed);
      }
    } else if (type.equals(Float.TYPE) || type.equals(Float.class)) {
      try {
        value = Float.valueOf(unparsed);
      } catch (NumberFormatException e) {
        throw new ConfigException("Unsupported float " + unparsed);
      }
    } else if (type.equals(Double.TYPE) || type.equals(Double.class)) {
      try {
        value = Double.valueOf(unparsed);
      } catch (NumberFormatException e) {
        throw new ConfigException("Unsupported double " + unparsed);
      }
    } else if (type.getName().equals("java.lang.String")) {
      value = unparsed;
    } else {
      throw new ConfigException("Unsupported type " + type.getName());
    }

    try {
      field.set(null, value);
    } catch (IllegalAccessException e) {
      throw new ConfigException("Cannot access " + field);
    }

    // record the application
    String classname = field.getDeclaringClass().getName();
    String fieldname = field.getName();
    Assert.assertTrue(fieldname.startsWith(PREFIX)); // remove the prefix
    fieldname = fieldname.substring(PREFIX.length());
    addRecord(classname, fieldname, unparsed);
  }

  private void addRecord(String classname, String fieldname, String unparsed)
  {
    Assert.assertTrue(! fieldname.startsWith(PREFIX)); // must not have prefix
    String record = classname + "." + fieldname + " = " + unparsed;
    statements.add(record);
  }

}
