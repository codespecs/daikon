package daikon.config;

import java.lang.reflect.Field;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import utilMDE.Assert;

public final class Configuration
{

  // ============================== STATIC COMPONENT ==============================

  private static final String DEFAULTS = "defaults.txt";
  private static final String PREFIX = "dkconfig_";

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
   * Load the defaults
   **/
  private Configuration() {
    InputStream stream = Configuration.class.getResourceAsStream(DEFAULTS);
    Assert.assert(stream != null, "Cannot load defaults from '" + DEFAULTS + "'");
    apply(stream);
  }

  public static class ConfigException extends RuntimeException {
    public ConfigException(String s) { super(s); }
    public ConfigException() { super(); }
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

  // ============================== ADT COMPONENT ==============================

  private List statements = new ArrayList();

  public void apply(InputStream input)
  {
    Assert.assert(input != null);
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
    Assert.assert(line != null);

    int eq = line.indexOf('=');
    Assert.assert(eq >= 0, "Setting must contain =");

    String name = line.substring(0, eq).trim();
    String value = line.substring(eq+1).trim();

    apply(name, value);
  }

  public void apply(String name, String value)
  {
    Assert.assert(name != null);
    Assert.assert(value != null);

    int dot = name.lastIndexOf('.');
    Assert.assert(dot >= 0, "Name must contain .");

    String classname = name.substring(0, dot);
    String fieldname = name.substring(dot+1);

    apply(classname, fieldname, value);
  }

  public void apply(String classname, String fieldname, String value)
  {
    Assert.assert(classname != null);
    Assert.assert(fieldname != null);
    Assert.assert(value != null);

    Class clazz;
    try {
      clazz = Class.forName(classname);
    } catch (ClassNotFoundException e) {
      throw new ConfigException("No class " + classname);
    }

    Field field;
    try {
      field = clazz.getDeclaredField(PREFIX + fieldname);
    } catch (SecurityException e) {
      throw new ConfigException("No access to field " + fieldname);
    } catch (NoSuchFieldException e) {
      throw new ConfigException("No such field " + fieldname);
    }

    apply(field, value);
  }

  private void apply(Field field, String unparsed)
  {
    Assert.assert(field != null);
    Assert.assert(unparsed != null);

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
	value = Integer.decode(unparsed);
      } catch (NumberFormatException e) {
	throw new ConfigException("Unsupported int " + unparsed);
      }
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
    String record = classname + "." + fieldname + " = " + unparsed;
    statements.add(record);
  }

}
