package daikon.mints;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URI;
import java.net.URL;
import java.text.CharacterIterator;
import java.text.StringCharacterIterator;
import java.util.*;

//import java.util.function.Function;

/**
 * <p>
 * Represents a JSON (JavaScript Object Notation) entity. For more information about JSON, please see
 * <a href="http://www.json.org" target="_">http://www.json.org</a>.
 * </p>
 * <p>
 * <p>
 * A JSON entity can be one of several things: an object (set of name/Json entity pairs), an array (a list of
 * other JSON entities), a string, a number, a boolean or null. All of those are represented as <code>Json</code>
 * instances. Each of the different types of entities supports a different set of operations. However, this class
 * unifies all operations into a single interface so in Java one is always dealing with a single object type: this class.
 * The approach effectively amounts to dynamic typing where using an unsupported operation won't be detected at
 * compile time, but will throw a runtime {@link UnsupportedOperationException}. It simplifies working with JSON
 * structures considerably and it leads to shorter at cleaner Java code. It makes much easier to work
 * with JSON structure without the need to convert to "proper" Java representation in the form of
 * POJOs and the like. When traversing a JSON, there's no need to type-cast at each step because there's
 * only one type: <code>Json</code>.
 * </p>
 * <p>
 * <p>
 * One can examine the concrete type of a <code>Json</code> with one of the <code>isXXX</code> methods:
 * {@link #isObject()}, {@link #isArray()},{@link #isNumber()},{@link #isBoolean()},{@link #isString()},
 * {@link #isNull()}.
 * </p>
 * <p>
 * <p>
 * The underlying representation of a given <code>Json</code> instance can be obtained by calling
 * the generic {@link #getValue()} method or one of the <code>asXXX</code> methods such
 * as {@link #asBoolean()} or {@link #asString()} etc.
 * JSON objects are represented as Java {@link Map}s while JSON arrays are represented as Java
 * {@link List}s. Because those are mutable aggregate structures, there are two versions of the
 * corresponding <code>asXXX</code> methods: {@link #asMap()} which performs a deep copy of the underlying
 * map, unwrapping every nested Json entity to its Java representation and {@link #asJsonMap()} which
 * simply return the map reference. Similarly there are {@link #asList()} and {@link #asJsonList()}.
 * </p>
 * <p>
 * <h3>Constructing and Modifying JSON Structures</h3>
 * <p>
 * <p>
 * There are several static factory methods in this class that allow you to create new
 * <code>Json</code> instances:
 * </p>
 * <p>
 * <table>
 * <tr><td>{@link #read(String)}</td>
 * <td>Parse a JSON string and return the resulting <code>Json</code> instance. The syntax
 * recognized is as defined in <a href="http://www.json.org">http://www.json.org</a>.
 * </td>
 * </tr>
 * <tr><td>{@link #make(Object)}</td>
 * <td>Creates a Json instance based on the concrete type of the parameter. The types
 * recognized are null, numbers, primitives, String, Map, Collection, Java arrays
 * and <code>Json</code> itself.</td>
 * </tr>
 * <tr><td>{@link #nil()}</td>
 * <td>Return a <code>Json</code> instance representing JSON <code>null</code>.</td>
 * </tr>
 * <tr><td>{@link #object()}</td>
 * <td>Create and return an empty JSON object.</td>
 * </tr>
 * <tr><td>{@link #object(Object...)}</td>
 * <td>Create and return a JSON object populated with the key/value pairs
 * passed as an argument sequence. Each even parameter becomes a key (via
 * <code>toString</code>) and each odd parameter is converted to a <code>Json</code>
 * value.</td>
 * </tr>
 * <tr><td>{@link #array()}</td>
 * <td>Create and return an empty JSON array.</td>
 * </tr>
 * <tr><td>{@link #array(Object...)}</td>
 * <td>Create and return a JSON array from the list of arguments.</td>
 * </tr>
 * </table>
 * <p>
 * <p>
 * <p>
 * If a <code>Json</code> instance is an object, you can set its properties by
 * calling the {@link #set(String, Object)} method which will add a new property or replace an existing one.
 * Adding elements to an array <code>Json</code> is done with the {@link #add(Object)} method.
 * Removing elements by their index (or key) is done with the {@link #delAt(int)} (or
 * {@link #delAt(String)}) method. You can also remove an element from an array without
 * knowing its index with the {@link #remove(Object)} method. All these methods return the
 * <code>Json</code> instance being manipulated so that method calls can be chained.
 * If you want to remove an element from an object or array and return the removed element
 * as a result of the operation, call {@link #atDel(int)} or {@link #atDel(String)} instead.
 * </p>
 * <p>
 * <p>
 * If you want to add properties to an object in bulk or append a sequence of elements to array,
 * use the {@link Json#with(Json, Json[])} method. When used on an object, this method expects another
 * object as its argument and it will copy all properties of that argument into itself. Similarly,
 * when called on array, the method expects another array and it will append all elements of its
 * argument to itself.
 * </p>
 * <p>
 * <p>
 * To make a clone of a Json object, use the {@link #dup()} method. This method will create a new
 * object even for the immutable primitive Json types. Objects and arrays are cloned
 * (i.e. duplicated) recursively.
 * </p>
 * <p>
 * <h3>Navigating JSON Structures</h3>
 * <p>
 * <p>
 * The {@link #at(int)} method returns the array element at the specified index and the
 * {@link #at(String)} method does the same for a property of an object instance. You can
 * use the {@link #at(String, Object)} version to create an object property with a default
 * value if it doesn't exist already.
 * </p>
 * <p>
 * <p>
 * To test just whether a Json object has a given property, use the {@link #has(String)} method. To test
 * whether a given object property or an array elements is equal to a particular value, use the
 * {@link #is(String, Object)} and {@link #is(int, Object)} methods respectively. Those methods return
 * true if the given named property (or indexed element) is equal to the passed in Object as the second
 * parameter. They return false if an object doesn't have the specified property or an index array is out
 * of bounds. For example is(name, value) is equivalent to 'has(name) &amp;&amp; at(name).equals(make(value))'.
 * </p>
 * <p>
 * <p>
 * To help in navigating JSON structures, instances of this class contain a reference to the
 * enclosing JSON entity (object or array) if any. The enclosing entity can be accessed
 * with {@link #up()} method.
 * </p>
 * <p>
 * <p>
 * The combination of method chaining when modifying <code>Json</code> instances and
 * the ability to navigate "inside" a structure and then go back to the enclosing
 * element lets one accomplish a lot in a single Java statement, without the need
 * of intermediary variables. Here for example how the following JSON structure can
 * be created in one statement using chained calls:
 * </p>
 * <p>
 * <pre><code>
 * {"menu": {
 * "id": "file",
 * "value": "File",
 * "popup": {
 *   "menuitem": [
 *     {"value": "New", "onclick": "CreateNewDoc()"},
 *     {"value": "Open", "onclick": "OpenDoc()"},
 *     {"value": "Close", "onclick": "CloseDoc()"}
 *   ]
 * }
 * "position": 0
 * }}
 * </code></pre>
 * <p>
 * <pre><code>
 * import mjson.Json;
 * import static mjson.Json.*;
 * ...
 * Json j = object()
 *  .at("menu", object())
 *    .set("id", "file")
 *    .set("value", "File")
 *    .at("popup", object())
 *      .at("menuitem", array())
 *        .add(object("value", "New", "onclick", "CreateNewDoc()"))
 *        .add(object("value", "Open", "onclick", "OpenDoc()"))
 *        .add(object("value", "Close", "onclick", "CloseDoc()"))
 *        .up()
 *      .up()
 *    .set("position", 0)
 *  .up();
 * ...
 * </code></pre>
 * <p>
 * <p>
 * If there's no danger of naming conflicts, a static import of the factory methods (<code>
 * import static json.Json.*;</code>) would reduce typing even further and make the code more
 * readable.
 * </p>
 * <p>
 * <h3>Converting to String</h3>
 * <p>
 * <p>
 * To get a compact string representation, simply use the {@link #toString()} method. If you
 * want to wrap it in a JavaScript callback (for JSON with padding), use the {@link #pad(String)}
 * method.
 * </p>
 *
 * @author Borislav Iordanov
 * @version 2.0.0
 */
public class Json implements Iterable<Json> {

  /**
   * <p>
   * This interface defines how <code>Json</code> instances are constructed. There is a
   * default implementation for each kind of <code>Json</code> value, but you can provide
   * your own implementation. For example, you might want a different representation of
   * an object than a regular <code>HashMap</code>. Or you might want string comparison to be
   * case insensitive.
   * </p>
   * <p>
   * <p>
   * In addition, the {@link #make(Object)} method allows you plug-in your own mapping
   * of arbitrary Java objects to <code>Json</code> instances. You might want to implement
   * a Java Beans to JSON mapping or any other JSON serialization that makes sense in your
   * project.
   * </p>
   * <p>
   * <p>
   * To avoid implementing all methods in that interface, you can extend the {@link DefaultMaker}
   * default implementation and simply overwrite the ones you're interested in.
   * </p>
   *
   * @author Borislav Iordanov
   */
  interface Maker {
    /**
     * Construct and return an object representing JSON <code>null</code>. Implementations are
     * free to cache a return the same instance. The resulting value must return
     * <code>true</code> from <code>isNull()</code> and <code>null</code> from
     * <code>getValue()</code>.
     *
     * @return The representation of a JSON <code>null</code> value.
     */
    Json nil();

    /**
     * Construct and return a JSON boolean. The resulting value must return
     * <code>true</code> from <code>isBoolean()</code> and the passed
     * in parameter from <code>getValue()</code>.
     *
     * @param value The boolean value.
     * @return A JSON with <code>isBoolean() == true</code>. Implementations
     * are free to cache and return the same instance for true and false.
     */
    Json bool(boolean value);

    /**
     * Construct and return a JSON string. The resulting value must return
     * <code>true</code> from <code>isString()</code> and the passed
     * in parameter from <code>getValue()</code>.
     *
     * @param value The string to wrap as a JSON value.
     * @return A JSON element with the given string as a value.
     */
    Json string(String value);

    /**
     * Construct and return a JSON number. The resulting value must return
     * <code>true</code> from <code>isNumber()</code> and the passed
     * in parameter from <code>getValue()</code>.
     *
     * @param value The numeric value.
     * @return Json instance representing that value.
     */
    Json number(Number value);

    /**
     * Construct and return a JSON object. The resulting value must return
     * <code>true</code> from <code>isObject()</code> and an implementation
     * of <code>java.util.Map</code> from <code>getValue()</code>.
     *
     * @return An empty JSON object.
     */
    Json object();

    /**
     * Construct and return a JSON object. The resulting value must return
     * <code>true</code> from <code>isArray()</code> and an implementation
     * of <code>java.util.List</code> from <code>getValue()</code>.
     *
     * @return An empty JSON array.
     */
    Json array();

    /**
     * Construct and return a JSON object. The resulting value can be of any
     * JSON type. The method is responsible for examining the type of its
     * argument and performing an appropriate mapping to a <code>Json</code>
     * instance.
     *
     * @param anything An arbitray Java object from which to construct a <code>Json</code>
     *                 element.
     * @return The newly constructed <code>Json</code> instance.
     */
    Json make(Object anything);
  }

  public interface Function<T, R> {

    /**
     * Applies this function to the given argument.
     *
     * @param t the function argument
     * @return the function result
     */
    R apply(T t);
  }

  @Override public Iterator<Json> iterator() {
    return new Iterator<Json>() {
      @Override
      public boolean hasNext() {
        return false;
      }

      @Override
      public Json next() {
        return null;
      }

      @Override
      public void remove() {
      }
    };
  }

  static String fetchContent(URL url) {
    java.io.Reader reader = null;
    try {
      reader = new java.io.InputStreamReader((java.io.InputStream) url.getContent());
      StringBuilder content = new StringBuilder();
      char[] buf = new char[1024];
      for (int n = reader.read(buf); n > -1; n = reader.read(buf))
        content.append(buf, 0, n);
      return content.toString();
    } catch (Exception ex) {
      throw new RuntimeException(ex);
    } finally {
      if (reader != null) try {
        reader.close();
      } catch (Throwable t) {
      }
    }
  }

  static Json resolvePointer(String pointerRepresentation, Json top) {
    String[] parts = pointerRepresentation.split("/");
    Json result = top;
    for (String p : parts) {
      // TODO: unescaping and decoding
      if (p.length() == 0)
        continue;
      p = p.replace("~1", "/").replace("~0", "~");
      if (result.isArray())
        result = result.at(Integer.parseInt(p));
      else if (result.isObject())
        result = result.at(p);
      else
        throw new RuntimeException("Can't resolve pointer " + pointerRepresentation +
          " on document " + top.toString(200));
    }
    return result;
  }

  static URI makeAbsolute(URI base, String ref) throws Exception {
    URI refuri;
    if (base != null && base.getAuthority() != null && !new URI(ref).isAbsolute()) {
      StringBuilder sb = new StringBuilder();
      if (base.getScheme() != null)
        sb.append(base.getScheme()).append("://");
      sb.append(base.getAuthority());
      if (!ref.startsWith("/")) {
        if (ref.startsWith("#"))
          sb.append(base.getPath());
        else {
          int slashIdx = base.getPath().lastIndexOf('/');
          sb.append(slashIdx == -1 ? base.getPath() : base.getPath().substring(0, slashIdx)).append("/");
        }
      }
      refuri = new URI(sb.append(ref).toString());
    } else if (base != null)
      refuri = base.resolve(ref);
    else
      refuri = new URI(ref);
    return refuri;
  }

  static Json resolveRef(URI base,
                         Json refdoc,
                         URI refuri,
                         Map<String, Json> resolved,
                         Map<Json, Json> expanded,
                         Function<URI, Json> uriResolver) throws Exception {
    if (refuri.isAbsolute() &&
      (base == null || !base.isAbsolute() ||
        !base.getScheme().equals(refuri.getScheme()) ||
        !Objects.equals(base.getHost(), refuri.getHost()) ||
        base.getPort() != refuri.getPort() ||
        !base.getPath().equals(refuri.getPath()))) {
      URI docuri = null;
      refuri = refuri.normalize();
      if (refuri.getHost() == null)
        docuri = new URI(refuri.getScheme() + ":" + refuri.getPath());
      else
        docuri = new URI(refuri.getScheme() + "://" + refuri.getHost() +
          ((refuri.getPort() > -1) ? ":" + refuri.getPort() : "") +
          refuri.getPath());
      refdoc = uriResolver.apply(docuri);
      refdoc = expandReferences(refdoc, refdoc, docuri, resolved, expanded, uriResolver);
    }
    if (refuri.getFragment() == null)
      return refdoc;
    else
      return resolvePointer(refuri.getFragment(), refdoc);
  }

  /**
   * <p>
   * Replace all JSON references, as per the http://tools.ietf.org/html/draft-pbryan-zyp-json-ref-03
   * specification, by their referants.
   * </p>
   */
  static Json expandReferences(Json json,
                               Json topdoc,
                               URI base,
                               Map<String, Json> resolved,
                               Map<Json, Json> expanded,
                               Function<URI, Json> uriResolver) throws Exception {
    if (expanded.containsKey(json)) return json;
    if (json.isObject()) {
      if (json.has("id") && json.at("id").isString()) // change scope of nest references
      {
        base = base.resolve(json.at("id").asString());
      }

      if (json.has("$ref")) {
        URI refuri = makeAbsolute(base, json.at("$ref").asString()); // base.resolve(json.at("$ref").asString());
        Json ref = resolved.get(refuri.toString());
        if (ref == null) {
          ref = Json.object();
          resolved.put(refuri.toString(), ref);
          ref.with(resolveRef(base, topdoc, refuri, resolved, expanded, uriResolver));
        }
        json = ref;
      } else {
        for (Map.Entry<String, Json> e : json.asJsonMap().entrySet())
          json.set(e.getKey(), expandReferences(e.getValue(), topdoc, base, resolved, expanded, uriResolver));
      }
    } else if (json.isArray()) {
      for (int i = 0; i < json.asJsonList().size(); i++)
        json.set(i,
          expandReferences(json.at(i), topdoc, base, resolved, expanded, uriResolver));
    }
    expanded.put(json, json);
    return json;
  }

  public static class DefaultMaker implements Maker {
    public Json nil() {
      return Json.topnull;
    }

    public Json bool(boolean x) {
      return new BooleanJson(x ? Boolean.TRUE : Boolean.FALSE, null);
    }

    public Json string(String x) {
      return new StringJson(x, null);
    }

    public Json number(Number x) {
      return new NumberJson(x, null);
    }

    public Json array() {
      return new ArrayJson();
    }

    public Json object() {
      return new ObjectJson();
    }

    public Json make(Object anything) {
      if (anything == null)
        return topnull;
      else if (anything instanceof Json)
        return (Json) anything;
      else if (anything instanceof String)
        return maker().string((String) anything);
      else if (anything instanceof Collection<?>) {
        Json L = array();
        for (Object x : (Collection<?>) anything)
          L.add(maker().make(x));
        return L;
      } else if (anything instanceof Map<?, ?>) {
        Json O = object();
        for (Map.Entry<?, ?> x : ((Map<?, ?>) anything).entrySet())
          O.set(x.getKey().toString(), maker().make(x.getValue()));
        return O;
      } else if (anything instanceof Boolean)
        return maker().bool((Boolean) anything);
      else if (anything instanceof Number)
        return maker().number((Number) anything);
      else if (anything.getClass().isArray()) {
        Class<?> comp = anything.getClass().getComponentType();
        if (!comp.isPrimitive())
          return Json.array((Object[]) anything);
        Json A = array();
        if (boolean.class == comp)
          for (boolean b : (boolean[]) anything) A.add(b);
        else if (byte.class == comp)
          for (byte b : (byte[]) anything) A.add(b);
        else if (char.class == comp)
          for (char b : (char[]) anything) A.add(b);
        else if (short.class == comp)
          for (short b : (short[]) anything) A.add(b);
        else if (int.class == comp)
          for (int b : (int[]) anything) A.add(b);
        else if (long.class == comp)
          for (long b : (long[]) anything) A.add(b);
        else if (float.class == comp)
          for (float b : (float[]) anything) A.add(b);
        else if (double.class == comp)
          for (double b : (double[]) anything) A.add(b);
        return A;
      } else
        throw new IllegalArgumentException("Don't know how to convert to Json : " + anything);
    }
  }


  /**
   * <p>Return the {@link Maker} currently in effect. This is the factory that the {@link #make(Object)} method
   * will dispatch on upon determining the type of its argument. If you already know the type
   * of element to construct, you can avoid the type introspection implicit to the make method
   * and call the factory directly. This will result in an optimization. </p>
   *
   * @return the factory
   */
  public static Maker maker() {
    return Installer.MAKER;
  }


  /**
   * <p>
   * Parse a JSON entity from its string representation.
   * </p>
   *
   * @param jsonAsString A valid JSON representation as per the <a href="http://www.json.org">json.org</a>
   *                     grammar. Cannot be <code>null</code>.
   * @return The JSON entity parsed: an object, array, string, number or boolean, or null. Note that
   * this method will never return the actual Java <code>null</code>.
   */
  public static Json read(String jsonAsString) {
    return (Json) new JsonReader().read(jsonAsString);
  }

  static class Installer {
    static final Maker MAKER = new DefaultMaker();
  }

  /**
   * <p>
   * Parse a JSON entity from a <code>URL</code>.
   * </p>
   *
   * @param location A valid URL where to load a JSON document from. Cannot be <code>null</code>.
   * @return The JSON entity parsed: an object, array, string, number or boolean, or null. Note that
   * this method will never return the actual Java <code>null</code>.
   */
  public static Json read(URL location) {
    return (Json) new JsonReader().read(fetchContent(location));
  }

  /**
   * <p>
   * Parse a JSON entity from a {@link CharacterIterator}.
   * </p>
   *
   * @param it A character iterator.
   * @return the parsed JSON element
   * @see #read(String)
   */
  public static Json read(CharacterIterator it) {
    return (Json) new JsonReader().read(it);
  }

  /**
   * @return the <code>null Json</code> instance.
   */
  public static Json nil() {
    return maker().nil();
  }

  /**
   * @return a newly constructed, empty JSON object.
   */
  public static Json object() {
    return maker().object();
  }

  /**
   * <p>Return a new JSON object initialized from the passed list of
   * name/value pairs. The number of arguments must
   * be even. Each argument at an even position is taken to be a name
   * for the following value. The name arguments are normally of type
   * Java String, but they can be of any other type having an appropriate
   * <code>toString</code> method. Each value is first converted
   * to a <code>Json</code> instance using the {@link #make(Object)} method.
   * </p>
   *
   * @param args A sequence of name value pairs.
   * @return the new JSON object.
   */
  public static Json object(Object... args) {
    Json j = object();
    if (args.length % 2 != 0)
      throw new IllegalArgumentException("An even number of arguments is expected.");
    for (int i = 0; i < args.length; i++)
      j.set(args[i].toString(), maker().make(args[++i]));
    return j;
  }

  /**
   * @return a new constructed, empty JSON array.
   */
  public static Json array() {
    return maker().array();
  }

  /**
   * <p>Return a new JSON array filled up with the list of arguments.</p>
   *
   * @param args The initial content of the array.
   * @return the new JSON array
   */
  public static Json array(Object... args) {
    Json A = array();
    for (Object x : args)
      A.add(maker().make(x));
    return A;
  }

  /**
   * <p>
   * Exposes some internal methods that are useful for {@link Maker} implementations
   * or other extension/layers of the library.
   * </p>
   *
   * @author Borislav Iordanov
   */
  public static class help {
    /**
     * <p>
     * Perform JSON escaping so that ", <, >, etc. characters are properly encoded in the
     * JSON string representation before returning to the client code. This is useful when
     * serializing property names or string values.
     * </p>
     */
    public static String escape(String string) {
      return stringEscaper.escapeJsonString(string);
    }

    /**
     * <p>
     * Given a JSON Pointer, as per RFC 6901, return the nested JSON value within
     * the <code>element</code> parameter.
     * </p>
     */
    public static Json resolvePointer(String pointer, Json element) {
      return Json.resolvePointer(pointer, element);
    }
  }

  static class JsonSingleValueIterator implements Iterator<Json> {
    private boolean retrieved = false;

    @Override
    public boolean hasNext() {
      return !retrieved;
    }

    @Override
    public Json next() {
      retrieved = true;
      return null;
    }

    @Override
    public void remove() {
    }
  }


  /**
   * <p>
   * Convert an arbitrary Java instance to a {@link Json} instance.
   * </p>
   * <p>
   * <p>
   * Maps, Collections and arrays are recursively copied where each of
   * their elements concerted into <code>Json</code> instances as well. The keys
   * of a {@link Map} parameter are normally strings, but anything with a meaningful
   * <code>toString</code> implementation will work as well.
   * </p>
   *
   * @param anything Any Java object that the current JSON factory in effect is capable of handling.
   * @return The <code>Json</code>. This method will never return <code>null</code>. It will
   * throw an {@link IllegalArgumentException} if it doesn't know how to convert the argument
   * to a <code>Json</code> instance.
   * @throws IllegalArgumentException when the concrete type of the parameter is
   *                                  unknown.
   */
  public static Json make(Object anything) {
    return maker().make(anything);
  }

  // end of static utility method section

  Json enclosing = null;

  protected Json() {
  }

  protected Json(Json enclosing) {
    this.enclosing = enclosing;
  }

  /**
   * <p>Return a string representation of <code>this</code> that does
   * not exceed a certain maximum length. This is useful in constructing
   * error messages or any other place where only a "preview" of the
   * JSON element should be displayed. Some JSON structures can get
   * very large and this method will help avoid string serializing
   * the whole of them. </p>
   *
   * @param maxCharacters The maximum number of characters for
   *                      the string representation.
   * @return The string representation of this object.
   */
  public String toString(int maxCharacters) {
    return toString();
  }

  /**
   * <p>Explicitly set the parent of this element. The parent is presumably an array
   * or an object. Normally, there's no need to call this method as the parent is
   * automatically set by the framework. You may need to call it however, if you implement
   * your own {@link Maker} with your own implementations of the Json types.
   * </p>
   *
   * @param enclosing The parent element.
   */
  public void attachTo(Json enclosing) {
    this.enclosing = enclosing;
  }

  /**
   * @return the <code>Json</code> entity, if any, enclosing this
   * <code>Json</code>. The returned value can be <code>null</code> or
   * a <code>Json</code> object or list, but not one of the primitive types.
   */
  public final Json up() {
    return enclosing;
  }

  /**
   * @return a clone (a duplicate) of this <code>Json</code> entity. Note that cloning
   * is deep if array and objects. Primitives are also cloned, even though their values are immutable
   * because the new enclosing entity (the result of the {@link #up()} method) may be different.
   * since they are immutable.
   */
  public Json dup() {
    return this;
  }

  /**
   * <p>Return the <code>Json</code> element at the specified index of this
   * <code>Json</code> array. This method applies only to Json arrays.
   * </p>
   *
   * @param index The index of the desired element.
   * @return The JSON element at the specified index in this array.
   */
  public Json at(int index) {
    throw new UnsupportedOperationException();
  }

  /**
   * <p>
   * Return the specified property of a <code>Json</code> object or <code>null</code>
   * if there's no such property. This method applies only to Json objects.
   * </p>
   *
   * @param property The property name.
   * @return The JSON element that is the value of that property.
   */
  public Json at(String property) {
    throw new UnsupportedOperationException();
  }

  /**
   * <p>
   * Return the specified property of a <code>Json</code> object if it exists.
   * If it doesn't, then create a new property with value the <code>def</code>
   * parameter and return that parameter.
   * </p>
   *
   * @param property The property to return.
   * @param def      The default value to set and return in case the property doesn't exist.
   */
  public final Json at(String property, Json def) {
    Json x = at(property);
    if (x == null) {
//			set(property, def);
      return def;
    } else
      return x;
  }

  /**
   * <p>
   * Return the specified property of a <code>Json</code> object if it exists.
   * If it doesn't, then create a new property with value the <code>def</code>
   * parameter and return that parameter.
   * </p>
   *
   * @param property The property to return.
   * @param def      The default value to set and return in case the property doesn't exist.
   */
  public final Json at(String property, Object def) {
    return at(property, make(def));
  }

  /**
   * <p>
   * Return true if this <code>Json</code> object has the specified property
   * and false otherwise.
   * </p>
   *
   * @param property The name of the property.
   */
  public boolean has(String property) {
    throw new UnsupportedOperationException();
  }

  /**
   * <p>
   * Return <code>true</code> if and only if this <code>Json</code> object has a property with
   * the specified value. In particular, if the object has no such property <code>false</code> is returned.
   * </p>
   *
   * @param property The property name.
   * @param value    The value to compare with. Comparison is done via the equals method.
   *                 If the value is not an instance of <code>Json</code>, it is first converted to
   *                 such an instance.
   * @return
   */
  public boolean is(String property, Object value) {
    throw new UnsupportedOperationException();
  }

  /**
   * <p>
   * Return <code>true</code> if and only if this <code>Json</code> array has an element with
   * the specified value at the specified index. In particular, if the array has no element at
   * this index, <code>false</code> is returned.
   * </p>
   *
   * @param index The 0-based index of the element in a JSON array.
   * @param value The value to compare with. Comparison is done via the equals method.
   *              If the value is not an instance of <code>Json</code>, it is first converted to
   *              such an instance.
   * @return
   */
  public boolean is(int index, Object value) {
    throw new UnsupportedOperationException();
  }

  /**
   * <p>
   * Add the specified <code>Json</code> element to this array.
   * </p>
   *
   * @return this
   */
  public Json add(Json el) {
    throw new UnsupportedOperationException();
  }

  /**
   * <p>
   * Add an arbitrary Java object to this <code>Json</code> array. The object
   * is first converted to a <code>Json</code> instance by calling the static
   * {@link #make} method.
   * </p>
   *
   * @param anything Any Java object that can be converted to a Json instance.
   * @return this
   */
  public final Json add(Object anything) {
    return add(make(anything));
  }

  /**
   * <p>
   * Remove the specified property from a <code>Json</code> object and return
   * that property.
   * </p>
   *
   * @param property The property to be removed.
   * @return The property value or <code>null</code> if the object didn't have such
   * a property to begin with.
   */
  public Json atDel(String property) {
    throw new UnsupportedOperationException();
  }

  /**
   * <p>
   * Remove the element at the specified index from a <code>Json</code> array and return
   * that element.
   * </p>
   *
   * @param index The index of the element to delete.
   * @return The element value.
   */
  public Json atDel(int index) {
    throw new UnsupportedOperationException();
  }

  /**
   * <p>
   * Delete the specified property from a <code>Json</code> object.
   * </p>
   *
   * @param property The property to be removed.
   * @return this
   */
  public Json delAt(String property) {
    throw new UnsupportedOperationException();
  }

  /**
   * <p>
   * Remove the element at the specified index from a <code>Json</code> array.
   * </p>
   *
   * @param index The index of the element to delete.
   * @return this
   */
  public Json delAt(int index) {
    throw new UnsupportedOperationException();
  }

  /**
   * <p>
   * Remove the specified element from a <code>Json</code> array.
   * </p>
   *
   * @param el The element to delete.
   * @return this
   */
  public Json remove(Json el) {
    throw new UnsupportedOperationException();
  }

  /**
   * <p>
   * Remove the specified Java object (converted to a Json instance)
   * from a <code>Json</code> array. This is equivalent to
   * <code>remove({@link #make(Object)})</code>.
   * </p>
   *
   * @param anything The object to delete.
   * @return this
   */
  public final Json remove(Object anything) {
    return remove(make(anything));
  }

  /**
   * <p>
   * Set a <code>Json</code> objects's property.
   * </p>
   *
   * @param property The property name.
   * @param value    The value of the property.
   * @return this
   */
  public Json set(String property, Json value) {
    throw new UnsupportedOperationException();
  }

  /**
   * <p>
   * Set a <code>Json</code> objects's property.
   * </p>
   *
   * @param property The property name.
   * @param value    The value of the property, converted to a <code>Json</code> representation
   *                 with {@link #make}.
   * @return this
   */
  public final Json set(String property, Object value) {
    return set(property, make(value));
  }

  /**
   * <p>
   * Change the value of a JSON array element. This must be an array.
   * </p>
   *
   * @param index 0-based index of the element in the array.
   * @param value the new value of the element
   * @return this
   */
  public Json set(int index, Object value) {
    throw new UnsupportedOperationException();
  }

  /**
   * <p>
   * Combine this object or array with the passed in object or array. The types of
   * <code>this</code> and the <code>object</code> argument must match. If both are
   * <code>Json</code> objects, all properties of the parameter are added to <code>this</code>.
   * If both are arrays, all elements of the parameter are appended to <code>this</code>
   * </p>
   *
   * @param object  The object or array whose properties or elements must be added to this
   *                Json object or array.
   * @param options A sequence of options that governs the merging process.
   * @return this
   */
  public Json with(Json object, Json[] options) {
    throw new UnsupportedOperationException();
  }

  /**
   * Same as <code>{}@link #with(Json,Json...options)}</code> with each option
   * argument converted to <code>Json</code> first.
   */
  public Json with(Json object, Object... options) {
    Json[] jopts = new Json[options.length];
    for (int i = 0; i < jopts.length; i++)
      jopts[i] = make(options[i]);
    return with(object, jopts);
  }

  /**
   * @return the underlying value of this <code>Json</code> entity. The actual value will
   * be a Java Boolean, String, Number, Map, List or null. For complex entities (objects
   * or arrays), the method will perform a deep copy and extra underlying values recursively
   * for all nested elements.
   */
  public Object getValue() {
    throw new UnsupportedOperationException();
  }

  /**
   * @return the boolean value of a boolean <code>Json</code> instance. Call
   * {@link #isBoolean()} first if you're not sure this instance is indeed a
   * boolean.
   */
  public boolean asBoolean() {
    throw new UnsupportedOperationException();
  }

  /**
   * @return the string value of a string <code>Json</code> instance. Call
   * {@link #isString()} first if you're not sure this instance is indeed a
   * string.
   */
  public String asString() {
    throw new UnsupportedOperationException();
  }

  /**
   * @return the integer value of a number <code>Json</code> instance. Call
   * {@link #isNumber()} first if you're not sure this instance is indeed a
   * number.
   */
  public int asInteger() {
    throw new UnsupportedOperationException();
  }

  /**
   * @return the float value of a float <code>Json</code> instance. Call
   * {@link #isNumber()} first if you're not sure this instance is indeed a
   * number.
   */
  public float asFloat() {
    throw new UnsupportedOperationException();
  }

  /**
   * @return the double value of a number <code>Json</code> instance. Call
   * {@link #isNumber()} first if you're not sure this instance is indeed a
   * number.
   */
  public double asDouble() {
    throw new UnsupportedOperationException();
  }

  /**
   * @return the long value of a number <code>Json</code> instance. Call
   * {@link #isNumber()} first if you're not sure this instance is indeed a
   * number.
   */
  public long asLong() {
    throw new UnsupportedOperationException();
  }

  /**
   * @return the short value of a number <code>Json</code> instance. Call
   * {@link #isNumber()} first if you're not sure this instance is indeed a
   * number.
   */
  public short asShort() {
    throw new UnsupportedOperationException();
  }

  /**
   * @return the byte value of a number <code>Json</code> instance. Call
   * {@link #isNumber()} first if you're not sure this instance is indeed a
   * number.
   */
  public byte asByte() {
    throw new UnsupportedOperationException();
  }

  /**
   * @return the first character of a string <code>Json</code> instance. Call
   * {@link #isString()} first if you're not sure this instance is indeed a
   * string.
   */
  public char asChar() {
    throw new UnsupportedOperationException();
  }

  /**
   * @return a map of the properties of an object <code>Json</code> instance. The map
   * is a clone of the object and can be modified safely without affecting it. Call
   * {@link #isObject()} first if you're not sure this instance is indeed a
   * <code>Json</code> object.
   */
  public Map<String, Object> asMap() {
    throw new UnsupportedOperationException();
  }

  /**
   * @return the underlying map of properties of a <code>Json</code> object. The returned
   * map is the actual object representation so any modifications to it are modifications
   * of the <code>Json</code> object itself. Call
   * {@link #isObject()} first if you're not sure this instance is indeed a
   * <code>Json</code> object.
   */
  public Map<String, Json> asJsonMap() {
    throw new UnsupportedOperationException();
  }

  /**
   * @return a list of the elements of a <code>Json</code> array. The list is a clone
   * of the array and can be modified safely without affecting it. Call
   * {@link #isArray()} first if you're not sure this instance is indeed a
   * <code>Json</code> array.
   */
  public List<Object> asList() {
    throw new UnsupportedOperationException();
  }

  /**
   * @return the underlying {@link List} representation of a <code>Json</code> array.
   * The returned list is the actual array representation so any modifications to it
   * are modifications of the <code>Json</code> array itself. Call
   * {@link #isArray()} first if you're not sure this instance is indeed a
   * <code>Json</code> array.
   */
  public List<Json> asJsonList() {
    throw new UnsupportedOperationException();
  }

  /**
   * @return <code>true</code> if this is a <code>Json</code> null entity
   * and <code>false</code> otherwise.
   */
  public boolean isNull() {
    return false;
  }

  /**
   * @return <code>true</code> if this is a <code>Json</code> string entity
   * and <code>false</code> otherwise.
   */
  public boolean isString() {
    return false;
  }

  /**
   * @return <code>true</code> if this is a <code>Json</code> number entity
   * and <code>false</code> otherwise.
   */
  public boolean isNumber() {
    return false;
  }

  /**
   * @return <code>true</code> if this is a <code>Json</code> boolean entity
   * and <code>false</code> otherwise.
   */
  public boolean isBoolean() {
    return false;
  }

  /**
   * @return <code>true</code> if this is a <code>Json</code> array (i.e. list) entity
   * and <code>false</code> otherwise.
   */
  public boolean isArray() {
    return false;
  }

  /**
   * @return <code>true</code> if this is a <code>Json</code> object entity
   * and <code>false</code> otherwise.
   */
  public boolean isObject() {
    return false;
  }

  /**
   * @return <code>true</code> if this is a <code>Json</code> primitive entity
   * (one of string, number or boolean) and <code>false</code> otherwise.
   */
  public boolean isPrimitive() {
    return isString() || isNumber() || isBoolean();
  }

  /**
   * <p>
   * Json-pad this object as an argument to a callback function.
   * </p>
   *
   * @param callback The name of the callback function. Can be null or empty,
   *                 in which case no padding is done.
   * @return The jsonpadded, stringified version of this object if the <code>callback</code>
   * is not null or empty, or just the stringified version of the object.
   */
  public String pad(String callback) {
    return (callback != null && callback.length() > 0)
      ? callback + "(" + toString() + ");"
      : toString();
  }

  //-------------------------------------------------------------------------
  // END OF PUBLIC INTERFACE
  //-------------------------------------------------------------------------

  /**
   * Return an object representing the complete configuration
   * of a merge. The properties of the object represent paths
   * of the JSON structure being merged and the values represent
   * the set of options that apply to each path.
   *
   * @param options the configuration options
   * @return the configuration object
   */
  protected Json collectWithOptions(Json... options) {
    Json result = object();
    for (Json opt : options) {
      if (opt.isString()) {
        if (!result.has(""))
          result.set("", object());
        result.at("").set(opt.asString(), true);
      } else {
        if (!opt.has("for"))
          opt.set("for", array(""));
        Json forPaths = opt.at("for");
        if (!forPaths.isArray())
          forPaths = array(forPaths);
        for (Json path : forPaths.asJsonList()) {
          if (!result.has(path.asString()))
            result.set(path.asString(), object());
          Json at_path = result.at(path.asString());
          at_path.set("merge", opt.is("merge", true));
          at_path.set("dup", opt.is("dup", true));
          at_path.set("sort", opt.is("sort", true));
          at_path.set("compareBy", opt.at("compareBy", nil()));
        }
      }
    }
    return result;
  }

  static class NullJson extends Json {
    private static final long serialVersionUID = 1L;

    NullJson() {
    }

    NullJson(Json e) {
      super(e);
    }

    public Object getValue() {
      return null;
    }

    public Json dup() {
      return new NullJson();
    }

    public boolean isNull() {
      return true;
    }

    public String toString() {
      return "null";
    }

    public List<Object> asList() {
      return Collections.singletonList(null);
    }

    public int hashCode() {
      return 0;
    }

    public boolean equals(Object x) {
      return x instanceof NullJson;
    }

    @Override
    public Iterator<Json> iterator() {
      return new JsonSingleValueIterator() {
        @Override
        public Json next() {
          super.next();
          return NullJson.this;
        }
      };
    }

  }

  static NullJson topnull = new NullJson();

  /**
   * <p>
   * Set the parent (i.e. enclosing element) of Json element.
   * </p>
   *
   * @param el
   * @param parent
   */
  static void setParent(Json el, Json parent) {
    if (el.enclosing == null)
      el.enclosing = parent;
    else if (el.enclosing instanceof ParentArrayJson)
      ((ParentArrayJson) el.enclosing).L.add(parent);
    else {
      ParentArrayJson A = new ParentArrayJson();
      A.L.add(el.enclosing);
      A.L.add(parent);
      el.enclosing = A;
    }
  }

  /**
   * <p>
   * Remove/unset the parent (i.e. enclosing element) of Json element.
   * </p>
   *
   * @param el
   * @param parent
   */
  static void removeParent(Json el, Json parent) {
    if (el.enclosing == parent)
      el.enclosing = null;
    else if (el.enclosing.isArray()) {
      ArrayJson A = (ArrayJson) el.enclosing;
      int idx = 0;
      while (A.L.get(idx) != parent && idx < A.L.size()) idx++;
      if (idx < A.L.size())
        A.L.remove(idx);
    }
  }

  static class BooleanJson extends Json {
    private static final long serialVersionUID = 1L;

    boolean val;

    BooleanJson() {
    }

    BooleanJson(Json e) {
      super(e);
    }

    BooleanJson(Boolean val, Json e) {
      super(e);
      this.val = val;
    }

    public Object getValue() {
      return val;
    }

    public Json dup() {
      return new BooleanJson(val, null);
    }

    public boolean asBoolean() {
      return val;
    }

    public boolean isBoolean() {
      return true;
    }

    public String toString() {
      return val ? "true" : "false";
    }

    @SuppressWarnings("unchecked")
    public List<Object> asList() {
      return (List<Object>) (List<?>) Collections.singletonList(val);
    }

    public int hashCode() {
      return val ? 1 : 0;
    }

    public boolean equals(Object x) {
      return x instanceof BooleanJson && ((BooleanJson) x).val == val;
    }

    @Override
    public Iterator<Json> iterator() {
      return new JsonSingleValueIterator() {
        @Override
        public Json next() {
          super.next();
          return BooleanJson.this;
        }
      };
    }

  }

  static class StringJson extends Json {
    private static final long serialVersionUID = 1L;

    String val;

    StringJson() {
    }

    StringJson(Json e) {
      super(e);
    }

    StringJson(String val, Json e) {
      super(e);
      this.val = val;
    }

    public Json dup() {
      return new StringJson(val, null);
    }

    public boolean isString() {
      return true;
    }

    public Object getValue() {
      return val;
    }

    public String asString() {
      return val;
    }

    public int asInteger() {
      return Integer.parseInt(val);
    }

    public float asFloat() {
      return Float.parseFloat(val);
    }

    public double asDouble() {
      return Double.parseDouble(val);
    }

    public long asLong() {
      return Long.parseLong(val);
    }

    public short asShort() {
      return Short.parseShort(val);
    }

    public byte asByte() {
      return Byte.parseByte(val);
    }

    public char asChar() {
      return val.charAt(0);
    }

    @SuppressWarnings("unchecked")
    public List<Object> asList() {
      return (List<Object>) (List<?>) Collections.singletonList(val);
    }

    public String toString() {
      return '"' + stringEscaper.escapeJsonString(val) + '"';
    }

    public String toString(int maxCharacters) {
      if (val.length() <= maxCharacters)
        return toString();
      else
        return '"' + stringEscaper.escapeJsonString(val.subSequence(0, maxCharacters)) + "...\"";
    }

    public int hashCode() {
      return val.hashCode();
    }

    public boolean equals(Object x) {
      return x instanceof StringJson && ((StringJson) x).val.equals(val);
    }

    @Override
    public Iterator<Json> iterator() {
      return new JsonSingleValueIterator() {
        @Override
        public Json next() {
          super.next();
          return StringJson.this;
        }
      };
    }

  }

  static class NumberJson extends Json {
    private static final long serialVersionUID = 1L;

    Number val;

    NumberJson() {
    }

    NumberJson(Json e) {
      super(e);
    }

    NumberJson(Number val, Json e) {
      super(e);
      this.val = val;
    }

    public Json dup() {
      return new NumberJson(val, null);
    }

    public boolean isNumber() {
      return true;
    }

    public Object getValue() {
      return val;
    }

    public String asString() {
      return val.toString();
    }

    public int asInteger() {
      return val.intValue();
    }

    public float asFloat() {
      return val.floatValue();
    }

    public double asDouble() {
      return val.doubleValue();
    }

    public long asLong() {
      return val.longValue();
    }

    public short asShort() {
      return val.shortValue();
    }

    public byte asByte() {
      return val.byteValue();
    }

    @SuppressWarnings("unchecked")
    public List<Object> asList() {
      return (List<Object>) (List<?>) Collections.singletonList(val);
    }

    public String toString() {
      return val.toString();
    }

    public int hashCode() {
      return val.hashCode();
    }

    public boolean equals(Object x) {
      return x instanceof NumberJson && val.doubleValue() == ((NumberJson) x).val.doubleValue();
    }

    @Override
    public Iterator<Json> iterator() {
      return new JsonSingleValueIterator() {
        @Override
        public Json next() {
          super.next();
          return NumberJson.this;
        }
      };
    }

  }

  static class ArrayJson extends Json {
    private static final long serialVersionUID = 1L;

    List<Json> L = new ArrayList<Json>();

    ArrayJson() {
    }

    ArrayJson(Json e) {
      super(e);
    }

    @Override
    public Iterator<Json> iterator() {
      return L.iterator();
    }

    public Json dup() {
      ArrayJson j = new ArrayJson();
      for (Json e : L) {
        Json v = e.dup();
        v.enclosing = j;
        j.L.add(v);
      }
      return j;
    }

    public Json set(int index, Object value) {
      Json jvalue = make(value);
      L.set(index, jvalue);
      setParent(jvalue, this);
      return this;
    }

    public List<Json> asJsonList() {
      return L;
    }

    public List<Object> asList() {
      ArrayList<Object> A = new ArrayList<Object>();
      for (Json x : L)
        A.add(x.getValue());
      return A;
    }

    public boolean is(int index, Object value) {
      if (index < 0 || index >= L.size())
        return false;
      else
        return L.get(index).equals(make(value));
    }

    public Object getValue() {
      return asList();
    }

    public boolean isArray() {
      return true;
    }

    public Json at(int index) {
      return L.get(index);
    }

    public Json add(Json el) {
      L.add(el);
      setParent(el, this);
      return this;
    }

    public Json remove(Json el) {
      L.remove(el);
      el.enclosing = null;
      return this;
    }

    boolean isEqualJson(Json left, Json right) {
      if (left == null)
        return right == null;
      else
        return left.equals(right);
    }

    boolean isEqualJson(Json left, Json right, Json fields) {
      if (fields.isNull())
        return left.equals(right);
      else if (fields.isString())
        return isEqualJson(resolvePointer(fields.asString(), left),
          resolvePointer(fields.asString(), right));
      else if (fields.isArray()) {
        for (Json field : fields.asJsonList())
          if (!isEqualJson(resolvePointer(field.asString(), left),
            resolvePointer(field.asString(), right)))
            return false;
        return true;
      } else
        throw new IllegalArgumentException("Compare by options should be either a property name or an array of property names: " + fields);
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    int compareJson(Json left, Json right, Json fields) {
      if (fields.isNull())
        return ((Comparable) left.getValue()).compareTo(right.getValue());
      else if (fields.isString()) {
        Json leftProperty = resolvePointer(fields.asString(), left);
        Json rightProperty = resolvePointer(fields.asString(), right);
        return ((Comparable) leftProperty).compareTo(rightProperty);
      } else if (fields.isArray()) {
        for (Json field : fields.asJsonList()) {
          Json leftProperty = resolvePointer(field.asString(), left);
          Json rightProperty = resolvePointer(field.asString(), right);
          int result = ((Comparable) leftProperty).compareTo(rightProperty);
          if (result != 0)
            return result;
        }
        return 0;
      } else
        throw new IllegalArgumentException("Compare by options should be either a property name or an array of property names: " + fields);
    }

    Json withOptions(Json array, Json allOptions, String path) {
      Json opts = allOptions.at(path, object());
      boolean dup = opts.is("dup", true);
      Json compareBy = opts.at("compareBy", nil());
      if (opts.is("sort", true)) {
        int thisIndex = 0, thatIndex = 0;
        while (thatIndex < array.asJsonList().size()) {
          Json thatElement = array.at(thatIndex);
          if (thisIndex == L.size()) {
            L.add(dup ? thatElement.dup() : thatElement);
            thisIndex++;
            thatIndex++;
            continue;
          }
          int compared = compareJson(at(thisIndex), thatElement, compareBy);
          if (compared < 0) // this < that
            thisIndex++;
          else if (compared > 0) // this > that
          {
            L.add(thisIndex, dup ? thatElement.dup() : thatElement);
            thatIndex++;
          } else { // equal, ignore
            thatIndex++;
          }
        }
      } else {
        for (Json thatElement : array.asJsonList()) {
          boolean present = false;
          for (Json thisElement : L)
            if (isEqualJson(thisElement, thatElement, compareBy)) {
              present = true;
              break;
            }
          if (!present)
            L.add(dup ? thatElement.dup() : thatElement);
        }
      }
      return this;
    }

    public Json with(Json object, Json... options) {
      if (object == null) return this;
      if (!object.isArray())
        add(object);
      else if (options.length > 0) {
        Json O = collectWithOptions(options);
        return withOptions(object, O, "");
      } else
        // what about "enclosing" here? we don't have a provision where a Json
        // element belongs to more than one enclosing elements...
        L.addAll(((ArrayJson) object).L);
      return this;
    }

    public Json atDel(int index) {
      Json el = L.remove(index);
      if (el != null)
        el.enclosing = null;
      return el;
    }

    public Json delAt(int index) {
      Json el = L.remove(index);
      if (el != null)
        el.enclosing = null;
      return this;
    }

    public String toString() {
      return toString(Integer.MAX_VALUE);
    }

    public String toString(int maxCharacters) {
      return toStringImpl(maxCharacters, new IdentityHashMap<Json, Json>());
    }

    String toStringImpl(int maxCharacters, Map<Json, Json> done) {
      StringBuilder sb = new StringBuilder("[");
      for (Iterator<Json> i = L.iterator(); i.hasNext(); ) {
        Json value = i.next();
        String s = value.isObject() ? ((ObjectJson) value).toStringImpl(maxCharacters, done)
          : value.isArray() ? ((ArrayJson) value).toStringImpl(maxCharacters, done)
          : value.toString(maxCharacters);
        if (sb.length() + s.length() > maxCharacters)
          s = s.substring(0, Math.max(0, maxCharacters - sb.length()));
        else
          sb.append(s);
        if (i.hasNext())
          sb.append(",");
        if (sb.length() >= maxCharacters) {
          sb.append("...");
          break;
        }
      }
      sb.append("]");
      return sb.toString();
    }

    public int hashCode() {
      return L.hashCode();
    }

    public boolean equals(Object x) {
      return x instanceof ArrayJson && ((ArrayJson) x).L.equals(L);
    }
  }

  static class ParentArrayJson extends ArrayJson {

    /**
     *
     */
    private static final long serialVersionUID = 1L;

  }

  static class ObjectJson extends Json {
    private static final long serialVersionUID = 1L;

    Map<String, Json> object = new HashMap<String, Json>();

    @Override public Iterator<Json> iterator() {
      return object.values().iterator();
    }

    ObjectJson() {
    }

    ObjectJson(Json e) {
      super(e);
    }

    public Json dup() {
      ObjectJson j = new ObjectJson();
      for (Map.Entry<String, Json> e : object.entrySet()) {
        Json v = e.getValue().dup();
        v.enclosing = j;
        j.object.put(e.getKey(), v);
      }
      return j;
    }

    public boolean has(String property) {
      return object.containsKey(property);
    }

    public boolean is(String property, Object value) {
      Json p = object.get(property);
      if (p == null)
        return false;
      else
        return p.equals(make(value));
    }

    public Json at(String property) {
      return object.get(property);
    }

    protected Json withOptions(Json other, Json allOptions, String path) {
      if (!allOptions.has(path))
        allOptions.set(path, object());
      Json options = allOptions.at(path, object());
      boolean duplicate = options.is("dup", true);
      if (options.is("merge", true)) {
        for (Map.Entry<String, Json> e : other.asJsonMap().entrySet()) {
          Json local = object.get(e.getKey());
          if (local instanceof ObjectJson)
            ((ObjectJson) local).withOptions(e.getValue(), allOptions, path + "/" + e.getKey());
          else if (local instanceof ArrayJson)
            ((ArrayJson) local).withOptions(e.getValue(), allOptions, path + "/" + e.getKey());
          else
            set(e.getKey(), duplicate ? e.getValue().dup() : e.getValue());
        }
      } else if (duplicate)
        for (Map.Entry<String, Json> e : other.asJsonMap().entrySet())
          set(e.getKey(), e.getValue().dup());
      else
        for (Map.Entry<String, Json> e : other.asJsonMap().entrySet())
          set(e.getKey(), e.getValue());
      return this;
    }

    public Json with(Json x, Json... options) {
      if (x == null) return this;
      if (!x.isObject())
        throw new UnsupportedOperationException();
      if (options.length > 0) {
        Json O = collectWithOptions(options);
        return withOptions(x, O, "");
      } else for (Map.Entry<String, Json> e : x.asJsonMap().entrySet())
        set(e.getKey(), e.getValue());
      return this;
    }

    public Json set(String property, Json el) {
      if (property == null)
        throw new IllegalArgumentException("Null property names are not allowed, value is " + el);
      if (el == null)
        el = nil();
      setParent(el, this);
      object.put(property, el);
      return this;
    }

    public Json atDel(String property) {
      Json el = object.remove(property);
      removeParent(el, this);
      return el;
    }

    public Json delAt(String property) {
      Json el = object.remove(property);
      removeParent(el, this);
      return this;
    }

    public Object getValue() {
      return asMap();
    }

    public boolean isObject() {
      return true;
    }

    public Map<String, Object> asMap() {
      HashMap<String, Object> m = new HashMap<String, Object>();
      for (Map.Entry<String, Json> e : object.entrySet())
        m.put(e.getKey(), e.getValue().getValue());
      return m;
    }

    @Override
    public Map<String, Json> asJsonMap() {
      return object;
    }

    public String toString() {
      return toString(Integer.MAX_VALUE);
    }

    public String toString(int maxCharacters) {
      return toStringImpl(maxCharacters, new IdentityHashMap<Json, Json>());
    }

    String toStringImpl(int maxCharacters, Map<Json, Json> done) {
      StringBuilder sb = new StringBuilder("{");
      if (done.containsKey(this))
        return sb.append("...}").toString();
      done.put(this, this);
      for (Iterator<Map.Entry<String, Json>> i = object.entrySet().iterator(); i.hasNext(); ) {
        Map.Entry<String, Json> x = i.next();
        sb.append('"');
        sb.append(stringEscaper.escapeJsonString(x.getKey()));
        sb.append('"');
        sb.append(":");
        String s = x.getValue().isObject() ? ((ObjectJson) x.getValue()).toStringImpl(maxCharacters, done)
          : x.getValue().isArray() ? ((ArrayJson) x.getValue()).toStringImpl(maxCharacters, done)
          : x.getValue().toString(maxCharacters);
        if (sb.length() + s.length() > maxCharacters)
          s = s.substring(0, Math.max(0, maxCharacters - sb.length()));
        sb.append(s);
        if (i.hasNext())
          sb.append(",");
        if (sb.length() >= maxCharacters) {
          sb.append("...");
          break;
        }
      }
      sb.append("}");
      return sb.toString();
    }

    public int hashCode() {
      return object.hashCode();
    }

    public boolean equals(Object x) {
      return x instanceof ObjectJson && ((ObjectJson) x).object.equals(object);
    }
  }

  // ------------------------------------------------------------------------
  // Extra utilities, taken from around the internet:
  // ------------------------------------------------------------------------

	/*
   * Copyright (C) 2008 Google Inc.
	 *
	 * Licensed under the Apache License, Version 2.0 (the "License");
	 * you may not use this file except in compliance with the License.
	 * You may obtain a copy of the License at
	 *
	 * http://www.apache.org/licenses/LICENSE-2.0
	 *
	 * Unless required by applicable law or agreed to in writing, software
	 * distributed under the License is distributed on an "AS IS" BASIS,
	 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
	 * See the License for the specific language governing permissions and
	 * limitations under the License.
	 */

  static StringEscaper stringEscaper = new StringEscaper(false);

  /**
   * JSON escaping utility.
   *
   * <p>
   * <p>This class contains a single method to escape a passed in string value:
   * <pre>
   *   String jsonStringValue = "beforeQuote\"afterQuote";
   *   String escapedValue = StringEscaper.escapeJsonString(jsonStringValue);
   * </pre></p>
   *
   * @author Inderjeet Singh
   * @author Joel Leitch
   */
  final static class StringEscaper {

    private static final char[] HEX_CHARS = {
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
    };

    private static final Set<Character> JS_ESCAPE_CHARS;
    private static final Set<Character> HTML_ESCAPE_CHARS;

    static {
      Set<Character> mandatoryEscapeSet = new HashSet<>();
      mandatoryEscapeSet.add('"');
      mandatoryEscapeSet.add('\\');

      JS_ESCAPE_CHARS = Collections.unmodifiableSet(mandatoryEscapeSet);

      Set<Character> htmlEscapeSet = new HashSet<>();
      htmlEscapeSet.add('<');
      htmlEscapeSet.add('>');
      htmlEscapeSet.add('&');
      htmlEscapeSet.add('=');
      htmlEscapeSet.add('\'');

      HTML_ESCAPE_CHARS = Immutables.setOf(htmlEscapeSet);
    }

    private final boolean escapeHtmlCharacters;

    StringEscaper(boolean escapeHtmlCharacters) {
      this.escapeHtmlCharacters = escapeHtmlCharacters;
    }

    String escapeJsonString(CharSequence plainText) {
      StringBuilder escapedString = new StringBuilder(plainText.length() + 20);
      try {
        escapeJsonString(plainText, escapedString);
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
      return escapedString.toString();
    }

    private void escapeJsonString(CharSequence plainText, StringBuilder out) throws IOException {
      int pos = 0;  // Index just past the last char in plainText written to out.
      int len = plainText.length();

      for (int charCount, i = 0; i < len; i += charCount) {
        int codePoint = Character.codePointAt(plainText, i);
        charCount = Character.charCount(codePoint);

        if (!isControlCharacter(codePoint) && !mustEscapeCharInJsString(codePoint)) {
          continue;
        }

        out.append(plainText, pos, i);
        pos = i + charCount;
        switch (codePoint) {
          case '\b':
            out.append("\\b");
            break;
          case '\t':
            out.append("\\t");
            break;
          case '\n':
            out.append("\\n");
            break;
          case '\f':
            out.append("\\f");
            break;
          case '\r':
            out.append("\\r");
            break;
          case '\\':
            out.append("\\\\");
            break;
          case '/':
            out.append("\\/");
            break;
          case '"':
            out.append("\\\"");
            break;
          default:
            appendHexJavaScriptRepresentation(codePoint, out);
            break;
        }
      }
      out.append(plainText, pos, len);
    }

    private boolean mustEscapeCharInJsString(int codepoint) {
      if (!Character.isSupplementaryCodePoint(codepoint)) {
        char c = (char) codepoint;
        return JS_ESCAPE_CHARS.contains(c)
          || (escapeHtmlCharacters && HTML_ESCAPE_CHARS.contains(c));
      }
      return false;
    }

    private static boolean isControlCharacter(int codePoint) {
      // JSON spec defines these code points as control characters, so they must be escaped
      return codePoint < 0x20
        || codePoint == 0x2028  // Line separator
        || codePoint == 0x2029  // Paragraph separator
        || (codePoint >= 0x7f && codePoint <= 0x9f);
    }

    private static void appendHexJavaScriptRepresentation(int codePoint, Appendable out)
      throws IOException {

      if (Character.isSupplementaryCodePoint(codePoint)) {
        // Handle supplementary unicode values which are not representable in
        // javascript.  We deal with these by escaping them as two 4B sequences
        // so that they will round-trip properly when sent from java to javascript
        // and back.
        char[] surrogates = Character.toChars(codePoint);
        appendHexJavaScriptRepresentation(surrogates[0], out);
        appendHexJavaScriptRepresentation(surrogates[1], out);
        return;
      }

      out.append("\\u")
        .append(HEX_CHARS[(codePoint >>> 12) & 0xf])
        .append(HEX_CHARS[(codePoint >>> 8) & 0xf])
        .append(HEX_CHARS[(codePoint >>> 4) & 0xf])
        .append(HEX_CHARS[codePoint & 0xf]);
    }
  }

  static class MalformedJsonException extends RuntimeException {
    private static final long serialVersionUID = 1L;

    MalformedJsonException(String msg) {
      super(msg);
    }
  }

  // Object that knows how to read JSON content
  private static class JsonReader {

    private static final Object OBJECT_END    = "}";
    private static final Object ARRAY_END     = "]";
    private static final Object OBJECT_START  = "{";
    private static final Object ARRAY_START   = "[";
    private static final Object COLON         = ":";
    private static final Object COMMA         = ",";

    private static final int FIRST    = 0;
    private static final int CURRENT  = 1;
    private static final int NEXT     = 2;

    private static final Map<Character, Character> 	ESCAPES;
    private static final Set<Object> 								PUNCTUATION;

    static {
      final Map<Character, Character> tempMap = new HashMap<>();
      tempMap.put('"', '"');
      tempMap.put('\\', '\\');
      tempMap.put('/', '/');
      tempMap.put('b', '\b');
      tempMap.put('f', '\f');
      tempMap.put('n', '\n');
      tempMap.put('r', '\r');
      tempMap.put('t', '\t');

      ESCAPES = Immutables.mapOf(tempMap);

      final List<Object> tempList = Arrays.asList(
        OBJECT_END,
        OBJECT_START,
        ARRAY_END,
        ARRAY_START,
        COLON,
        COMMA
      );

      PUNCTUATION = Immutables.setOf(tempList);

    }

    private CharacterIterator it;
    private char c;
    private Object token;
    private StringBuilder buf = new StringBuilder();

    private char next() {
      if (it.getIndex() == it.getEndIndex()) {
        throw new MalformedJsonException(
          "Reached end of input at the " + it.getIndex() + "th character."
        );
      }
      c = it.next();
      return c;
    }

    private char previous() {
      c = it.previous();
      return c;
    }

    private void skipWhiteSpace() {
      do {
        if (!Character.isWhitespace(c) && c == '/') {
          next();
          if (c == '*') {
            // skip multiline comments
            while (c != CharacterIterator.DONE){
              if (next() == '*' && next() == '/') {
                break;
              }
            }

            if (c == CharacterIterator.DONE) {
              throw new MalformedJsonException(
                "Unterminated comment while parsing JSON string."
              );
            }
          } else if (c == '/') {
            while (c != '\n' && c != CharacterIterator.DONE) {
              next();
            }
          } else {
            previous();
            break;
          }
        } else {
          break;
        }
      } while (next() != CharacterIterator.DONE);
    }

    public Object read(CharacterIterator ci, int start) {
      it = ci;

      switch (start) {
        case FIRST:
          c = it.first();
          break;
        case CURRENT:
          c = it.current();
          break;
        case NEXT:
          c = it.next();
          break;
      }

      return read();
    }

    public Object read(CharacterIterator it) {
      return read(it, NEXT);
    }

    public Object read(String string) {
      return read(new StringCharacterIterator(string), FIRST);
    }

    private void expected(Object expectedToken, Object actual) {
      if (expectedToken != actual) {
        throw new MalformedJsonException(
          "Expected " + expectedToken + ", but got " + actual + " instead."
        );
      }
    }

    @SuppressWarnings("unchecked")
    private <T> T read() {
      skipWhiteSpace();
      char ch = c;
      next();
      switch (ch) {
        case '"':
          token = readString();
          break;
        case '[':
          token = readArray();
          break;
        case ']':
          token = ARRAY_END;
          break;
        case ',':
          token = COMMA;
          break;
        case '{':
          token = readObject();
          break;
        case '}':
          token = OBJECT_END;
          break;
        case ':':
          token = COLON;
          break;
        case 't':
          if (c != 'r' || next() != 'u' || next() != 'e') {
            throw new MalformedJsonException(
              "Invalid JSON token: expected 'true' keyword."
            );
          }
          next();
          token = maker().bool(Boolean.TRUE);
          break;
        case 'f':
          if (c != 'a' || next() != 'l' || next() != 's' || next() != 'e')
            throw new MalformedJsonException("Invalid JSON token: expected 'false' keyword.");
          next();
          token = maker().bool(Boolean.FALSE);
          break;
        case 'n':
          if (c != 'u' || next() != 'l' || next() != 'l')
            throw new MalformedJsonException("Invalid JSON token: expected 'null' keyword.");
          next();
          token = nil();
          break;
        default:
          c = it.previous();
          if (Character.isDigit(c) || c == '-') {
            token = readNumber();
          } else {
            throw new MalformedJsonException(
              "Invalid JSON near position: " + it.getIndex()
            );
          }
      }

      return (T) token; // unchecked warning
    }

    private String readObjectKey() {
      Object key = read();
      if (key == null) {
        throw new MalformedJsonException(
          "Missing object key (don't forget to put quotes!)."
        );
      } else if (key == OBJECT_END) {
        return null;
      } else if (PUNCTUATION.contains(key)) {
        throw new MalformedJsonException(
          "Missing object key, found: " + key
        );
      } else {
        return ((Json) key).asString();
      }
    }

    private Json readObject() {
      final Json ret = object();
      String key = readObjectKey();
      while (token != OBJECT_END) {
        expected(COLON, read()); // should be a colon
        if (token != OBJECT_END) {
          Json value = read();
          ret.set(key, value);
          if (read() == COMMA) {
            key = readObjectKey();
            if (key == null || PUNCTUATION.contains(key)) {
              throw new MalformedJsonException(
                "Expected a property name, but found: " + key
              );
            }
          } else {
            expected(OBJECT_END, token);
          }
        }
      }

      return ret;
    }

    private Json readArray() {
      final Json ret = array();

      Object value = read();

      while (token != ARRAY_END) {
        if (PUNCTUATION.contains(value)) {
          throw new MalformedJsonException(
            "Expected array element, but found: " + value
          );
        }

        ret.add((Json) value);

        if (read() == COMMA) {
          value = read();
          if (value == ARRAY_END) {
            throw new MalformedJsonException(
              "Expected array element, but found end of array after command."
            );
          }
        } else {
          expected(ARRAY_END, token);
        }
      }

      return ret;
    }

    private Json readNumber() {
      int length = 0;
      boolean isFloatingPoint = false;
      buf.setLength(0);

      if (c == '-') {
        add();
      }

      length += addDigits();

      if (c == '.') {
        add();
        length += addDigits();
        isFloatingPoint = true;
      }

      if (c == 'e' || c == 'E') {
        add();

        if (c == '+' || c == '-') {
          add();
        }

        addDigits();
        isFloatingPoint = true;
      }

      String s = buf.toString();
      Number n = isFloatingPoint
        ? (length < 17) ? Double.valueOf(s) : new BigDecimal(s)
        : (length < 20) ? Long.valueOf(s) : new BigInteger(s);

      return maker().number(n);
    }

    private int addDigits() {
      int ret;

      for (ret = 0; Character.isDigit(c); ++ret) {
        add();
      }

      return ret;
    }

    private Json readString() {
      buf.setLength(0);
      while (c != '"') {
        if (c == '\\') {
          next();
          if (c == 'u') {
            add(unicode());
          } else {
            Object value = ESCAPES.get(c);
            if (value != null) {
              add((Character) value);
            }
          }
        } else {
          add();
        }
      }

      next();

      return maker().string(buf.toString());
    }

    private void add(char cc) {
      buf.append(cc);
      next();
    }

    private void add() {
      add(c);
    }

    private char unicode() {
      int value = 0;
      for (int i = 0; i < 4; ++i) {
        switch (next()) {
          case '0':
          case '1':
          case '2':
          case '3':
          case '4':
          case '5':
          case '6':
          case '7':
          case '8':
          case '9':
            value = (value << 4) + c - '0';
            break;
          case 'a':
          case 'b':
          case 'c':
          case 'd':
          case 'e':
          case 'f':
            value = (value << 4) + (c - 'a') + 10;
            break;
          case 'A':
          case 'B':
          case 'C':
          case 'D':
          case 'E':
          case 'F':
            value = (value << 4) + (c - 'A') + 10;
            break;
        }
      }
      return (char) value;
    }
  }

  // END Reader
}