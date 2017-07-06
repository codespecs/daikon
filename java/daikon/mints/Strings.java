package daikon.mints;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * @author Huascar Sanchez
 */
class Strings {
  private Strings(){}

  static boolean isEmpty(String text){
    final Optional<String> optional = Optional.ofNullable(text);
    return optional.map(s -> !s.isEmpty()).orElse(false);
  }


  static List<String> generateLabel(String guessedName){
    return Arrays.stream(
      guessedName.split("(?<!(^|[A-Z]))(?=[A-Z])|(?<!^)(?=[A-Z][a-z])"))
      .map(String::toLowerCase)
      .collect(Collectors.toList());
  }

  /**
   * <p>
   * Perform JSON escaping so that ", <, >, etc. characters are properly encoded in the
   * JSON string representation before returning to the client code. This is useful when
   * serializing property names or string values.
   * </p>
   */
  static String joinParameters(Map<String, String> env, List<String> params){

    if(Objects.isNull(params) || params.isEmpty()) return "";

    final StringBuilder toString = new StringBuilder(params.size() * 100);

    final Iterator<String> iterator = params.iterator();
    while (iterator.hasNext()){

      final String each = iterator.next().trim();
      if(env.containsKey(each)){
        toString.append(env.get(each));
        if(iterator.hasNext()){
          toString.append(", ");
        }
      } else {
        toString.append(each);
        if(iterator.hasNext()){
          toString.append(", ");
        }
      }

    }

    return toString.toString();
  }

  public static String escape(final String json) {
    StringBuilder sb = new StringBuilder();
    sb.append("\"");

    int jsonLength = json.length();

    for (int i = 0; i < jsonLength; i++) {
      char ch = json.charAt(i);
      switch (ch) {
        case '\\':
          sb.append("\\\\");
          break;
        case '\"':
          sb.append("\\\"");
          break;
        case '/':
          sb.append("\\/");
          break;
        case '\b':
          sb.append("\\b");
          break;
        case '\f':
          sb.append("\\f");
          break;
        case '\n':
          sb.append("\\n");
          break;
        case '\r':
          sb.append("\\r");
          break;
        case '\t':
          sb.append("\\t");
          break;
        default:
          sb.append(ch);
          break;
      }
    }
    sb.append("\"");
    return sb.toString();
  }
}
