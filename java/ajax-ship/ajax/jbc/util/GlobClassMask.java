/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.util.*;
import ajax.jbc.util.*;
import ajax.jbc.*;
import java.util.*;
import java.io.*;

public class GlobClassMask implements ClassMask {
    private String[] patterns;
    
    private static String[] makeSingleton(String s) {
        String[] ss = { s };
        
        return ss;
    }
    
    public GlobClassMask(String pattern) {
        this(makeSingleton(pattern));
    }
    
/**
Each pattern is a glob. Currently * and ? are the supported metacharacters,
with the usual meanings. A class is in the mask if its fully qualified name
matches the glob. Array classes are considered to have two names, the
raw name (e.g. [Ljava.lang.Object;, [Z) and the human readable name
(e.g. java.lang.Object[], int[]). Patterns containing metacharacters
are never allowed to match an array name (otherwise bad things could happen,
e.g. java.lang.* could match an infinite number of multidimensional
array classes).
*/
    public GlobClassMask(String[] patterns) {
        this.patterns = patterns;
    }
    
    private static boolean isClassLoadable(JBCClassLoader loader, String name) {
        while (loader != null) {
            if (loader.loadClassData(name) != null) {
                return true;
            }
            loader = loader.getParent();
        }

        return false;
    }    
    
    private static void tryAddClass(JBCClassLoader loader, String name, CompactSet result) {
        String elementName = name;
        
        if (elementName.endsWith("[]")) {
            elementName = JBCObjectType.convertHumanReadableToClassName(elementName);
        }
        
        if (elementName.endsWith(";")) {
            elementName = elementName.substring(0, elementName.length() - 1);
            while (elementName.length() > 0 && elementName.charAt(0) == '[') {
                elementName = elementName.substring(1);
            }
            if (elementName.length() > 0 && elementName.charAt(0) == 'L') {
                elementName = elementName.substring(1);
            }
        }
        
        if (isClassLoadable(loader, elementName)) {
            result.add(name);
        }
    }
    
    private static CompactSet fetchLoaderClassNames(StandardClassLoader loader) {
        CompactSet result = new CompactSet();
        
        while (loader != null) {
            for (Enumeration e = loader.getClassList(); e.hasMoreElements();) {
                result.add(e.nextElement());
            }
            
            JBCClassLoader parent = loader.getParent();
            
            if (parent instanceof StandardClassLoader) {
                loader = (StandardClassLoader)parent;
            } else {
                loader = null;
            }
        }
        
        return result;
    }
    
    private static String makeAlternativeName(String name) {
        if (name.startsWith("[")) {
            return JBCObjectType.convertClassNameToHumanReadable(name);
        } else if (name.endsWith("[]")) {
            return JBCObjectType.convertHumanReadableToClassName(name);
        }
        
        return null;
    }
    
    public boolean isMatchingClass(String name) {
        for (int i = 0; i < patterns.length; i++) {
            String p = patterns[i];
            
            if (!GlobMatcher.hasMetaCharacters(p)) {
                if (p.equals(name)) {
                    return true;
                } else {
                    String altName = makeAlternativeName(name);
                    
                    if (altName != null && p.equals(altName)) {
                        return true;
                    }
                }
            } else {
                if (GlobMatcher.isMatch(patterns[i], name)) {
                    return true;
                }
            }
        }
        
        return false;
    }
    
    public CompactSet findMatchingClasses(StandardClassLoader loader) {
        CompactSet result = new CompactSet();
        CompactSet loaderClassNames = null;
        
        for (int i = 0; i < patterns.length; i++) {
            String p = patterns[i];
            
            if (!GlobMatcher.hasMetaCharacters(p)) {
                tryAddClass(loader, p, result);
            } else {
                GlobMatcher matcher = new GlobMatcher(p);
                
                if (loaderClassNames == null) {
                    loaderClassNames = fetchLoaderClassNames(loader);
                }
                
                for (Enumeration e = loaderClassNames.elements(); e.hasMoreElements();) {
                    String s = (String)e.nextElement();
                    
                    if (matcher.isMatch(s)) {
                        tryAddClass(loader, s, result);
                    } else {
                        String alt = makeAlternativeName(s);
                        
                        if (alt != null && matcher.isMatch(alt)) {
                            tryAddClass(loader, s, result);
                        }
                    }
                }
            }
        }
        
        return result;
    }
    
    public static GlobClassMask makeFrom(String s) {
        return new GlobClassMask(StringUtils.split(s, '|'));
    }
    
    public static GlobClassMask readFrom(BufferedReader r) throws IOException {
        Vector ss = new Vector();
        
        for (String s = r.readLine(); s != null; s = r.readLine()) {
            int hashIndex = s.indexOf('#');
            
            if (hashIndex >= 0) {
                s = s.substring(0, hashIndex);
            }
            
            s = s.trim();
            
            if (s.length() > 0) {
                ss.addElement(s);
            }
        }
        
        String[] patterns = new String[ss.size()];
        
        ss.copyInto(patterns);
        return new GlobClassMask(patterns);
    }

    public static GlobClassMask readFrom(File f) throws IOException {
        return readFrom(new BufferedReader(new FileReader(f)));
    }    
    
    public static GlobClassMask readFrom(InputStream s) throws IOException {
        return readFrom(new BufferedReader(new InputStreamReader(s)));
    }    
}
