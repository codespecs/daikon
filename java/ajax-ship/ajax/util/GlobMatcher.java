/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util;

import ajax.Globals;

public class GlobMatcher {
    private char[] prefix;
    private char[] suffix;
    private char[] interior;
    
    private static final char[] emptyChars = new char[0];
    
    public GlobMatcher(String pattern) {
        parsePattern(pattern);
    }
    
    public static boolean isMetaCharacter(char c) {
        switch (c) {
            case '*':
            case '?':
                return true;
                
            default:
                return false;
        }
    }
    
    public static boolean hasMetaCharacters(String s) {
        for (int i = s.length() - 1; i >= 0; i--) {
            if (isMetaCharacter(s.charAt(i))) {
                return true;
            }
        }
        
        return false;
    }
    
    private void parsePattern(String pattern) {
        int firstStar = pattern.indexOf('*');
        
        if (firstStar < 0) {
            prefix = pattern.toCharArray();
            suffix = emptyChars;
            interior = null;
        } else {
            prefix = new char[firstStar];
            pattern.getChars(0, firstStar, prefix, 0);
            
            int lastStar = pattern.lastIndexOf('*');
            int patLen = pattern.length();
            
            suffix = new char[patLen - lastStar - 1];
            pattern.getChars(lastStar + 1, patLen, suffix, 0);
            
            if (firstStar < lastStar) {
                interior = new char[lastStar - firstStar - 1];
                pattern.getChars(firstStar + 1, lastStar, interior, 0);
            } else {
                interior = emptyChars;
            }
        }
    }
    
    public boolean isMatch(String s) {
        int sLength = s.length();
        
        if (sLength < prefix.length + suffix.length) {
            return false;
        } else {
            for (int i = 0; i < prefix.length; i++) {
                char prefixCh = prefix[i];
                
                if (prefixCh != '?' && s.charAt(i) != prefixCh) {
                    return false;
                }
            }
            
            int sIndex = sLength - suffix.length;
            
            for (int i = 0; i < suffix.length; i++) {
                char suffixCh = suffix[i];
                
                if (suffixCh != '?' && s.charAt(sIndex + i) != suffixCh) {
                    return false;
                }
            }
            
            if (interior == null) {
                return sIndex == prefix.length;
            } else if (interior.length > 0) {
                Globals.localError("We don't handle nonempty interiors yet!!");
            }
            
            return true;
        }
    }
    
    public static boolean isMatch(String pattern, String s) {
        return (new GlobMatcher(pattern)).isMatch(s);
    }
}
