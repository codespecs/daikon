/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util;

import java.io.*;

public class IOUtils {
    public static byte[] readFileBytes(File f) throws IOException {
	InputStream in = new FileInputStream(f);
	int size = (int)Math.min(f.length(), (long)Integer.MAX_VALUE);

        if (size == Integer.MAX_VALUE) {
	    return readBytes(in);
	} else {
	    return readBytes(in, size);
	}
    }

    public static byte[] readBytes(InputStream in) throws IOException {
	return readBytes(in, -1);
    }

    public static byte[] readBytes(InputStream in, int sizeHint) throws IOException {
	byte[] result = new byte[sizeHint < 65536 ? 65536 : sizeHint];
	int len = 0;
                    
	do {
	    if (len == result.length) {
		if (result.length == Integer.MAX_VALUE) {
		    break;
		}

                byte[] extra = null;
		int extraRead = 0;

		if (len == sizeHint) {
		    extra = new byte[16384]; // must be less than 1/2 of 65536

		    extraRead = in.read(extra, 0, extra.length);

                    if (extraRead < 0) {
			break;
		    }
		}
		
		byte[] newResult =
		    new byte[(int)Math.min(((long)len)*2, Integer.MAX_VALUE)];
                
		System.arraycopy(result, 0, newResult, 0, len);
		if (extra != null) {
                    System.arraycopy(extra, 0, newResult, len,
                       Math.max(newResult.length - len, extraRead));
		    len += extraRead;
		}
		result = newResult;
	    }

	    int amountRead = in.read(result, len, result.length - len);
                        
	    if (amountRead < 0) {
		break;
	    } else {
		len += amountRead;
	    }
	} while (true);
                 
        if (len == result.length) {
	    return result;
	} else {
	    byte[] data = new byte[len];
                    
	    System.arraycopy(result, 0, data, 0, len);
	    return data;
	}
    }

    public static char[] readChars(Reader in) throws IOException {
	char[] result = new char[20000];
	int len = 0;
                    
	do {
	    if (len == result.length) {
		if (result.length == Integer.MAX_VALUE) {
		    break;
		}

		char[] newResult =
		    new char[(int)Math.min(((long)len)*2, Integer.MAX_VALUE)];
                
		System.arraycopy(result, 0, newResult, 0, len);
		result = newResult;
	    }

	    int amountRead = in.read(result, len, result.length - len);
                        
	    if (amountRead < 0) {
		break;
	    } else {
		len += amountRead;
	    }
	} while (true);
                 
        if (len == result.length) {
	    return result;
	} else {
	    char[] data = new char[len];
                    
	    System.arraycopy(result, 0, data, 0, len);
	    return data;
	}
    }

    public static String readString(Reader in) throws IOException {
	return new String(readChars(in));
    }
}
