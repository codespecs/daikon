/*
 * @(#)StreamRedirectThread.java	1.4 03/01/23
 *
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */
/*
 * Copyright (c) 1997-2001 by Sun Microsystems, Inc. All Rights Reserved.
 *
 * Sun grants you ("Licensee") a non-exclusive, royalty free, license to use,
 * modify and redistribute this software in source and binary code form,
 * provided that i) this copyright notice and license appear on all copies of
 * the software; and ii) Licensee does not utilize the software in a manner
 * which is disparaging to Sun.
 *
 * This software is provided "AS IS," without a warranty of any kind. ALL
 * EXPRESS OR IMPLIED CONDITIONS, REPRESENTATIONS AND WARRANTIES, INCLUDING ANY
 * IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE OR
 * NON-INFRINGEMENT, ARE HEREBY EXCLUDED. SUN AND ITS LICENSORS SHALL NOT BE
 * LIABLE FOR ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING
 * OR DISTRIBUTING THE SOFTWARE OR ITS DERIVATIVES. IN NO EVENT WILL SUN OR ITS
 * LICENSORS BE LIABLE FOR ANY LOST REVENUE, PROFIT OR DATA, OR FOR DIRECT,
 * INDIRECT, SPECIAL, CONSEQUENTIAL, INCIDENTAL OR PUNITIVE DAMAGES, HOWEVER
 * CAUSED AND REGARDLESS OF THE THEORY OF LIABILITY, ARISING OUT OF THE USE OF
 * OR INABILITY TO USE SOFTWARE, EVEN IF SUN HAS BEEN ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGES.
 *
 * This software is not designed or intended for use in on-line control of
 * aircraft, air traffic, aircraft navigation or aircraft communications; or in
 * the design, construction, operation or maintenance of any nuclear
 * facility. Licensee represents and warrants that it will not use or
 * redistribute the Software for such purposes.
 */

package daikon.chicory;

import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.*;

/**
 * StreamRedirectThread is a thread that copies its input to its output and terminates when it
 * completes.
 *
 * @version (#) StreamRedirectThread.java 1.4 03/01/23 16:33:15
 * @author Robert Field
 */
public class StreamRedirectThread extends Thread {

  private final Reader in;
  private final Writer out;
  private final PrintStream outWriter;

  private boolean line_by_line = false;

  private boolean debug = false;

  private static final int BUFFER_SIZE = 2048;
  //for debugging: private static final int BUFFER_SIZE = 1;

  public StreamRedirectThread(String name, InputStream in, OutputStream out) {
    this(name, in, out, false, false);
  }

  public StreamRedirectThread(String name, InputStream in, OutputStream out, boolean line_by_line) {
    this(name, in, out, line_by_line, false);
  }

  /**
   * Set up for copy.
   *
   * @param name name of the thread
   * @param in stream to copy from
   * @param out stream to copy to
   * @param line_by_line whether to copy one line at a time
   * @param debug whether to enable debugging
   */
  public StreamRedirectThread(
      String name, InputStream in, OutputStream out, boolean line_by_line, boolean debug) {
    super(name);
    if (debug) {
      System.out.println(
          "StreamRedirectThread("
              + name
              + ", "
              + in
              + ", "
              + out
              + ", "
              + line_by_line
              + ", "
              + debug
              + ")");
    }
    if (in == null || out == null) {
      System.out.println("bad arguments to StreamRedirectThread: " + in + " " + out);
    }
    this.in = new InputStreamReader(in, UTF_8);
    this.out = new OutputStreamWriter(out, UTF_8);
    this.outWriter = new PrintStream(out);
    this.line_by_line = line_by_line;
    this.debug = debug;

    setPriority(Thread.MAX_PRIORITY - 1);
  }

  /** Copy. */
  @Override
  public void run() {
    try {
      if (line_by_line) {
        BufferedReader br = new BufferedReader(in, BUFFER_SIZE);

        char[] cbuf = new char[BUFFER_SIZE];
        int count;

        String line;
        while ((line = br.readLine()) != null) {
          outWriter.println(line);
        }
      } else {
        int nextChar;
        while (true) {
          // read() is a blocking call, but that's OK because
          // this is running in its own thread.
          nextChar = in.read();
          if (nextChar == -1) {
            break;
          }

          if (debug) {
            System.out.println("[[[" + nextChar + "]]]");
          }
          out.write(nextChar);
          out.flush();
        }

        out.flush();
      }
    } catch (IOException exc) {
      System.err.println("Child I/O Transfer - " + exc);
    }
  }
}
