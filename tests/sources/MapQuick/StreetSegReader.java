package MapQuick;

import java.io.*;
import java.util.*;

/**
 * A StreetSegReader reads StreetSegments from a set of Tiger Databases.
 *
 * <p>
 * @specfield sourceDirectory : String     // name of directory where Tiger files are located
 * @specfield streetSegs      : Collection // contents of the database; each element is a StreetSegment
 * @endspec
 *
 * <p> Tiger Databases are produced and maintained by the <a
 * href="http://tiger.census.gov/">U.S. Census Bureau</a>.
 *
 * <p> Tiger Databases are distributed in zip-compressed files.  To
 * allow for the loading of an arbitrary number of these databases,
 * one creates each StreetSegReader with a source directory argument,
 * which will then be searched for Tiger Database files when
 * <code>streetSegments()</code> is called.  The subdirectories of the
 * argument directory are not searched recursively for Tiger files as
 * well; only the immediate contents of this.sourceDirectory will be
 * considered as potential Tiger files.
 *
 * <p> StreetSegReader is only specified to operate correctly with
 * directories that contain no <tt>.zip</tt> files besides the Tiger
 * Databases. Files with other extensions (such as the
 * <tt>TIGER98.pdf</tt> file in <tt>/mit/6.170/tigerdb/database/</tt>)
 * will not affect the operation of StreetSegReader, but no other
 * <tt>.zip</tt> files should be kept in the source directory for a
 * StreetSegReader.
 */
public class StreetSegReader
{

  public StreetSegReader(String sourceDirectory) throws InvalidSourceException
  { this(new File(sourceDirectory)); }

  /**
   * @effects Constructs a StreetSegReader where sourceDirectory
   *   contains the Tiger Database files.
   *
   * @throws InvalidSourceException if sourceDirectory is not a
   *   directory.
   */
  public StreetSegReader(File sourceDirectory)
    throws InvalidSourceException
  {
    if (sourceDirectory == null) {
      throw new InvalidSourceException("Pass non-null DIRECTORY into StreetSegReader constructor");
    }

    if (!sourceDirectory.isDirectory()) {
      throw new InvalidSourceException("Pass DIRECTORY into StreetSegReader constructor");
    }

    // java.io.File is immutable
    this.sourceDirectory = sourceDirectory;
  }

  // where to load the database from
  // .isDirectory() == true
  private final File sourceDirectory;

  /**
   * Returns an Iterator over this.streetSegs.
   *
   * @returns an iterator that produces the contents of
   *   this.streetSegs.  Each element produced by the Iterator is a
   *   ps4.StreetSegment.
   */
  public StreetSegIterator streetSegments()
  {
    File[] filesToRead = sourceDirectory.listFiles(zip_filter);
    Arrays.sort(filesToRead);
    StreetSegmentFilter killfilter = KillfileReader.fromDir(sourceDirectory);
    return new StreetSegIterator(Arrays.asList(filesToRead).iterator(), killfilter);
  }

  // filter which gives us the .zip files
  private static final FilenameFilter zip_filter = new ZipFilter();
  private static class ZipFilter
    implements FilenameFilter
  {
    public boolean accept(File d, String name) {
      return (name.toLowerCase().endsWith("zip"));
    }
  }

  /**
   * Exception indicating that the requsted source for a tiger database is invalid
   */
  public static class InvalidSourceException
    extends Exception
  {
    public InvalidSourceException(String msg) {
      super(msg);
    }
  }

}
