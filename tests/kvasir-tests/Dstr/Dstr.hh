// $Id: Dstr.hh,v 1.3 2002/12/19 18:40:45 flaterco Exp $
// Dstr:  Dave's String class.
// This source is shared among several of my projects and is public domain.
// Operations are added as needed and there is not necessarily a canonical
// version.

/* Slackful strcmp; 0 = match.  It's case-insensitive and accepts a
   prefix instead of the entire string.  The second argument is the
   one that can be shorter. */
int slackcmp (char *a, char *b);

class Dstr {
public:
  Dstr();
  Dstr(const char *val);
  Dstr(char val);
  Dstr(const Dstr &val);
  ~Dstr();

  unsigned length() const;
  int isNull() const;

  // Read a line.  The trailing newline is stripped.  DOS/VMS two-character
  // line discipline is not supported.  On EOF, Dstr becomes null.
  Dstr& getline (FILE *fp);
  // Scan a string like fscanf (fp, "%s")
  Dstr& scan (FILE *fp);
  // Scan a line from a Dstr, stripping newline.
  void getline (Dstr &line_out);

  // Assign
  Dstr& operator= (const char *val);
  Dstr& operator= (char val);
  Dstr& operator= (const Dstr &val);

  // Append
  Dstr& operator+= (const char *val);
  Dstr& operator+= (char val);
  Dstr& operator+= (const Dstr &val);
  Dstr& operator+= (int val);
  Dstr& operator+= (unsigned int val);
  Dstr& operator+= (long int val);
  Dstr& operator+= (long unsigned int val);
  Dstr& operator+= (long long int val);
  Dstr& operator+= (double val);

  // Prepend
  Dstr& operator*= (const char *val);
  Dstr& operator*= (char val);
  Dstr& operator*= (const Dstr &val);

  // Truncate
  Dstr& operator-= (unsigned at_index);

  // Break off the first substring delimited by whitespace or double quotes
  // (no escaping) and assign it to val.  The double quotes are NOT removed,
  // and if the argument is terminated by the end-of-line rather than a
  // matching quote, you'll get the unbalanced quotes back.
  Dstr& operator/= (Dstr &val);
  // Remove all text before the specified index
  Dstr& operator/= (unsigned at_index);

  // Convert contents to a double.
  double asdouble() const;

  // Get index; returns -1 if not found
  int strchr (char val) const;
  int strrchr (char val) const;
  int strstr (const Dstr &val) const;
  int strstr (const char *val) const;

  // Case-insensitive.
  int strcasestr (const Dstr &val) const;

  // Get character at index
  char operator[] (unsigned at_index) const;

  // Pad to length with spaces.
  Dstr &pad (unsigned to_length);
  // Strip leading and trailing whitespace.
  Dstr &trim ();

  // Retrieve value as character string.  This will actually be theBuffer
  // unless it's NULL -- in which case an empty string will be
  // substituted.
  char *aschar() const;
  // Same thing, but strdup'd
  char *asdupchar() const;
  // Same thing, but starting at index.
  char *ascharfrom(unsigned from_index) const;
  // Retrieve value as a character string, no NULL masking.
  char *asrawchar() const;

  // Replace all instances of character X with character Y; returns number
  // of reps.
  unsigned repchar (char X, char Y);

  // Smash case
  Dstr &lowercase();

protected:
  char *theBuffer;
  unsigned max;   // Total max buffer size including \0
  unsigned used;  // Length not including \0
};

// Compare
int operator== (const Dstr &val1, const char *val2);
int operator== (const char *val1, const Dstr &val2);
int operator== (const Dstr &val1, const Dstr &val2);
int operator!= (const char *val1, const Dstr &val2);
int operator!= (const Dstr &val1, const char *val2);
int operator!= (const Dstr &val1, const Dstr &val2);

// "Is kinda like" comparison operator (opposite of slackcmp)
/* It's case-insensitive and accepts a prefix instead of the entire
   string.  The second argument is the one that can be shorter. */
int operator%= (const Dstr &a, const Dstr &b);

// This is case insensitive and sorts by Latin1 collating sequence.
int dstrcasecmp (const Dstr &val1, const Dstr &val2);
int dstrcasecmp (const Dstr &val1, char *val2);
