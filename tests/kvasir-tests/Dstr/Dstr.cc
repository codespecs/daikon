// $Id: Dstr.cc,v 1.3 2002/12/19 18:40:45 flaterco Exp $
// Dstr:  Dave's String class.
// This source is shared among several of my projects and is public domain.
// Operations are added as needed and there is not necessarily a canonical
// version.

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>
#define min(x, y) ((x) < (y) ? (x) : (y))
#include "Dstr.hh"

// Case-insensitive collation for ISO 8859-1.
// DWF 1999-10-27

// This collation follows the partial ordering formed by Unicode Level
// 1 weights for alphabetic characters, except for the ligature ß
// which should be replaced by ss.  All non-alphabetic characters
// retain their ASCII ordering.

// Reference:
//   http://www.unicode.org/unicode/reports/tr10/basekeys.txt

// For ß you have to look in the "compound keys" file under
// LATIN SMALL LETTER SHARP S:
//   http://www.unicode.org/unicode/reports/tr10/compkeys.txt

// The actual values assigned are not from Unicode but are a relic from
// the previous version of this collation.

// Equivalence classes:
// 256  AaÀÁÂÃÄÅàáâãäå
// 263  Ææ
// 264  Bb
// 265  Cc
// 266  Çç
// 267  Dd
// 268  Ðð
// 269  EeÈÉÊËèéêë
// 274  Ff
// 275  Gg
// 276  Hh
// 277  IiÌÍÎÏìíîï
// 282  Jj
// 283  Kk
// 284  Ll
// 285  Mm
// 286  NnÑñ
// 288  OoÒÓÔÕÖòóôõö
// 294  Øø
// 295  Pp
// 296  Qq
// 297  Rr
// 298  Ss
// 299  ß
// 300  Tt
// 302  UuÙÚÛÜùúûü
// 307  Vv
// 308  Ww
// 309  Xx
// 310  YyÝýÿ
// 313  Zz
// 315  Þþ

int collation[256] = {
    0,  //
    1,  //
    2,  //
    3,  //
    4,  //
    5,  //
    6,  //
    7,  //
    8,  //
    9,  //
   10,  //
   11,  //
   12,  //
   13,  //
   14,  //
   15,  //
   16,  //
   17,  //
   18,  //
   19,  //
   20,  //
   21,  //
   22,  //
   23,  //
   24,  //
   25,  //
   26,  //
   27,  //
   28,  //
   29,  //
   30,  //
   31,  //
   32,  //
   33,  // !
   34,  // "
   35,  // #
   36,  // $
   37,  // %
   38,  // &
   39,  // '
   40,  // (
   41,  // )
   42,  // *
   43,  // +
   44,  // ,
   45,  // -
   46,  // .
   47,  // /
   48,  // 0
   49,  // 1
   50,  // 2
   51,  // 3
   52,  // 4
   53,  // 5
   54,  // 6
   55,  // 7
   56,  // 8
   57,  // 9
   58,  // :
   59,  // ;
   60,  // <
   61,  // =
   62,  // >
   63,  // ?
   64,  // @
  256,  // A
  264,  // B
  265,  // C
  267,  // D
  269,  // E
  274,  // F
  275,  // G
  276,  // H
  277,  // I
  282,  // J
  283,  // K
  284,  // L
  285,  // M
  286,  // N
  288,  // O
  295,  // P
  296,  // Q
  297,  // R
  298,  // S
  300,  // T
  302,  // U
  307,  // V
  308,  // W
  309,  // X
  310,  // Y
  313,  // Z
   91,  // [
   92,  // backslash
   93,  // ]
   94,  // ^
   95,  // _
   96,  // `
  256,  // a
  264,  // b
  265,  // c
  267,  // d
  269,  // e
  274,  // f
  275,  // g
  276,  // h
  277,  // i
  282,  // j
  283,  // k
  284,  // l
  285,  // m
  286,  // n
  288,  // o
  295,  // p
  296,  // q
  297,  // r
  298,  // s
  300,  // t
  302,  // u
  307,  // v
  308,  // w
  309,  // x
  310,  // y
  313,  // z
  123,  // {
  124,  // |
  125,  // }
  126,  // ~
  127,  //
  128,  //
  129,  //
  130,  //
  131,  //
  132,  //
  133,  //
  134,  //
  135,  //
  136,  //
  137,  //
  138,  //
  139,  //
  140,  //
  141,  //
  142,  //
  143,  //
  144,  //
  145,  //
  146,  //
  147,  //
  148,  //
  149,  //
  150,  //
  151,  //
  152,  //
  153,  //
  154,  //
  155,  //
  156,  //
  157,  //
  158,  //
  159,  //
  160,  //
  161,  // ¡
  162,  // ¢
  163,  // £
  164,  // ¤
  165,  // ¥
  166,  // ¦
  167,  // §
  168,  // ¨
  169,  // ©
  170,  // ª
  171,  // «
  172,  // ¬
  173,  // ­
  174,  // ®
  175,  // ¯
  176,  // °
  177,  // ±
  178,  // ²
  179,  // ³
  180,  // ´
  181,  // µ
  182,  // ¶
  183,  // ·
  184,  // ¸
  185,  // ¹
  186,  // º
  187,  // »
  188,  // ¼
  189,  // ½
  190,  // ¾
  191,  // ¿
  256,  // À
  256,  // Á
  256,  // Â
  256,  // Ã
  256,  // Ä
  256,  // Å
  263,  // Æ
  266,  // Ç
  269,  // È
  269,  // É
  269,  // Ê
  269,  // Ë
  277,  // Ì
  277,  // Í
  277,  // Î
  277,  // Ï
  268,  // Ð
  286,  // Ñ
  288,  // Ò
  288,  // Ó
  288,  // Ô
  288,  // Õ
  288,  // Ö
  215,  // ×
  294,  // Ø
  302,  // Ù
  302,  // Ú
  302,  // Û
  302,  // Ü
  310,  // Ý
  315,  // Þ
  299,  // ß
  256,  // à
  256,  // á
  256,  // â
  256,  // ã
  256,  // ä
  256,  // å
  263,  // æ
  266,  // ç
  269,  // è
  269,  // é
  269,  // ê
  269,  // ë
  277,  // ì
  277,  // í
  277,  // î
  277,  // ï
  268,  // ð
  286,  // ñ
  288,  // ò
  288,  // ó
  288,  // ô
  288,  // õ
  288,  // ö
  247,  // ÷
  294,  // ø
  302,  // ù
  302,  // ú
  302,  // û
  302,  // ü
  310,  // ý
  315,  // þ
  310   // ÿ
};


Dstr::Dstr () {
  theBuffer = NULL;
}

Dstr::Dstr (const char *val) {
  if (val) {
    theBuffer = strdup (val);
    used = strlen (val);
    max = used + 1;
  } else
    theBuffer = NULL;
}

Dstr::Dstr (char val) {
  char t[2];
  t[0] = val;
  t[1] = '\0';
  theBuffer = strdup (t);
  used = 1;
  max = 2;
}

Dstr::Dstr (const Dstr &val) {
  if (!(val.isNull())) {
    theBuffer = val.asdupchar();
    used = val.length();
    max = used + 1;
  } else
    theBuffer = NULL;
}

Dstr::~Dstr () {
  if (theBuffer)
    free (theBuffer);
}

// This sped up the indexing step on my PC from 14 seconds to 12 seconds,
// so it's worth keeping, but the primary time waster is all the effort
// required to extract coordinates and build the StationRefs.
Dstr&
Dstr::getline (FILE *fp) {
  char buf[82], *ret;
  if (!(ret = fgets (buf, 82, fp)))
    (*this) = (char *)NULL;
  else {
    (*this) = "";
    while (ret) {
      (*this) += buf;
      if (used > 0)
        if (theBuffer[used-1] == '\n') {
          (*this) -= used-1;
          break;
        }
      ret = fgets (buf, 82, fp);
    }
  }
  return (*this);
}

void Dstr::getline (Dstr &line_out) {
  line_out = (char *)NULL;
  while (((*this).length() > 0) && (*this)[0] != '\n') {
    line_out += (*this)[0];
    (*this) /= 1;
  }
  if ((*this)[0] == '\n')
    (*this) /= 1;
}

// Scan a string like fscanf (fp, "%s")
Dstr&
Dstr::scan (FILE *fp) {
  int c;
  (*this) = (char *)NULL;
  // Skip whitespace
  do {
    c = getc(fp);
    if (c == EOF)
      return (*this);
  } while (isspace (c));
  // Get the string
  (*this) = (char)c;
  while (1) {
    c = getc(fp);
    if (c == EOF)
      return (*this);
    if (isspace (c))
      return (*this);
    (*this) += (char)c;
  }
}

Dstr &
Dstr::pad (unsigned to_length) {
  while (length() < to_length)
    (*this) += " ";
  return (*this);
}

Dstr &Dstr::trim () {
  while (isspace ((*this)[0]))
    (*this) /= 1;
  while (isspace ((*this)[this->length()-1]))
    (*this) -= (this->length()-1);
  return (*this);
}

Dstr&
Dstr::operator-= (unsigned at_index) {
  if (theBuffer) {
    if (at_index < used) {
      theBuffer[at_index] = '\0';
      used = at_index;
    }
  }
  return (*this);
}

int
Dstr::strchr (char val) const {
  if (!theBuffer)
    return -1;
  char *c = ::strchr (theBuffer, val);
  if (!c)
    return -1;
  return (c - theBuffer);
}

int
Dstr::strrchr (char val) const {
  if (!theBuffer)
    return -1;
  char *c = ::strrchr (theBuffer, val);
  if (!c)
    return -1;
  return (c - theBuffer);
}

int
Dstr::strstr (const Dstr &val) const {
  if (!theBuffer)
    return -1;
  if (!val.theBuffer)
    return -1;
  char *c = ::strstr (theBuffer, val.theBuffer);
  if (!c)
    return -1;
  return (c - theBuffer);
}

int Dstr::strcasestr (const Dstr &val) const {
  if (!theBuffer)
    return -1;
  if (!val.theBuffer)
    return -1;
  if (val.length() == 0)
    return 0;
  int i;
  for (i=0; i<(int)used-(int)(val.length())+1; i++)
    if (!slackcmp (theBuffer+i, val.aschar()))
      return i;
  return -1;
}

int
Dstr::strstr (const char *val) const {
  Dstr temp (val);
  return strstr (temp);
}

Dstr &
Dstr::operator= (const char *val) {
  if (theBuffer)
    free (theBuffer);
  if (val) {
    theBuffer = strdup (val);
    used = strlen (val);
    max = used + 1;
  } else
    theBuffer = NULL;
  return (*this);
}

Dstr&
Dstr::operator= (char val) {
  char t[2];
  t[0] = val;
  t[1] = '\0';
  (*this) = t;
  return (*this);
}

Dstr &
Dstr::operator= (const Dstr &val) {
  (*this) = val.theBuffer;
  return (*this);
}

Dstr&
Dstr::operator+= (const char *val) {
  if (val) {
    if (!theBuffer)
      (*this) = val;
    else {
      unsigned l;
      if ((l = strlen (val))) {
        while (l + used >= max) {  // Leave room for terminator
          // Expand
          max *= 2;
          assert (theBuffer = (char *) realloc (theBuffer, max*sizeof(char)));
        }
        strcpy (theBuffer+used, val);
        used += l;
      }
    }
  }
  return (*this);
}

Dstr&
Dstr::operator+= (char val) {
  char t[2];
  t[0] = val;
  t[1] = '\0';
  (*this) += t;
  return (*this);
}

Dstr&
Dstr::operator+= (const Dstr &val) {
  (*this) += val.theBuffer;
  return (*this);
}

Dstr&
Dstr::operator+= (int val) {
  char t[80];
  sprintf (t, "%d", val);
  (*this) += t;
  return (*this);
}

Dstr&
Dstr::operator+= (unsigned int val) {
  char t[80];
  sprintf (t, "%u", val);
  (*this) += t;
  return (*this);
}

Dstr&
Dstr::operator+= (long int val) {
  char t[80];
  sprintf (t, "%ld", val);
  (*this) += t;
  return (*this);
}

Dstr&
Dstr::operator+= (long unsigned int val) {
  char t[80];
  sprintf (t, "%lu", val);
  (*this) += t;
  return (*this);
}

Dstr&
Dstr::operator+= (long long int val) {
  char t[80];
  sprintf (t, "%lld", val);
  (*this) += t;
  return (*this);
}

Dstr&
Dstr::operator+= (double val) {
  char t[80];
  sprintf (t, "%f", val);
  (*this) += t;
  return (*this);
}

Dstr&
Dstr::operator*= (const char *val) {
  Dstr temp (*this);
  (*this) = val;
  (*this) += temp;
  return (*this);
}

Dstr&
Dstr::operator*= (char val) {
  Dstr temp (*this);
  (*this) = val;
  (*this) += temp;
  return (*this);
}

Dstr&
Dstr::operator*= (const Dstr &val) {
  Dstr temp (*this);
  (*this) = val;
  (*this) += temp;
  return (*this);
}

Dstr&
Dstr::operator/= (unsigned at_index) {
  if (theBuffer) {
    Dstr temp ((*this).ascharfrom(at_index));
    (*this) = temp;
  }
  return (*this);
}

Dstr&
Dstr::operator/= (Dstr &val) {
  val = (char *)NULL;
  if (theBuffer) {
    // Eat whitespace
    while (used > 0 && isspace ((*this)[0]))
      (*this) /= 1;
    // Anything left?
    if (used == 0) {
      // Nothing left.
      (*this) = (char *)NULL;
    } else {
      if ((*this)[0] == '"') {
        // Delimited argument
        val += (*this)[0];
        (*this) /= 1;
        while (used > 0 && (*this)[0] != '"') {
          val += (*this)[0];
          (*this) /= 1;
        }
        // Grab the matching quote, if any.
        if (used > 0) {
          val += (*this)[0];
          (*this) /= 1;
        }
      } else {
        // Undelimited argument
        while (used > 0 && (!(isspace ((*this)[0])))) {
          val += (*this)[0];
          (*this) /= 1;
        }
      }
    }
  }
  return (*this);
}

char
Dstr::operator[] (unsigned at_index) const {
  if (!theBuffer)
    return '\0';
  if (at_index >= used)
    return '\0';
  return theBuffer[at_index];
}

unsigned
Dstr::repchar (char X, char Y) {
  unsigned i, repcount=0;
  for (i=0; i<length(); i++)
    if (theBuffer[i] == X) {
      theBuffer[i] = Y;
      repcount++;
    }
  return repcount;
}

unsigned
Dstr::length () const {
  if (!theBuffer)
    return 0;
  return used;
}

int
Dstr::isNull () const {
  if (theBuffer)
    return 0;
  return 1;
}

double
Dstr::asdouble() const {
  double t;
  if (sscanf (aschar(), "%lf", &t) != 1)
    fprintf (stderr, "Warning:  illegal conversion of Dstr to double\n");
  return t;
}

char *
Dstr::aschar () const {
  if (theBuffer)
    return theBuffer;
  return (char *)"";
}

char *
Dstr::asdupchar() const {
  return strdup (aschar());
}

char *
Dstr::ascharfrom(unsigned from_index) const {
  if (!theBuffer)
    return (char *)"";
  if (from_index >= used)
    return (char *)"";
  return theBuffer + from_index;
}

char *
Dstr::asrawchar () const {
  return theBuffer;
}

int operator== (const Dstr &val1, const char *val2) {
  if ((!val2) && (val1.isNull()))
    return 1;
  if ((!val2) || (val1.isNull()))
    return 0;
  return (!(strcmp (val1.aschar(), val2)));
}

int operator== (const char *val1, const Dstr &val2) {
  return (val2 == val1);
}

int operator== (const Dstr &val1, const Dstr &val2) {
  return (val1 == val2.asrawchar());
}

int operator!= (const char *val1, const Dstr &val2) {
  return (!(val1 == val2));
}

int operator!= (const Dstr &val1, const char *val2) {
  return (!(val1 == val2));
}

int operator!= (const Dstr &val1, const Dstr &val2) {
  return (!(val1 == val2));
}

Dstr &
Dstr::lowercase() {
  unsigned i;
  for (i=0; i<length(); i++)
    theBuffer[i] = tolower(theBuffer[i]);
  return (*this);
}

int dstrcasecmp (const Dstr &val1, const Dstr &val2) {
  unsigned n = min (val1.length(), val2.length());
  unsigned i;
  int c, ii;
  for (i=0; i<n; i++) {
    ii = (int)(val1[i]);
    if (ii < 0)
      ii += 256;
    c = collation[ii];
    ii = (int)(val2[i]);
    if (ii < 0)
      ii += 256;
    c -= collation[ii];
    if (c)
      return c;
  }
  c = (int)(val1.length()) - (int)(val2.length());
  return c;
}

/* Slackful strcmp; 0 = match.  It's case-insensitive and accepts a
   prefix instead of the entire string.  The second argument is the
   one that can be shorter. */
int
slackcmp (char *a, char *b)
{
  int c, n, ii;
  unsigned i;
  n = strlen (b);
  if ((int)(strlen (a)) < n)
    return 1;
  for (i=0;i<n;i++) {
    ii = (int)(a[i]);
    if (ii < 0)
      ii += 256;
    c = collation[ii];
    ii = (int)(b[i]);
    if (ii < 0)
      ii += 256;
    c -= collation[ii];
    if (c)
      return c;
  }
  return 0;
}

int operator%= (const Dstr &a, const Dstr &b) {
  return !(slackcmp (a.aschar(), b.aschar()));
}

int dstrcasecmp (const Dstr &val1, char *val2) {
  Dstr temp (val2);
  return dstrcasecmp (val1, temp);
}
