#! /usr/bin/env php
<?php
// clean-decls.php
//
// Reads from standard in and writes to standard out.  Decl header info
// is removed so that diffs can work more cleanly.

// main
{
  $STDIN = STDIN;

  while (!feof ($STDIN)) {
    $line = trim (fgets ($STDIN));
    // echo "$line\n";
    if (strpos ($line, "// Declarations ") === 0)
      continue;
    if (strpos ($line, "// Written") === 0) {
      $line = fgets ($STDIN);
      continue;
    }
    if ($line == "VarComparability") {
      $line = fgets ($STDIN);
      $line = fgets ($STDIN);
      continue;
    }

    if ($line == "ListImplementors") {
      while ($line != "")
        $line = trim(fgets ($STDIN));
      $line = trim (fgets ($STDIN));
      continue;
    }

    printf ("%s\n", $line);
  }
}