#! /usr/bin/env php
<?php
// dtrace-rm-decls.php <dtrace_file> <decl_file>
//
// declarations are removed from <dtrace_file> and placed in <decl_file>
// They are discarded if <decl_file> is not specified

// main
{
  $dtrace_name = $argv[1];
  $dt_in = fopen ($dtrace_name, "r");
  $dt_out_name = tempnam ("/tmp", "dtrace");
  $dt_out = fopen ($dt_out_name, "w");
  $decl_name = $argv[2];
  if ($decl_name)
    $decl = fopen ($decl_name, "w");

  // process each line in the dtrace file
  while (!feof ($dt_in)) {
    $line = trim (fgets ($dt_in));

    // write declarations to the decl file
    if ($line == "DECLARE") {
      $decl_lines = array();
      while ($line) {
        $decl_lines[] = $line;
        $line = trim (fgets ($dt_in));
      }
      if ($decl) {
        foreach ($decl_lines as $decl_line)
          fputs ($decl, $decl_line. "\n");
        fputs ($decl, "\n");
      }
      continue;
    }

    // All other lines are saved
    fputs ($dt_out, $line . "\n");
  }

  // close the files
  fclose ($dt_in);
  fclose ($dt_out);
  if ($decl)
    fclose ($decl);

  // move the result back to dtrace file
  rename ($dt_out_name, $dtrace_name);
}