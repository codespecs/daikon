<?php
//
//  log2html.php process an output file from daikon that contains debug
//  logging and produces an html file that is (at least to some eyes)
//  significantly more readable.  It also incorporates in tracebacks
//  in a more condensed form and allows users to M1 a traceback
//  field and bring up the related file in emacs.  This capability
//  requires that files of type applications/emacs be handled by
//  the script browser_emacs.  More information is available
//  in the daikon developer manual available on the daikon web
//  page


// parses a line from a java traceback and returns the java file name
// (relative to the top of the java source tree), qualified class name,
// unqualified class name, method name, and line #
function parse_frame ($frame) {

  $frame = str_replace ("\tat ", "", $frame);
  list ($class_method, $file) = explode ("(", $frame, 2);
  $cma = explode (".", $class_method);
  $unqual_name = $cma[count($cma)-2];
  $method_name = $cma[count($cma)-1];

  for ($i = 0; $i < count($cma) - 1; $i++) {
    if ($qual_name)
      $qual_name .= ".";
    $qual_name .= $cma[$i];
  }
  $file_name = str_replace (".", "/", $qual_name) . ".java";

  list ($garbage, $line) = explode (":", $file);
  $line = str_replace (")", "", $line);

  return (array ($file_name, $qual_name, $unqual_name, $method_name, $line));
}

function process_line ($line, $tb) {

  global $start_table;
  global $stop_table;
  global $log_cnt;
  global $tb_open;
  global $all_open;
  global $HTTP_SERVER_VARS;
  global $url;

  if (strpos ($line, "@") === 0) {

    $log_cnt++;

    if (!$start_table) {
      echo "</pre>\n";
      echo "<table border cellspacing=0 cellpadding=2>\n";
      $start_table = 1;
    }

    $out = str_replace ("<", "&lt;", $line);
    $out = str_replace (": ", "<td>", $out);
    $out = str_replace (",", ", ", $out);
    $out = ereg_replace ("^@  *", "", $out);

    $code_base = "~/research/invariants/java";

    // Add the traceback (if any) after the debug name
    $tb_str = "";
    if (count ($tb) == 0)
      $tb_str = "";
    else if ($all_open || in_array ($log_cnt, $tb_open)) {
      $tb_str = "<a href=$url&close=$log_cnt#$log_cnt><b>-</b></a>";
      $first = true;
      foreach ($tb as $frame) {
        if (strpos ($frame, "inv.Invariant.log"))
          continue;
        list ($file, $qname, $uqname, $method, $line_no) = parse_frame($frame);
        if (!$first)
          $tb_str .= "&nbsp;&nbsp;";
        $tb_str .= "<a href=emacs_launch.php?file=$code_base/$file&line=$line_no>"
                .  "$uqname.$method</a><br>\n";
        $first = false;
      }
    } else {
      foreach ($tb as $frame) {
        if (strpos ($frame, "inv.Invariant.log")
          || strpos ($frame, "Debug.log"))
          continue;
        list ($file, $qname, $uqname, $method, $line_no) = parse_frame($frame);
        break;
      }
      $tb_str = "<a href=$url&open=$log_cnt#$log_cnt><b>+</b></a>";
      $tb_str .= "<a href=emacs_launch.php?file=$code_base/$file&line=$line_no>"
                .  "$uqname.$method</a><br>\n";
    }

    $pos = strpos ($out, "<td>");
    $out = substr ($out, 0, $pos) . " <td> $tb_str" . substr ($out, $pos);

    if (strpos ($line, "daikon.inv.track") > 0) {
      // $out = str_replace ("EXIT(", "EXIT <td> (", $out);
      // $out = str_replace ("ENTER(", "ENTER <td> (", $out);
      // $out = str_replace (", ", ", <td> ", $out);
    }
    echo ("<tr valign=top> <td> <a name=$log_cnt> $out\n");

  } else { // normal non-log line

    if ($start_table && !$end_table) {
      echo "</table>\n";
      echo "<pre>\n";
      $start_table = 0;
    }
    echo "$line\n";
    if (count ($tb))
      echo implode ("\n", $tb);
  }
}

  if ($file) {
    $stdin = @fopen ($file, "r");
    if (!$stdin) {
      echo "<b>Error: </b> Can't open file $file<br>\n";
      exit;
    }
  } else { // no file specified, put up a form
    echo "<title> log2html </title>\n";
    echo "<body> <h3 align=center> log2html </h3>\n";
    echo "<form action=log2html.php method=get>\n";
    echo "Enter Daikon Output Filename: <input type=text name=file size=30>";
    echo "</form>\n";
    exit;
  }

  echo "<title> log2html $file </title>\n";

  $url = "log2html.php?file=$file";

  // Read in the current state
  // Right now it has two lines.  The first is all_open and the second
  // is the list of hand-opened tracebacks.
  $fp = @fopen ("$file.state", "r+");
  if (!$fp)
    echo "<font color=red> Warning: Can't open state file '$file.state' "
        . "</font><p>\n";
  else {
    $all_open = trim (fgets ($fp, 1024));
    $tb_open = explode (" ", trim (fgets ($fp, 1024)));
  }

  // if open or close is specified, add it in and rewrite the state
  if ($fp) {
    if ($open)
      $tb_open[] = $open;
    if ($close) {
      if (in_array ($close, $tb_open)) {
        $key = array_search ($close, $tb_open);
        // echo "old tb_open = " . implode (" ", $tb_open) . "<br>\n";
        // echo "unsetting $key<br>\n";
        unset ($tb_open[$key]);
        $tb_open = array_values ($tb_open);
        // echo "new tb_open = " . implode (" ", $tb_open) . "<br>\n";
      }
    }
    rewind ($fp);
    $x = fwrite ($fp, "$all_open\n");
    $x1 = fwrite ($fp, implode (" ", $tb_open) . "\n");
    ftruncate ($fp, ftell($fp));
    // echo "x/x1 = $x, $x1\n";
    // echo "ftell = " . ftell($fp) . "\n";
    // echo "all_open = $all_open\n";
    // echo "tb_open = " . implode (" ", $tb_open) . "\n";
    fclose ($fp);
  }
  $start_table = 0;
  $end_table = 0;

  echo "<pre>\n";
  unset ($next_line);
  while (!feof ($stdin)) {

    $tb = array();

    // Get a line of input, using any previously read lines first
    if (isset ($next_line))
      $line = $next_line;
    else
      $line = rtrim (fgets ($stdin, 1024));

    // Look for a traceback on the following lines
    $next_line = rtrim (fgets ($stdin, 1024));
    if (strpos ($next_line, "java.lang.Throwable: debug traceback") === 0) {
      $next_line = rtrim (fgets ($stdin, 1024));
      while (strpos ($next_line, "\tat daikon.") === 0) {
        $tb[] = $next_line;
        $next_line = rtrim (fgets ($stdin, 1024));
      }
    }

    $pos = strpos ($line, "]@ ");
    if ($pos <= 0) {
      $pos = strpos ($line, "...@ ");
      if ($pos > 0)
        $pos += 2;
      else
        $pos = strpos ($line, ".@");
    }
    if ($pos > 0) {
      process_line (substr ($line, 0, $pos + 1), array());
      process_line (substr ($line, $pos + 1), $tb);
    } else {
      process_line ($line, $tb);
    }

  }

  echo "</pre>\n";
?>
