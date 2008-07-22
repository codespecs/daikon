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
  global $table_line_cnt;
  global $log_cnt;
  global $tb_open;
  global $all_open;
  global $HTTP_SERVER_VARS;
  global $url;

  // echo "processing line '$line'<br>\n";

  if ($skip && ereg ("^\[[0-9: PMA]*]: Reading.*\.\.\. *", $line)) {
      // echo "matched '$line'<br>\n";
      return;
  }

  // $mark_arr = array ("orig(this.s[post(set1)..])", "this.s[orig(set1)..]");
  $mark_arr = array();

  if (strpos ($line, "@") === 0) {

    $log_cnt++;

    if (!$start_table) {
      echo "</pre>\n";
      echo "<table border cellspacing=0 cellpadding=2>\n";
      $start_table = 1;
      $table_line_cnt = 0;
    } else if ($start_table && !$end_table && ($table_line_cnt > 1000)) {
      echo "</table>\n";
      echo "<table border cellspacing=0 cellpadding=2>\n";
      $table_line_cnt = 0;
    }
    $table_line_cnt++;

    $out = str_replace ("<", "&lt;", $line);
    $out = str_replace (": ", "<td>", $out);
    $out = str_replace (",", ", ", $out);
    $out = ereg_replace ("^@  *", "", $out);
    foreach ($mark_arr as $mark)
      $out = str_replace ($mark, "<font color=red>$mark</font>", $out);
    // $out = str_replace ("-- ", "<div style=\"margin-left:10px\">", $out);

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
          || strpos ($frame, "Debug.log")
          || strpos ($frame, "Implication.log"))
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
    $out = str_replace ("<", "&lt;", $line);
    echo "$out\n";
    if (count ($tb))
      echo implode ("\n", $tb);
  }
}

  $file = $_REQUEST['file'];

  if ($file) {
    $file = str_replace ("~jhp/", "/afs/csail.mit.edu/u/j/jhp/", $file);
    if (strpos ($file, "daikon") === false) {
      echo "<b>Error: </b> file '$file' is an invalid daikon output file<br>";
      echo "email jhp@csail.mit.edu if this is unexpected";
      exit;
    }

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


  $url = "log2html.php?file=$file";

  // Read in the current state
  // Right now it has two lines.  The first is all_open and the second
  // is the list of hand-opened tracebacks.
  $tb_open = array();
  $fp = @fopen ("$file.state", "r+");
  if (!$fp)
    echo "<font color=red> Warning: Can't open state file '$file.state' "
        . "</font><p>\n";
  else {
    $all_open = trim (fgets ($fp, 5 * 1024));
    $tb_open = explode (" ", trim (fgets ($fp, 5*1024)));
  }

  // if open or close is specified, add it in and rewrite the state
  if ($fp) {
    if ($open)
      $tb_open[] = $open;
    if ($close) {
      while (in_array ($close, $tb_open)) {
        $key = array_search ($close, $tb_open);
        // echo "old tb_open = " . implode (" ", $tb_open) . "<br>\n";
        // echo "unsetting $key<br>\n";
        unset ($tb_open[$key]);
      }
      $tb_open = array_values ($tb_open);
      // echo "new tb_open = " . implode (" ", $tb_open) . "<br>\n";

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
    if ($open || $close) {
      header("Location: http://".$_SERVER['HTTP_HOST']
                     .dirname($_SERVER['PHP_SELF'])
                     ."/log2html.php?file=$file");
      exit;
    }
  }

  echo "<title> log2html $file </title>\n";
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
        $line = rtrim (get_line ($stdin));

    // Look for a traceback on the following lines
    $next_line = rtrim (get_line ($stdin));
    if (strpos ($next_line, "java.lang.Throwable: debug traceback") === 0) {
      $next_line = rtrim (fgets ($stdin, 5*1024));
      while (strpos ($next_line, "\tat daikon.") === 0) {
        $tb[] = $next_line;
        $next_line = rtrim (get_line ($stdin));
      }
    }

    $pos = strpos ($line, "]@ ");
    if ($pos <= 0) {
      if (ereg ("\.\.\. *@ ", $line))
        $pos = strpos ($line, " @ ");
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

// Reads the next line from fp, treating normal line breaks and carriage
// returns as line breaks
function get_line ($fp) {

    static $lines;
    static $pos;

    // Get a new set of lines
    if (!is_array ($lines) || ($pos >= count($lines))) {
        $line = fgets ($fp, 10*1024);
        // echo "Read line '$line'<br>\n";
        $lines = explode ("", $line);
        $pos = 0;
        // echo "line count = " + count($lines) + "<br>\n";
    }

    $out = $lines[$pos];
    $pos++;
    return ($out);
}


?>
