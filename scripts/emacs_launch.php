<?php
//
//  Creates a very simple output of type application/emacs that contains
//  an emacsclient command to execute.  Used in conjunction with the
//  helper application browser_emacs in a browser to bring up the
//  specified file in emacs
//
//  See log2html.php and the daikon developer manual for more information
//
header ("Content-type: application/emacs");
echo "emacsclient -n +$line $file";

?>
