/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util.salamis;

public interface SalamisParserConstants {

  int EOF = 0;
  int SINGLE_LINE_COMMENT = 6;
  int FORMAL_COMMENT = 7;
  int MULTI_LINE_COMMENT = 8;
  int CATCH = 9;
  int GOTO = 10;
  int CHOOSE = 11;
  int NEW = 12;
  int STRING = 13;
  int IDENTIFIER = 14;
  int LETTER = 15;
  int DIGIT = 16;
  int LBRACE = 17;
  int RBRACE = 18;
  int SEMICOLON = 19;
  int COMMA = 20;
  int DOT = 21;

  int DEFAULT = 0;

  String[] tokenImage = {
    "<EOF>",
    "\" \"",
    "\"\\t\"",
    "\"\\n\"",
    "\"\\r\"",
    "\"\\f\"",
    "<SINGLE_LINE_COMMENT>",
    "<FORMAL_COMMENT>",
    "<MULTI_LINE_COMMENT>",
    "\"catch\"",
    "\"goto\"",
    "\"choose\"",
    "\"new\"",
    "<STRING>",
    "<IDENTIFIER>",
    "<LETTER>",
    "<DIGIT>",
    "\"{\"",
    "\"}\"",
    "\";\"",
    "\",\"",
    "\".\"",
    "\"(\"",
    "\")\"",
    "\":\"",
    "\":=\"",
    "\"=\"",
    "\"#\"",
  };

}
