package agora;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

public class AgoraTest {

  public static void main(String[] args) {
    AgoraTest a = new AgoraTest();
    a.emptySequence(new ArrayList<>());
    a.emptySequence(new LinkedList<>());
    a.emptySequence(new ArrayList<>());
    a.emptySequence(new LinkedList<>());
    a.emptySequence(new ArrayList<>());
    a.emptySequence(new LinkedList<>());
    a.emptySequence(new ArrayList<>());
    a.emptySequence(new LinkedList<>());
    a.emptySequence(new ArrayList<>());
    a.emptySequence(new LinkedList<>());

    a.emptyString(new String(""));
    a.emptyString(new String(""));
    a.emptyString(new String(""));
    a.emptyString(new String(""));
    a.emptyString(new String(""));
    a.emptyString(new String(""));
    a.emptyString(new String(""));
    a.emptyString(new String(""));
    a.emptyString(new String(""));
    a.emptyString(new String(""));

    a.stringParameter1("22");
    a.stringParameter1("122");
    a.stringParameter1("-322");
    a.stringParameter1("3.14159");
    a.stringParameter1("1.618");
    a.stringParameter1("-333333333");
    a.stringParameter1("1234567890");
    a.stringParameter1("22");
    a.stringParameter1("22");

    a.stringParameter1("22");
    a.stringParameter1("122");
    a.stringParameter1("-322");
    a.stringParameter1("3.14159");
    a.stringParameter1("1.618");
    a.stringParameter1("-333333333");
    a.stringParameter1("1234567890");
    a.stringParameter1("22");
    a.stringParameter1("22");
    a.stringParameter1("");
  }

  void emptySequence(List<String> lst) {}

  void emptyString(String s) {}

  void stringParameter1(String s) {}

  void stringParameter2(String s) {}

  void stringParameter3(String s) {}
}
