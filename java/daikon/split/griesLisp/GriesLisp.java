package daikon.split.griesLisp;

import daikon.split.*;

// I really ought to abstract out the tests rather than making each a class
// all to itself.  Oh, well.


// Instantiate this class in order to install all the appropriate splitters
// in SplitterList.
public class GriesLisp {

  static {

    // p173-14.3
    // (>= x y)
    // (>= y x)

    SplitterList.put("P173-14.3",
                     new Splitter[] { new x_ge_y(),
                                      new y_ge_x() });

    // p176
    // (<= x y)
    // (<= y x)

    SplitterList.put("P176",
                     new Splitter[] { new x_le_y(),
                                      new y_le_x() });

    // p177-14.8
    // (< j 9)
    // (= j 9)

    SplitterList.put("P177-14.8",
                     new Splitter[] { new j_lt_9(),
                                      new j_eq_9() });

    // p177-14.9
    // (< j 9)
    // (>= j 9)

    SplitterList.put("P177-14.9",
                     new Splitter[] { new j_lt_9(),
                                      new j_ge_9() });

    // p177-1
    // (and (<= 0 j) (< j 9))
    // (>= j 9)

    SplitterList.put("P177-1",
                     new Splitter[] { new j_ge_0_lt_9(),
                                      new j_ge_9() });

    // p177-2
    // (< j 9)
    // (= j 9)

    SplitterList.put("P177-2",
                     new Splitter[] { new j_lt_9(),
                                      new j_eq_9() });

    // p178-1b
    // (>= x 0)
    // (<= x 0)

    SplitterList.put("P178-1B",
                     new Splitter[] { new x_ge_0(),
                                      new x_le_0() });

    // p180-15.1.1
    // (/= i n)

    SplitterList.put("P180-15.1.1",
                     new Splitter[] { new i_ne_n() });

    // p184-3
    // (/= i n)
    // (>= x (aref b i))
    // (<= x (aref b i))

    SplitterList.put("P184-3",
                     new Splitter[] { new i_ne_n(),
                                      new x_ge_b_sub_i(),
                                      new x_le_b_sub_i() });

    // p187
    // (> q0 q1)
    // (> q1 q2)
    // (> q2 q3)

    SplitterList.put("P187",
                     new Splitter[] { new q0_gt_q1(),
                                      new q1_gt_q2(),
                                      new q2_gt_q3() });

    // p191-2
    // (> x y)
    // (> y x)

    SplitterList.put("P191-2",
                     new Splitter[] { new x_gt_y(),
                                      new y_gt_x() });

  }

}
