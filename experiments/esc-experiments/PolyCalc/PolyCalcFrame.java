package PolyCalc;

import javax.swing.*;
import java.awt.*;
// import com.borland.jbcl.layout.*;
import javax.swing.border.*;
import java.awt.event.*;


/**
 * Title:        6.170 PS1
 * Description:  6.170 Problem Set 1
 * Copyright:    Copyright (c) 2001
 * Company:      MIT
 * @author Felix Klock, Andreas Hofmann
 * @version 1.0
 */

public class PolyCalcFrame extends JFrame {
  JPanel jPanel1 = new JPanel();
  JPanel jPanel2 = new JPanel();
  JPanel jPanel3 = new JPanel();
  JPanel jPanel4 = new JPanel();
  JPanel jPanel5 = new JPanel();
  JPanel jPanel6 = new JPanel();
  Border border1;
  JLabel jLabel5 = new JLabel();
  JLabel jLabel6 = new JLabel();
  JTextField jTextField1 = new JTextField();
  JTextField jTextField2 = new JTextField();
  JButton jButton_exp = new JButton();
  JButton jButton_div = new JButton();
  JButton jButton_mul = new JButton();
  JButton jButton_7 = new JButton();
  JButton jButton_8 = new JButton();
  JButton jButton_9 = new JButton();
  JButton jButton_4 = new JButton();
  JButton jButton_5 = new JButton();
  JButton jButton_6 = new JButton();
  JButton jButton_1 = new JButton();
  JButton jButton_2 = new JButton();
  JButton jButton_3 = new JButton();
  JButton jButton_0 = new JButton();
  JButton jButton_x = new JButton();
  JButton jButton_Enter = new JButton();
  JButton jButton_plus = new JButton();
  JButton jButton_minus = new JButton();
  JButton jButton_swap = new JButton();
  JButton jButton_pop = new JButton();
  JButton jButton_dup = new JButton();
  JTextArea jTextArea_Current = new JTextArea();
  JTextArea jTextArea_Stack_3 = new JTextArea();
  JTextArea jTextArea_Stack_2 = new JTextArea();
  JTextArea jTextArea_Stack_1 = new JTextArea();
  JTextArea jTextArea_Stack_0 = new JTextArea();
  BorderLayout borderLayout1 = new BorderLayout();

  PolyGraph pg = null;

  RatPolyStack stack = null;
  Border border2;
  Border border3;
  Border border4;
  Border border5;
  JPanel jPanel7 = new JPanel();
  JButton jButton_stack_mul = new JButton();
  JButton jButton_stack_div = new JButton();
  JButton jButton_stack_add = new JButton();
  JButton jButton_stack_sub = new JButton();
  JButton jButton_Clear = new JButton();



  public PolyCalcFrame() {
    try
    {
      jbInit();
      // exit the application when this window is closed
      addWindowListener(new WindowAdapter() {
	  public void windowClosing(WindowEvent e) {
	    setVisible(false);
	    dispose();
	    System.exit(0);
	  }
	});
    }
    catch(Exception e)
    {
      e.printStackTrace();
    }
  }
  private void jbInit() throws Exception
  {
    border1 = BorderFactory.createBevelBorder(BevelBorder.LOWERED,Color.white,Color.white,Color.white,new Color(124, 124, 124));
    border2 = BorderFactory.createBevelBorder(BevelBorder.LOWERED,Color.red,Color.white,new Color(178, 178, 178),new Color(124, 124, 124));
    border3 = BorderFactory.createBevelBorder(BevelBorder.LOWERED,Color.blue,Color.white,new Color(178, 178, 178),new Color(124, 124, 124));
    border4 = BorderFactory.createBevelBorder(BevelBorder.LOWERED,Color.green,Color.white,new Color(178, 178, 178),new Color(124, 124, 124));
    border5 = BorderFactory.createBevelBorder(BevelBorder.LOWERED,Color.orange,Color.white,new Color(178, 178, 178),new Color(124, 124, 124));
    jPanel1.setLayout(null);
    jPanel1.setBackground(Color.gray);
    jPanel1.setBorder(BorderFactory.createRaisedBevelBorder());
    jPanel2.setBorder(BorderFactory.createRaisedBevelBorder());
    jPanel2.setBounds(new Rectangle(21, 21, 330, 371));
    jPanel2.setLayout(borderLayout1);
    jPanel3.setBorder(BorderFactory.createRaisedBevelBorder());
    jPanel3.setBounds(new Rectangle(366, 23, 259, 219));
    jPanel3.setLayout(null);
    jPanel4.setBorder(BorderFactory.createRaisedBevelBorder());
    jPanel4.setBounds(new Rectangle(365, 258, 258, 132));
    jPanel4.setLayout(null);
    jPanel5.setBorder(BorderFactory.createRaisedBevelBorder());
    jPanel5.setBounds(new Rectangle(21, 410, 167, 90));
    jPanel5.setLayout(null);
    jPanel6.setBorder(BorderFactory.createRaisedBevelBorder());
    jPanel6.setBounds(new Rectangle(197, 410, 196, 275));
    jPanel6.setLayout(null);
    jLabel5.setText("X max");
    jLabel5.setBounds(new Rectangle(8, 47, 34, 25));
    jLabel6.setBounds(new Rectangle(10, 10, 34, 24));
    jLabel6.setText("X min");
    jTextField1.setText("-1.0");
    jTextField1.setBounds(new Rectangle(50, 15, 104, 22));
    jTextField1.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jTextField1_actionPerformed(e);
      }
    });
    jTextField2.setBounds(new Rectangle(49, 51, 104, 22));
    jTextField2.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jTextField2_actionPerformed(e);
      }
    });
    jTextField2.setText("1.0");
    jButton_exp.setFont(new java.awt.Font("Dialog", 0, 18));
    jButton_exp.setMargin(new Insets(2, 2, 2, 2));
    jButton_exp.setText("^");
    jButton_exp.setBounds(new Rectangle(9, 10, 41, 44));
    jButton_exp.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_exp_actionPerformed(e);
      }
    });
    jButton_div.setBounds(new Rectangle(103, 10, 41, 44));
    jButton_div.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_div_actionPerformed(e);
      }
    });
    jButton_div.setText("/");
    jButton_div.setFont(new java.awt.Font("Dialog", 0, 18));
    jButton_div.setMargin(new Insets(2, 2, 2, 2));
    jButton_mul.setFont(new java.awt.Font("Dialog", 0, 18));
    jButton_mul.setMargin(new Insets(2, 2, 2, 2));
    jButton_mul.setText("*");
    jButton_mul.setBounds(new Rectangle(55, 10, 41, 44));
    jButton_mul.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_mul_actionPerformed(e);
      }
    });
    jButton_7.setBounds(new Rectangle(9, 60, 41, 44));
    jButton_7.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_7_actionPerformed(e);
      }
    });
    jButton_7.setFont(new java.awt.Font("Dialog", 1, 18));
    jButton_7.setHorizontalTextPosition(SwingConstants.CENTER);
    jButton_7.setMargin(new Insets(2, 2, 2, 2));
    jButton_7.setText("7");
    jButton_8.setText("8");
    jButton_8.setHorizontalTextPosition(SwingConstants.CENTER);
    jButton_8.setMargin(new Insets(2, 2, 2, 2));
    jButton_8.setFont(new java.awt.Font("Dialog", 1, 18));
    jButton_8.setBounds(new Rectangle(55, 60, 41, 44));
    jButton_8.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_8_actionPerformed(e);
      }
    });
    jButton_9.setBounds(new Rectangle(103, 60, 41, 44));
    jButton_9.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_9_actionPerformed(e);
      }
    });
    jButton_9.setFont(new java.awt.Font("Dialog", 1, 18));
    jButton_9.setHorizontalTextPosition(SwingConstants.CENTER);
    jButton_9.setMargin(new Insets(2, 2, 2, 2));
    jButton_9.setText("9");
    jButton_4.setText("4");
    jButton_4.setHorizontalTextPosition(SwingConstants.CENTER);
    jButton_4.setMargin(new Insets(2, 2, 2, 2));
    jButton_4.setFont(new java.awt.Font("Dialog", 1, 18));
    jButton_4.setBounds(new Rectangle(9, 111, 41, 44));
    jButton_4.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_4_actionPerformed(e);
      }
    });
    jButton_5.setBounds(new Rectangle(55, 111, 41, 44));
    jButton_5.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_5_actionPerformed(e);
      }
    });
    jButton_5.setFont(new java.awt.Font("Dialog", 1, 18));
    jButton_5.setHorizontalTextPosition(SwingConstants.CENTER);
    jButton_5.setMargin(new Insets(2, 2, 2, 2));
    jButton_5.setText("5");
    jButton_6.setText("6");
    jButton_6.setHorizontalTextPosition(SwingConstants.CENTER);
    jButton_6.setMargin(new Insets(2, 2, 2, 2));
    jButton_6.setFont(new java.awt.Font("Dialog", 1, 18));
    jButton_6.setBounds(new Rectangle(103, 111, 41, 44));
    jButton_6.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_6_actionPerformed(e);
      }
    });
    jButton_1.setBounds(new Rectangle(9, 163, 41, 44));
    jButton_1.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_1_actionPerformed(e);
      }
    });
    jButton_1.setFont(new java.awt.Font("Dialog", 1, 18));
    jButton_1.setHorizontalTextPosition(SwingConstants.CENTER);
    jButton_1.setMargin(new Insets(2, 2, 2, 2));
    jButton_1.setText("1");
    jButton_2.setText("2");
    jButton_2.setHorizontalTextPosition(SwingConstants.CENTER);
    jButton_2.setMargin(new Insets(2, 2, 2, 2));
    jButton_2.setFont(new java.awt.Font("Dialog", 1, 18));
    jButton_2.setBounds(new Rectangle(55, 163, 41, 44));
    jButton_2.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_2_actionPerformed(e);
      }
    });
    jButton_3.setBounds(new Rectangle(103, 163, 41, 44));
    jButton_3.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_3_actionPerformed(e);
      }
    });
    jButton_3.setFont(new java.awt.Font("Dialog", 1, 18));
    jButton_3.setHorizontalTextPosition(SwingConstants.CENTER);
    jButton_3.setMargin(new Insets(2, 2, 2, 2));
    jButton_3.setText("3");
    jButton_0.setText("0");
    jButton_0.setHorizontalTextPosition(SwingConstants.CENTER);
    jButton_0.setMargin(new Insets(2, 2, 2, 2));
    jButton_0.setFont(new java.awt.Font("Dialog", 1, 18));
    jButton_0.setBounds(new Rectangle(9, 218, 89, 44));
    jButton_0.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_0_actionPerformed(e);
      }
    });
    jButton_x.setText("x");
    jButton_x.setHorizontalTextPosition(SwingConstants.CENTER);
    jButton_x.setMargin(new Insets(2, 2, 2, 2));
    jButton_x.setFont(new java.awt.Font("Dialog", 1, 18));
    jButton_x.setBounds(new Rectangle(103, 218, 41, 44));
    jButton_x.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_x_actionPerformed(e);
      }
    });
    jButton_Enter.setBounds(new Rectangle(8, 171, 77, 44));
    jButton_Enter.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_Enter_actionPerformed(e);
      }
    });
    jButton_Enter.setFont(new java.awt.Font("Dialog", 1, 18));
    jButton_Enter.setHorizontalTextPosition(SwingConstants.CENTER);
    jButton_Enter.setMargin(new Insets(2, 2, 2, 2));
    jButton_Enter.setText("Enter");
    jButton_plus.setBounds(new Rectangle(149, 10, 41, 44));
    jButton_plus.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_plus_actionPerformed(e);
      }
    });
    jButton_plus.setFont(new java.awt.Font("Dialog", 1, 18));
    jButton_plus.setHorizontalTextPosition(SwingConstants.CENTER);
    jButton_plus.setMargin(new Insets(2, 2, 2, 2));
    jButton_plus.setText("+");
    jButton_minus.setText("-");
    jButton_minus.setHorizontalTextPosition(SwingConstants.CENTER);
    jButton_minus.setMargin(new Insets(2, 2, 2, 2));
    jButton_minus.setFont(new java.awt.Font("Dialog", 1, 18));
    jButton_minus.setBounds(new Rectangle(149, 61, 41, 44));
    jButton_minus.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_minus_actionPerformed(e);
      }
    });
    jButton_swap.setText("Swap");
    jButton_swap.setHorizontalTextPosition(SwingConstants.CENTER);
    jButton_swap.setMargin(new Insets(2, 2, 2, 2));
    jButton_swap.setFont(new java.awt.Font("Dialog", 1, 18));
    jButton_swap.setBounds(new Rectangle(8, 8, 75, 44));
    jButton_swap.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_swap_actionPerformed(e);
      }
    });
    jButton_pop.setBounds(new Rectangle(8, 63, 75, 44));
    jButton_pop.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_pop_actionPerformed(e);
      }
    });
    jButton_pop.setFont(new java.awt.Font("Dialog", 1, 18));
    jButton_pop.setHorizontalTextPosition(SwingConstants.CENTER);
    jButton_pop.setMargin(new Insets(2, 2, 2, 2));
    jButton_pop.setText("Pop");
    jButton_dup.setText("Dup");
    jButton_dup.setHorizontalTextPosition(SwingConstants.CENTER);
    jButton_dup.setMargin(new Insets(2, 2, 2, 2));
    jButton_dup.setFont(new java.awt.Font("Dialog", 1, 18));
    jButton_dup.setBounds(new Rectangle(8, 118, 76, 44));
    jButton_dup.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_dup_actionPerformed(e);
      }
    });
    jTextArea_Current.setLineWrap(true);
    jTextArea_Current.setBorder(BorderFactory.createLoweredBevelBorder());
    jTextArea_Current.setBounds(new Rectangle(8, 11, 239, 111));
    jTextArea_Stack_3.setLineWrap(true);
    jTextArea_Stack_3.setBorder(border5);
    jTextArea_Stack_3.setEditable(false);
    jTextArea_Stack_3.setBounds(new Rectangle(8, 10, 241, 40));
    jTextArea_Stack_2.setBounds(new Rectangle(8, 57, 241, 40));
    jTextArea_Stack_2.setLineWrap(true);
    jTextArea_Stack_2.setBorder(border4);
    jTextArea_Stack_2.setEditable(false);
    jTextArea_Stack_1.setLineWrap(true);
    jTextArea_Stack_1.setBorder(border3);
    jTextArea_Stack_1.setEditable(false);
    jTextArea_Stack_1.setBounds(new Rectangle(8, 105, 241, 40));
    jTextArea_Stack_0.setLineWrap(true);
    jTextArea_Stack_0.setBorder(border2);
    jTextArea_Stack_0.setEditable(false);
    jTextArea_Stack_0.setBounds(new Rectangle(8, 154, 241, 40));
    jPanel7.setBorder(BorderFactory.createRaisedBevelBorder());
    jPanel7.setBounds(new Rectangle(403, 411, 199, 283));
    jPanel7.setLayout(null);
    jButton_stack_mul.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_stack_mul_actionPerformed(e);
      }
    });
    jButton_stack_mul.setBounds(new Rectangle(105, 118, 75, 44));
    jButton_stack_mul.setFont(new java.awt.Font("Dialog", 1, 18));
    jButton_stack_mul.setMargin(new Insets(2, 2, 2, 2));
    jButton_stack_mul.setHorizontalTextPosition(SwingConstants.CENTER);
    jButton_stack_mul.setText("Mul");
    jButton_stack_div.setText("Div");
    jButton_stack_div.setHorizontalTextPosition(SwingConstants.CENTER);
    jButton_stack_div.setMargin(new Insets(2, 2, 2, 2));
    jButton_stack_div.setFont(new java.awt.Font("Dialog", 1, 18));
    jButton_stack_div.setBounds(new Rectangle(105, 171, 75, 44));
    jButton_stack_div.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_stack_div_actionPerformed(e);
      }
    });
    jButton_stack_add.setText("Add");
    jButton_stack_add.setHorizontalTextPosition(SwingConstants.CENTER);
    jButton_stack_add.setMargin(new Insets(2, 2, 2, 2));
    jButton_stack_add.setFont(new java.awt.Font("Dialog", 1, 18));
    jButton_stack_add.setBounds(new Rectangle(105, 8, 75, 44));
    jButton_stack_add.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_stack_add_actionPerformed(e);
      }
    });
    jButton_stack_sub.setText("Sub");
    jButton_stack_sub.setHorizontalTextPosition(SwingConstants.CENTER);
    jButton_stack_sub.setMargin(new Insets(2, 2, 2, 2));
    jButton_stack_sub.setFont(new java.awt.Font("Dialog", 1, 18));
    jButton_stack_sub.setBounds(new Rectangle(105, 63, 75, 44));
    jButton_stack_sub.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_stack_sub_actionPerformed(e);
      }
    });
    jButton_Clear.setText("Clear");
    jButton_Clear.setMargin(new Insets(2, 2, 2, 2));
    jButton_Clear.setHorizontalTextPosition(SwingConstants.CENTER);
    jButton_Clear.setFont(new java.awt.Font("Dialog", 1, 18));
    jButton_Clear.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jButton_Clear_actionPerformed(e);
      }
    });
    jButton_Clear.setBounds(new Rectangle(105, 225, 77, 44));
    this.getContentPane().add(jPanel1, BorderLayout.CENTER);
    jPanel1.add(jPanel2, null);
    jPanel1.add(jPanel3, null);
    jPanel3.add(jTextArea_Stack_3, null);
    jPanel3.add(jTextArea_Stack_2, null);
    jPanel3.add(jTextArea_Stack_0, null);
    jPanel3.add(jTextArea_Stack_1, null);
    jPanel1.add(jPanel4, null);
    jPanel4.add(jTextArea_Current, null);
    jPanel1.add(jPanel5, null);
    jPanel5.add(jLabel6, null);
    jPanel5.add(jLabel5, null);
    jPanel5.add(jTextField1, null);
    jPanel5.add(jTextField2, null);
    jPanel1.add(jPanel6, null);
    jPanel6.add(jButton_exp, null);
    jPanel6.add(jButton_8, null);
    jPanel6.add(jButton_9, null);
    jPanel6.add(jButton_4, null);
    jPanel6.add(jButton_5, null);
    jPanel6.add(jButton_6, null);
    jPanel6.add(jButton_3, null);
    jPanel6.add(jButton_0, null);
    jPanel6.add(jButton_x, null);
    jPanel6.add(jButton_div, null);
    jPanel6.add(jButton_plus, null);
    jPanel6.add(jButton_mul, null);
    jPanel6.add(jButton_7, null);
    jPanel6.add(jButton_2, null);
    jPanel6.add(jButton_1, null);
    jPanel6.add(jButton_minus, null);
    jPanel1.add(jPanel7, null);
    jPanel7.add(jButton_pop, null);
    jPanel7.add(jButton_dup, null);
    jPanel7.add(jButton_Enter, null);
    jPanel7.add(jButton_swap, null);
    jPanel7.add(jButton_stack_div, null);
    jPanel7.add(jButton_stack_mul, null);
    jPanel7.add(jButton_stack_add, null);
    jPanel7.add(jButton_stack_sub, null);
    jPanel7.add(jButton_Clear, null);

    pg = new PolyGraph(this);
    // pg.setBackground(Color.white);
    jPanel2.add(pg, "Center");
  }

  /**Main method*/
  public static void main(String[] args) {
    try {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
    }
    catch(Exception e) {
      e.printStackTrace();
    }

    PolyCalcFrame frame = new PolyCalcFrame();

    //Validate frames that have preset sizes
    frame.validate();

    //Center the window
    Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();

    frame.setSize(new Dimension(670, 730));
    frame.setTitle("Polynomial Calculator");

    Dimension frameSize = frame.getSize();

    if (frameSize.height > screenSize.height) {
      frameSize.height = screenSize.height;
    }
    if (frameSize.width > screenSize.width) {
      frameSize.width = screenSize.width;
    }
    frame.setLocation((screenSize.width - frameSize.width) / 2, (screenSize.height - frameSize.height) / 2);
    frame.setVisible(true);
  }
    
  boolean startNewEq = false;
  void appendToCurrentText(String textToAdd)
  {
      if(startNewEq) {
	  jTextArea_Current.setText("");
	  startNewEq = false;
      }
    String currentText = jTextArea_Current.getText();
    jTextArea_Current.setText(currentText + textToAdd);
  }

  void jButton_1_actionPerformed(ActionEvent e)
  {
    appendToCurrentText("1");
  }

  void jButton_2_actionPerformed(ActionEvent e)
  {
    appendToCurrentText("2");
  }

  void jButton_3_actionPerformed(ActionEvent e)
  {
    appendToCurrentText("3");
  }

  void jButton_4_actionPerformed(ActionEvent e)
  {
    appendToCurrentText("4");
  }

  void jButton_5_actionPerformed(ActionEvent e)
  {
    appendToCurrentText("5");
  }

  void jButton_6_actionPerformed(ActionEvent e)
  {
    appendToCurrentText("6");
  }

  void jButton_7_actionPerformed(ActionEvent e)
  {
    appendToCurrentText("7");
  }

  void jButton_8_actionPerformed(ActionEvent e)
  {
    appendToCurrentText("8");
  }

  void jButton_9_actionPerformed(ActionEvent e)
  {
    appendToCurrentText("9");
  }

  void jButton_exp_actionPerformed(ActionEvent e)
  {
      if (startNewEq == true) 
	  startNewEq = false;
    appendToCurrentText("^");
  }

  void jButton_mul_actionPerformed(ActionEvent e)
  {
      if (startNewEq == true) 
	  startNewEq = false;
    appendToCurrentText("*");
  }

  void jButton_div_actionPerformed(ActionEvent e)
  {
      if (startNewEq == true) 
	  startNewEq = false;
    appendToCurrentText("/");
  }

  void jButton_plus_actionPerformed(ActionEvent e)
  {
      if (startNewEq == true) 
	  startNewEq = false;
    appendToCurrentText("+");
  }

  void jButton_minus_actionPerformed(ActionEvent e)
  {
      if (startNewEq == true) 
	  startNewEq = false;
    appendToCurrentText("-");
  }

  void jButton_0_actionPerformed(ActionEvent e)
  {
    appendToCurrentText("0");
  }

  void jButton_x_actionPerformed(ActionEvent e)
  {
    appendToCurrentText("x");
  }

  void jButton_swap_actionPerformed(ActionEvent e)
  {
   try{
    if (stack != null)
      if (stack.size() > 1)
	  stack.swap();
    // Update displays (so that they display new stack contents).
    updateStackDisplay();
    updateGraph();
   } catch (Exception e1) {
       e1.printStackTrace();
       return;
   }
  }

  void jButton_pop_actionPerformed(ActionEvent e)
  {
   try{
    if (stack != null) {
     if (stack.size() > 0) {
      RatPoly popped = stack.pop();
      jTextArea_Current.setText(popped.unparse());
      startNewEq = true;
     }
    }
    // Update displays (so that they display new stack contents).
    updateStackDisplay();
    updateGraph();
   } catch (Exception e1) {
       e1.printStackTrace();
       return;
   }
  }

  void jButton_dup_actionPerformed(ActionEvent e)
  {
   try {
    if (stack != null)
      if (stack.size() > 0)
	  stack.dup();
    // Update displays (so that they display new stack contents).
    updateStackDisplay();
    updateGraph();
   } catch (Exception e1) {
       e1.printStackTrace();
       return;
   }
  }

  void jButton_Enter_actionPerformed(ActionEvent e)
  {
    String currentText = jTextArea_Current.getText();
    RatPoly parsedRatPoly;

    startNewEq = true;
    jTextArea_Current.setText("");

    // Exception handler used here since some of this calls what will
    // ultimately be student-written code.
    try
    {
      // Create stack if necessary
      if (stack == null)
        stack = new RatPolyStack();

      // Parse text to get new RatPoly and push it onto the stack.
      parsedRatPoly = RatPoly.parse(currentText);
      stack.push(parsedRatPoly);

      // Update displays (so that they display new stack contents).
      updateStackDisplay();
      updateGraph();
    }
    catch(Exception e1)
    {
      // Provide more informative message here.
      jTextArea_Current.setText(currentText);
      e1.printStackTrace();
      return;
    }

  }



  void updateStackDisplay()
  {
    // First, clear all the text areas for the stack display.
    jTextArea_Stack_0.setText("");
    jTextArea_Stack_1.setText("");
    jTextArea_Stack_2.setText("");
    jTextArea_Stack_3.setText("");

    RatPoly currentRatPoly;
    String tempString;

    if (stack != null)
    {

    // Now fill in new information base on what's in stack.
    // Note that size of stack must be checked.
    if (stack.size() > 0)
    {
      currentRatPoly = stack.get(0);
      tempString = currentRatPoly.unparse();
      jTextArea_Stack_0.setText(tempString);
    }

    if (stack.size() > 1)
    {
      currentRatPoly = stack.get(1);
      tempString = currentRatPoly.unparse();
      jTextArea_Stack_1.setText(tempString);
    }

    if (stack.size() > 2)
    {
      currentRatPoly = stack.get(2);
      tempString = currentRatPoly.unparse();
      jTextArea_Stack_2.setText(tempString);
    }

    if (stack.size() > 3)
    {
      currentRatPoly = stack.get(3);
      tempString = currentRatPoly.unparse();
      jTextArea_Stack_3.setText(tempString);
    }
    // Consider abstracting this better!  This would require
    // putting the text areas into an array.

    }

  }



  void updateGraph()
  {
    pg.repaint();
  }

  void jTextField1_actionPerformed(ActionEvent e)
  {
    updateGraph();
  }

  void jTextField2_actionPerformed(ActionEvent e)
  {
    updateGraph();
  }
  void jButton_stack_mul_actionPerformed(ActionEvent e)
  {
    try {
      if (stack != null)
        if (stack.size() > 1)
	    stack.mul();
      // Update displays (so that they display new stack contents).
      updateStackDisplay();
      updateGraph();
    } catch (Exception e1) {
        e1.printStackTrace();
        return;
    }
  }
  void jButton_stack_div_actionPerformed(ActionEvent e)
  {
    try {
      if (stack != null)
        if (stack.size() > 1)
	    stack.div();
      // Update displays (so that they display new stack contents).
      updateStackDisplay();
      updateGraph();
    } catch (Exception e1) {
        e1.printStackTrace();
        return;
    }
  }
  void jButton_stack_add_actionPerformed(ActionEvent e)
  {
    try {
      if (stack != null)
        if (stack.size() > 1)
	    stack.add();
      // Update displays (so that they display new stack contents).
      updateStackDisplay();
      updateGraph();
    } catch (Exception e1) {
        e1.printStackTrace();
        return;
    }
  }
  void jButton_stack_sub_actionPerformed(ActionEvent e)
  {
    try {
      if (stack != null)
        if (stack.size() > 1)
	    stack.sub();
      // Update displays (so that they display new stack contents).
      updateStackDisplay();
      updateGraph();
    } catch (Exception e1) {
        e1.printStackTrace();
        return;
    }
  }
  void jButton_Clear_actionPerformed(ActionEvent e)
  {
    try {
      if (stack != null)
        if (stack.size() > 0)
	    stack.clear();
      // Update displays (so that they display new stack contents).
      updateStackDisplay();
      updateGraph();
    } catch (Exception e1) {
        e1.printStackTrace();
        return;
    }
  }

}
