package PolyCalc;

import java.awt.*;

/**
 * Title:        6.170 PS1
 * Description:  6.170 Problem Set 1
 * Copyright:    Copyright (c) 2001
 * Company:      MIT
 * @author Felix Klock, Andreas Hofmann
 * @version 1.0
 */

public class PolyGraph extends Canvas
{
  Image img= null;
  int imgw=-1, imgh=-1;
  Color col[]= new Color[4];
  Color zeroline= new Color(0xe0, 0xe0, 0xff);
  PolyCalcFrame calcFrame = null;

  public PolyGraph(PolyCalcFrame cf)
  {
    super();

    calcFrame = cf;

    col[0]= new Color(0xa0, 0, 0);
    col[1]= new Color(0, 0, 0xFF);
    col[2]= new Color(0, 0x80, 0);
    col[3]= new Color(0, 0, 0);
  }


/*
	public void update() {
		if (img==null) {
			return;
		}
		// getChange marks the new drawing spot.  gi will only draw to here,
		// even if there is new data!
		int scrollamt= gi.getChange(imgw, imgh-17);
		if (scrollamt< -imgw) {return;}
		Graphics g= img.getGraphics();
		if (scrollamt>-imgw) {
			g.copyArea(0, 0, imgw, imgh-16, -scrollamt, 0);
		}
		if (scrollamt>0) {
			g.setColor(Color.white);
			g.fillRect(imgw-scrollamt, 0, scrollamt, imgh-16);
			g.setColor(zeroline);
			int y= gi.valueToY(0);
			if (y>0 && y<imgh-16) {
				g.drawLine(imgw-scrollamt, y, scrollamt, y);
			}
		} else if (scrollamt<0) {
			g.setColor(Color.white);
			g.fillRect(0, 0, -scrollamt, imgh-16);
			g.setColor(zeroline);
			int y= gi.valueToY(0);
			if (y>0 && y<imgh-16) {
				g.drawLine(0, y, -scrollamt-1, y);
			}
		}
		if (scrollamt>imgw) {
			g.setColor(Color.lightGray);
			g.fillRect(0, imgh-16, imgw, 16);
		}
//		System.out.println("Updating: scroll "+scrollamt);
		for (int i=0; i<gi.getNVars(); i++) {
			// getRecentData includes the last datapoint drawn
			// i.e. if there is one new point, len=2
			int len= gi.getRecentData(i);
//			System.out.println("  "+gi.getVarName(i)+": "+len);
			// draw data
			g.setColor(col[i]);
			g.drawPolyline(gi.getX(), gi.getY(), len);
			g.drawString(gi.getVarName(i), i*60+1, imgh-3);
		}
		// draw info along bottom
		g.setColor(Color.black);
		g.drawLine(0, imgh-16, imgw, imgh-16);
		repaint();
	}
        */

  public void update(Graphics g) {
    paint(g);
  }

  public void paint(Graphics g) {
    int w= getSize().width;
    int h= getSize().height;
    if (img==null || w!=imgw || h!=imgh) {
      img= createImage(w, h);
      imgw= w;
      imgh= h;
                        /*
			if (gi.getNVars()!=0) {
				update();
				return;
			}
                        */
    }

    g.setColor(Color.white);
    g.fillRect(0, 0, w, h);

    /*
    g.setColor(Color.red);
    String msg= "Click to graph selected variable";
    int wid= getFontMetrics(getFont()).stringWidth(msg);
    g.drawString(msg, (w-wid)/2, h/2);
    */


    // g.drawImage(img, 0, 0, w, h, Color.white, this);
    // img not really used.  Get rid of it?  It is intended for
    // double buffering.

    // g.drawLine(0, 0, imgw-16, imgh-16);


    // Draw axes and data
    int numIncrements = 100;
    int i;

    float xMin = Float.parseFloat(calcFrame.jTextField1.getText());
    float xMax = Float.parseFloat(calcFrame.jTextField2.getText());
    float yVal = 0;
    float yMin = 0;
    float yMax = 0;
    float[] xValBuffer1 = null;
    float[] yValBuffer1 = null;
    float[] xValBuffer2 = null;
    float[] yValBuffer2 = null;
    float[] xValBuffer3 = null;
    float[] yValBuffer3 = null;
    float[] xValBuffer4 = null;
    float[] yValBuffer4 = null;
    float[] yExtrema;
    String msg;

    if (xMin >= xMax)
    {
      g.setColor(Color.red);
      msg= "Xmin must be greater than Xmax";
      int wid= getFontMetrics(getFont()).stringWidth(msg);
      g.drawString(msg, (w-wid)/2, h/2);
      return;
    }

    // Get RatPoly
    RatPoly currentRatPoly;


    // Now fill in new information base on what's in stack.
    // Note that size of stack must be checked.
    if ((calcFrame.stack != null) &&
        (calcFrame.stack.size() > 0))
    {
      currentRatPoly = calcFrame.stack.get(0);
      xValBuffer1 = new float[numIncrements];
      yValBuffer1 = new float[numIncrements];
      yExtrema = new float[2];
    }
    else
    {
      g.setColor(Color.red);
      msg= "Stack is empty";
      int wid= getFontMetrics(getFont()).stringWidth(msg);
      g.drawString(msg, (w-wid)/2, h/2);
      return;
    }


    updatePlotBuffer(xMin, xMax, numIncrements,
                     xValBuffer1, yValBuffer1,
                     yExtrema, currentRatPoly);

    yMin = yExtrema[0];
    yMax = yExtrema[1];

    if (calcFrame.stack.size() > 1)
    {
      currentRatPoly = calcFrame.stack.get(1);
      xValBuffer2 = new float[numIncrements];
      yValBuffer2 = new float[numIncrements];

      updatePlotBuffer(xMin, xMax, numIncrements,
                       xValBuffer2, yValBuffer2,
                       yExtrema, currentRatPoly);

      if (yExtrema[0] < yMin)
        yMin = yExtrema[0];

      if (yExtrema[1] > yMax)
        yMax = yExtrema[1];
    }

    if (calcFrame.stack.size() > 2)
    {
      currentRatPoly = calcFrame.stack.get(2);
      xValBuffer3 = new float[numIncrements];
      yValBuffer3 = new float[numIncrements];

      updatePlotBuffer(xMin, xMax, numIncrements,
                       xValBuffer3, yValBuffer3,
                       yExtrema, currentRatPoly);

      if (yExtrema[0] < yMin)
        yMin = yExtrema[0];

      if (yExtrema[1] > yMax)
        yMax = yExtrema[1];
    }

    if (calcFrame.stack.size() > 3)
    {
      currentRatPoly = calcFrame.stack.get(3);
      xValBuffer4 = new float[numIncrements];
      yValBuffer4 = new float[numIncrements];

      updatePlotBuffer(xMin, xMax, numIncrements,
                       xValBuffer4, yValBuffer4,
                       yExtrema, currentRatPoly);

      if (yExtrema[0] < yMin)
        yMin = yExtrema[0];

      if (yExtrema[1] > yMax)
        yMax = yExtrema[1];
    }



    // At this point, min and max have been computed, and buffers
    // are full.  Draw axes, then draw graph line.
    int bord = 32;
    g.setColor(Color.black);
    g.drawLine(bord, h-bord, w-bord, h-bord);  // horizontal axis
    g.drawLine(bord, bord, bord, h-bord);  // vertical axis

    float gw = w - 2 * bord;  // width of graph area inside axes
    float gh = h - 2 * bord;  // height of graph area inside axes

    // Draw axis labels.
    msg = Float.toString(xMin);
    g.drawString(msg, bord, h-8);

    msg = Float.toString(xMax);
    g.drawString(msg, w-bord, h-8);

    msg = Float.toString(yMin);
    g.drawString(msg, 8, h-bord);

    msg = Float.toString(yMax);
    g.drawString(msg, 8, bord);

    // Draw graph line.
    g.setColor(Color.red);
    drawPlot(xMin, xMax, yMin, yMax,
             xValBuffer1, yValBuffer1,
             gw, gh, bord,
             numIncrements, h, g);

    g.setColor(Color.blue);
    if (calcFrame.stack.size() > 1)
    {
      drawPlot(xMin, xMax, yMin, yMax,
             xValBuffer2, yValBuffer2,
             gw, gh, bord,
             numIncrements, h, g);
    }

    g.setColor(Color.green);
    if (calcFrame.stack.size() > 2)
    {
      drawPlot(xMin, xMax, yMin, yMax,
             xValBuffer3, yValBuffer3,
             gw, gh, bord,
             numIncrements, h, g);
    }

    g.setColor(Color.orange);
    if (calcFrame.stack.size() > 3)
    {
      drawPlot(xMin, xMax, yMin, yMax,
             xValBuffer4, yValBuffer4,
             gw, gh, bord,
             numIncrements, h, g);
    }

    // Consider abstracting this better!
  }



  public void updatePlotBuffer(float xMin, float xMax, int numIncrements,
                               float xValBuffer[], float yValBuffer[],
                               float yExtrema[], RatPoly currentRatPoly)
  {
    float delta = (xMax - xMin) / numIncrements;
    float currentX = xMin;
    boolean firstTime = true;
    int i;
    float yVal = 0;
    float yMin = 0;
    float yMax = 0;

    for(i=0; i<numIncrements; ++i)
    {
      if(currentX < xMax)
      {
        xValBuffer[i] = currentX;
        yVal = (float)currentRatPoly.eval(currentX);
        yValBuffer[i] = yVal;

        if(firstTime)
        {
          firstTime = false;
          yMin = yVal;
          yMax = yVal;
        }
        else
        {
          if(yVal < yMin)
            yMin = yVal;

          if(yVal > yMax)
            yMax = yVal;
        }

        currentX += delta;
      }
      else
      {
        xValBuffer[i] = xValBuffer[i - 1];
        yValBuffer[i] = yValBuffer[i - 1];
      }
    }

    yExtrema[0] = yMin;
    yExtrema[1] = yMax;
  }



  public void drawPlot(float xMin, float xMax, float yMin, float yMax,
                       float xValBuffer[], float yValBuffer[],
                       float gw, float gh, int bord,
                       int numIncrements, int h, Graphics g)
  {
    float xVal = 0;
    float yVal = 0;
    float previousX = 0;
    float previousY = 0;
    boolean firstTime = true;
    float xRange = xMax - xMin;
    float yRange = yMax - yMin;
    int xPrevScaled = 0;
    int yPrevScaled = 0;
    int xScaled = 0;
    int yScaled = 0;
    int i;
    for(i=0; i<numIncrements; ++i)
    {
      if(firstTime)
      {
        firstTime = false;
        xVal = xValBuffer[i];
        yVal = yValBuffer[i];
        previousX = xVal;
        previousY = yVal;
      }
      else
      {
        previousX = xVal;
        previousY = yVal;
        xVal = xValBuffer[i];
        yVal = yValBuffer[i];
      }

      // Now draw a line from previous to current.
      xPrevScaled = Math.round((previousX - xMin) * gw / xRange);
      yPrevScaled = Math.round((previousY - yMin) * gh / yRange);
      xScaled = Math.round((xVal - xMin) * gw / xRange);
      yScaled = Math.round((yVal - yMin) * gh / yRange);

      g.drawLine(bord + xPrevScaled, h - bord - yPrevScaled,
                 bord + xScaled, h - bord - yScaled);
    }
  }

}
