package com.catehuston.imagefilter.model;

import processing.core.PApplet;

import com.catehuston.imagefilter.color.ColorHelper;

public class ImageState {
	
	ColorHelper colorHelper;
	IFAImage image;
	String filepath;
	
	public static final int initialHueTolerance = 5;
	
	boolean dominantHueHidden = false;
	boolean dominantHueShowing = false;
	int blueFilter = 0;
	int greenFilter = 0;
	int hueTolerance = 0;
	int redFilter = 0;
	
	public ImageState(ColorHelper colorHelper) {
		this.colorHelper = colorHelper;
		image = new IFAImage();
		hueTolerance = initialHueTolerance;
	}
	
	public IFAImage image() {
		return image;
	}

	public int blueFilter() {
		return blueFilter;
	}
	
	public int greenFilter() {
		return greenFilter;
	}
	
	public int redFilter() {
		return redFilter;
	}
	
	public int hueTolerance() {
		return hueTolerance;
	}
	
	public boolean dominantHueShowing() {
		return dominantHueShowing;
	}
	
	public boolean dominantHueHidden() {
		return dominantHueHidden;
	}
	
	public void setFilepath(String filepath) {
		this.filepath = filepath;
	}
	
	public String filepath() {
		return filepath;
	}
	
	public void updateImage(PApplet applet, int hueRange, int rgbColorRange) {
		if (dominantHueShowing) {
			colorHelper.processImageForHue(applet, image, hueRange, hueTolerance, true);
		} else if (dominantHueHidden) {
			colorHelper.processImageForHue(applet, image, hueRange, hueTolerance, false);
		}
		colorHelper.applyColorFilter(applet, image, redFilter, greenFilter,
				blueFilter, rgbColorRange);
		image.updatePixels();
	}

	public void processKeyPress(char key, int inc, int rgbColorRange, int hueIncrement, int hueRange) {
		switch (key) {
		 case 'r':
			 redFilter+=inc;
			 redFilter = Math.min(redFilter, rgbColorRange);
			 break;
		 case 'e':
			 redFilter-=inc;
			 redFilter = Math.max(redFilter, 0);
			 break;
		 case 'g':
			 greenFilter+=inc;
			 greenFilter = Math.min(greenFilter, rgbColorRange);
			 break;
		 case 'f':
			 greenFilter-=inc;
			 greenFilter = Math.max(greenFilter, 0);
			 break;
		 case 'b':
			 blueFilter+=inc;
			 blueFilter = Math.min(blueFilter, rgbColorRange);
			 break;
		 case 'v':
			 blueFilter-=inc;
			 blueFilter = Math.max(blueFilter, 0);
			 break;
		 case 'i':
			 hueTolerance+=hueIncrement;
			 hueTolerance = Math.min(hueTolerance, hueRange);
			 break;
		 case 'u':
			 hueTolerance-=hueIncrement;
			 hueTolerance = Math.max(hueTolerance, 0);
			 break;
		 case 'h':
			 dominantHueHidden = true;
			 dominantHueShowing = false;
			 break;
		 case 's':
			 dominantHueHidden = false;
			 dominantHueShowing = true;
			 break;
		 }
	}
	
	public void setUpImage(PApplet applet, int imageMax) {
		image.update(applet, filepath);
		// Fix the size.
		if (image.getWidth() > imageMax || image.getHeight() > imageMax) {
			int imgWidth = imageMax;
			int imgHeight = imageMax;
			if (image.getWidth() > image.getHeight()) {
				imgHeight = (imgHeight *  image.getHeight()) / image.getWidth();
			} else {
				imgWidth = (imgWidth * image.getWidth()) / image.getHeight();
			}
			image.resize(imgWidth, imgHeight);
		}
	}
	
	public void resetImage(PApplet applet, int imageMax) {
		redFilter = 0;
		greenFilter = 0;
		blueFilter = 0;
		hueTolerance = initialHueTolerance;
		dominantHueShowing = false;
		dominantHueHidden = false;
		setUpImage(applet, imageMax);
	}
	
	// For testing purposes only.
	protected void set(IFAImage image, boolean dominantHueHidden, boolean dominantHueShowing,
			int redFilter, int greenFilter, int blueFilter, int hueTolerance) {
		this.image = image;
		this.dominantHueHidden = dominantHueHidden;
		this.dominantHueShowing = dominantHueShowing;
		this.blueFilter = blueFilter;
		this.greenFilter = greenFilter;
		this.redFilter = redFilter;
		this.hueTolerance = hueTolerance;
	}
}
