package com.catehuston.imagefilter.model;

import processing.core.PApplet;

import com.catehuston.imagefilter.color.ColorHelper;

public class ImageState {

	enum ColorMode {
		COLOR_FILTER,
		SHOW_DOMINANT_HUE,
		HIDE_DOMINANT_HUE
	}

	private final ColorHelper colorHelper;
	private IFAImage image;
	private String filepath;

	public static final int INITIAL_HUE_TOLERANCE = 5;

	ColorMode colorModeState = ColorMode.COLOR_FILTER;
	int blueFilter = 0;
	int greenFilter = 0;
	int hueTolerance = 0;
	int redFilter = 0;

	public ImageState(ColorHelper colorHelper) {
		this.colorHelper = colorHelper;
		image = new IFAImage();
		hueTolerance = INITIAL_HUE_TOLERANCE;
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

	public ColorMode getColorMode() {
		return colorModeState;
	}

	public void setFilepath(String filepath) {
		this.filepath = filepath;
	}

	public String filepath() {
		return filepath;
	}

	public void updateImage(PApplet applet, int hueRange, int rgbColorRange, int imageMax) {
		setUpImage(applet, imageMax);
		if (colorModeState == ColorMode.SHOW_DOMINANT_HUE) {
			colorHelper.processImageForHue(applet, image, hueRange, hueTolerance, true);
		} else if (colorModeState == ColorMode.HIDE_DOMINANT_HUE) {
			colorHelper.processImageForHue(applet, image, hueRange, hueTolerance, false);
		}
		colorHelper.applyColorFilter(applet, image, redFilter, greenFilter,
				blueFilter, rgbColorRange);
		image.updatePixels();
	}

	public void processKeyPress(char key, int inc, int rgbColorRange, int hueIncrement, int hueRange) {
		switch (key) {
		case 'r':
			redFilter += inc;
			redFilter = Math.min(redFilter, rgbColorRange);
			break;
		case 'e':
			redFilter -= inc;
			redFilter = Math.max(redFilter, 0);
			break;
		case 'g':
			greenFilter += inc;
			greenFilter = Math.min(greenFilter, rgbColorRange);
			break;
		case 'f':
			greenFilter -= inc;
			greenFilter = Math.max(greenFilter, 0);
			break;
		case 'b':
			blueFilter += inc;
			blueFilter = Math.min(blueFilter, rgbColorRange);
			break;
		case 'v':
			blueFilter -= inc;
			blueFilter = Math.max(blueFilter, 0);
			break;
		case 'i':
			hueTolerance += hueIncrement;
			hueTolerance = Math.min(hueTolerance, hueRange);
			break;
		case 'u':
			hueTolerance -= hueIncrement;
			hueTolerance = Math.max(hueTolerance, 0);
			break;
		case 'h':
			if (colorModeState == ColorMode.HIDE_DOMINANT_HUE) {
				colorModeState = ColorMode.COLOR_FILTER;
			} else {
				colorModeState = ColorMode.HIDE_DOMINANT_HUE;
			}
			break;
		case 's':
			if (colorModeState == ColorMode.SHOW_DOMINANT_HUE) {
				colorModeState = ColorMode.COLOR_FILTER;
			} else {
				colorModeState = ColorMode.SHOW_DOMINANT_HUE;
			}
			break;
		case 'w':
			image().save(filepath() + "-new.png");
			break;
		}
	}

	public void setUpImage(PApplet applet, int imageMax) {
		image.update(applet, filepath);
		// Fix the size.
		if (image.getWidth() > imageMax || image.getHeight() > imageMax) {
			int width = imageMax;
			int height = imageMax;
			if (image.getWidth() > image.getHeight()) {
				height = (height * image.getHeight()) / image.getWidth();
			} else {
				width = (width * image.getWidth()) / image.getHeight();
			}
			image.resize(width, height);
		}
	}

	public void resetImage(PApplet applet, int imageMax) {
		redFilter = 0;
		greenFilter = 0;
		blueFilter = 0;
		hueTolerance = INITIAL_HUE_TOLERANCE;
		colorModeState = ColorMode.COLOR_FILTER;
		setUpImage(applet, imageMax);
	}

	// For testing purposes only.
	protected void set(IFAImage image, ColorMode colorModeState,
			int redFilter, int greenFilter, int blueFilter, int hueTolerance) {
		this.image = image;
		this.colorModeState = colorModeState;
		this.blueFilter = blueFilter;
		this.greenFilter = greenFilter;
		this.redFilter = redFilter;
		this.hueTolerance = hueTolerance;
	}
}
