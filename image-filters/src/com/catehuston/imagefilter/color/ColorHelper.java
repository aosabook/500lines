package com.catehuston.imagefilter.color;

import processing.core.PApplet;

import com.catehuston.imagefilter.model.HSBColor;
import com.catehuston.imagefilter.model.IFAImage;

public class ColorHelper {

	private final PixelColorHelper pixelColorHelper;

	public ColorHelper(PixelColorHelper pixelColorHelper) {
		this.pixelColorHelper = pixelColorHelper;
	}

	public boolean hueInRange(float hue, int hueRange, float lower, float upper) {
		// Need to compensate for it being circular - can go around.
		if (lower < 0) {
			lower += hueRange;
		}
		if (upper > hueRange) {
			upper -= hueRange;
		}
		if (lower < upper) {
			return hue < upper && hue > lower;
		} else {
			return hue < upper || hue > lower;
		}
	}

	public HSBColor getDominantHue(PApplet applet, IFAImage image, int hueRange) {
		image.loadPixels();
		int numberOfPixels = image.getPixels().length;
		int[] hues = new int[hueRange];
		float[] saturations = new float[hueRange];
		float[] brightnesses = new float[hueRange];

		for (int i = 0; i < numberOfPixels; i++) {
			int pixel = image.getPixel(i);
			int hue = Math.round(pixelColorHelper.hue(applet, pixel));
			float saturation = pixelColorHelper.saturation(applet, pixel);
			float brightness = pixelColorHelper.brightness(applet, pixel);
			hues[hue]++;
			saturations[hue] += saturation;
			brightnesses[hue] += brightness;
		}

		// Find the most common hue.
		int hueCount = hues[0];
		int hue = 0;
		for (int i = 1; i < hues.length; i++) {
			if (hues[i] > hueCount) {
				hueCount = hues[i];
				hue = i;
			}
		}

		// Return the color to display.
		float s = saturations[hue] / hueCount;
		float b = brightnesses[hue] / hueCount;
		return new HSBColor(hue, s, b);
	}

	public void processImageForHue(PApplet applet, IFAImage image, int hueRange,
			int hueTolerance, boolean showHue) {
		applet.colorMode(PApplet.HSB, (hueRange - 1));
		image.loadPixels();
		int numberOfPixels = image.getPixels().length;
		HSBColor dominantHue = getDominantHue(applet, image, hueRange);
		// Manipulate photo, grayscale any pixel that isn't close to that hue.
		float lower = dominantHue.h - hueTolerance;
		float upper = dominantHue.h + hueTolerance;
		for (int i = 0; i < numberOfPixels; i++) {
			int pixel = image.getPixel(i);
			float hue = pixelColorHelper.hue(applet, pixel);
			if (hueInRange(hue, hueRange, lower, upper) == showHue) {
				float brightness = pixelColorHelper.brightness(applet, pixel);
				image.setPixel(i, pixelColorHelper.color(applet, brightness));
			}
		}
		image.updatePixels();
	}

	public void applyColorFilter(PApplet applet, IFAImage image, int minRed,
			int minGreen, int minBlue, int colorRange) {
		applet.colorMode(PApplet.RGB, colorRange);
		image.loadPixels();
		int numberOfPixels = image.getPixels().length;
		for (int i = 0; i < numberOfPixels; i++) {
			int pixel = image.getPixel(i);
			float alpha = pixelColorHelper.alpha(applet, pixel);
			float red = pixelColorHelper.red(applet, pixel);
			float green = pixelColorHelper.green(applet, pixel);
			float blue = pixelColorHelper.blue(applet, pixel);

			red = (red >= minRed) ? red : 0;
			green = (green >= minGreen) ? green : 0;
			blue = (blue >= minBlue) ? blue : 0;

			image.setPixel(i, pixelColorHelper.color(applet, red, green, blue, alpha));
		}
	}
}
