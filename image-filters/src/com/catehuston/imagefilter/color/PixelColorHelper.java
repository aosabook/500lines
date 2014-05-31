package com.catehuston.imagefilter.color;

import processing.core.PApplet;

public class PixelColorHelper {

	public float alpha(PApplet applet, int pixel) {
		return applet.alpha(pixel);
	}

	public float blue(PApplet applet, int pixel) {
		return applet.blue(pixel);
	}

	public float brightness(PApplet applet, int pixel) {
		return applet.brightness(pixel);
	}

	public int color(PApplet applet, float greyscale) {
		return applet.color(greyscale);
	}

	public int color(PApplet applet, float red, float green, float blue, float alpha) {
		return applet.color(red, green, blue, alpha);
	}

	public float green(PApplet applet, int pixel) {
		return applet.green(pixel);
	}

	public float hue(PApplet applet, int pixel) {
		return applet.hue(pixel);
	}

	public float red(PApplet applet, int pixel) {
		return applet.red(pixel);
	}

	public float saturation(PApplet applet, int pixel) {
		return applet.saturation(pixel);
	}
}
