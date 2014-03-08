package com.catehuston.imagefilter.color;

import processing.core.PApplet;

public class PixelColorHelper {
	
	public float alpha(PApplet applet, int px) {
		return applet.alpha(px);
	}
	
	public float blue(PApplet applet, int px) {
		return applet.blue(px);
	}
	
	public float brightness(PApplet applet, int px) {
		return applet.brightness(px);
	}
	
	public int color(PApplet applet, float greyscale) {
		return applet.color(greyscale);
	}
	
	public int color(PApplet applet, float red, float green, float blue, float alpha) {
		return applet.color(red, green, blue, alpha);
	}
	
	public float green(PApplet applet, int px) {
		return applet.green(px);
	}
	
	public float hue(PApplet applet, int px) {
		return applet.hue(px);
	}
	
	public float red(PApplet applet, int px) {
		return applet.red(px);
	}
	
	public float saturation(PApplet applet, int px) {
		return applet.saturation(px);
	}
}
