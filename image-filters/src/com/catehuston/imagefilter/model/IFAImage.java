package com.catehuston.imagefilter.model;

import processing.core.PApplet;
import processing.core.PImage;

public class IFAImage {

	private PImage image;

	public IFAImage() {
		image = null;
	}

	public PImage image() {
		return image;
	}

	public void update(PApplet applet, String filepath) {
		image = null;
		image = applet.loadImage(filepath);
	}

	// Wrapped methods from PImage.

	public int getHeight() {
		return image.height;
	}

	public int getPixel(int px) {
		return image.pixels[px];
	}

	public int[] getPixels() {
		return image.pixels;
	}

	public int getWidth() {
		return image.width;
	}

	public void loadPixels() {
		image.loadPixels();
	}

	public void resize(int width, int height) {
		image.resize(width, height);
	}

	public void save(String filepath) {
		image.save(filepath);
	}

	public void setPixel(int px, int color) {
		image.pixels[px] = color;
	}

	public void updatePixels() {
		image.updatePixels();
	}
}
