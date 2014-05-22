package com.catehuston.imagefilter.app;

import java.io.File;

import processing.core.PApplet;

import com.catehuston.imagefilter.color.ColorHelper;
import com.catehuston.imagefilter.color.PixelColorHelper;
import com.catehuston.imagefilter.model.ImageState;

@SuppressWarnings("serial")
public class ImageFilterApp extends PApplet {
	
	static final String INSTRUCTIONS = "R: increase red filter\nE: reduce red filter\n"
			+ "G: increase green filter\nF: reduce green filter\nB: increase blue filter\n"
			+ "V: reduce blue filter\nI: increase hue tolerance\nU: reduce hue tolerance\n"
			+ "S: show dominant hue\nH: hide dominant hue\nC: choose a new file\n"
			+ "W: save file\nSPACE: reset image";

	static final int FILTER_HEIGHT = 2;
	static final int FILTER_INCREMENT = 5;
	static final int HUE_INCREMENT = 2;
	static final int HUE_RANGE = 100; 
	static final int IMAGE_MAX = 640;
	static final int RGB_COLOR_RANGE = 100;
	static final int SIDE_BAR_PADDING = 10;
	static final int SIDE_BAR_WIDTH = RGB_COLOR_RANGE + 2 * SIDE_BAR_PADDING + 50;
	
	private ImageState imageState;

	public void setup() {
		noLoop();
		imageState = new ImageState(new ColorHelper(new PixelColorHelper()));

		// Set up the view.
		size(IMAGE_MAX + SIDE_BAR_WIDTH, IMAGE_MAX);
		background(0);

		chooseFile();
	}
	
	public void draw() {
		background(0);

		colorMode(RGB, RGB_COLOR_RANGE);
		stroke(RGB_COLOR_RANGE);
		line(IMAGE_MAX, 0, IMAGE_MAX, IMAGE_MAX);

		// Draw red line
		int x = IMAGE_MAX + SIDE_BAR_PADDING;
		int y = 2 * SIDE_BAR_PADDING;
		stroke(RGB_COLOR_RANGE, 0, 0);
		line(x, y, x + RGB_COLOR_RANGE, y);
		line(x + imageState.redFilter(), y - FILTER_HEIGHT,
				x + imageState.redFilter(), y + FILTER_HEIGHT);

		// Draw green line
		y += 2 * SIDE_BAR_PADDING;
		stroke(0, RGB_COLOR_RANGE, 0);
		line(x, y, x + RGB_COLOR_RANGE, y);
		line(x + imageState.greenFilter(), y - FILTER_HEIGHT,
				x + imageState.greenFilter(), y + FILTER_HEIGHT);

		// Draw blue line
		y += 2 * SIDE_BAR_PADDING;
		stroke(0, 0, RGB_COLOR_RANGE);
		line(x, y, x + RGB_COLOR_RANGE, y);
		line(x + imageState.blueFilter(), y - FILTER_HEIGHT,
				x + imageState.blueFilter(), y + FILTER_HEIGHT);
		
		// Draw white line.
		y += 2 * SIDE_BAR_PADDING;
		stroke(HUE_RANGE);
		line(x, y, x + 100, y);
		line(x + imageState.hueTolerance(), y - FILTER_HEIGHT,
				x + imageState.hueTolerance(), y + FILTER_HEIGHT);

		y += 4 * SIDE_BAR_PADDING;
		text(INSTRUCTIONS, x, y);

		// Draw image.
		if (imageState.image() != null) {
			drawImage();
		}
		updatePixels();
	}
	
	// Callback for selectInput(), has to be public to be found.
	public void fileSelected(File file) {
		if (file == null) {
			println("User hit cancel.");
		} else {
			imageState.setFilepath(file.getAbsolutePath());
			imageState.setUpImage(this, IMAGE_MAX);
			redraw();
		}
	}
	
	private void drawImage() {
		imageMode(CENTER);
		imageState.updateImage(this, HUE_RANGE, RGB_COLOR_RANGE);
		image(imageState.image().image(), IMAGE_MAX/2, IMAGE_MAX/2, imageState.image().getWidth(),
				imageState.image().getHeight());
	}
	
	public void keyPressed() {
		switch(key) {
		case 'c':
			chooseFile();
			break;
		case 'w':
			imageState.image().save(imageState.filepath() + "-new.png");
			break;
		case ' ':
			imageState.resetImage(this, IMAGE_MAX);
			break;
		}
		imageState.processKeyPress(key, FILTER_INCREMENT, RGB_COLOR_RANGE, HUE_INCREMENT, HUE_RANGE);
		redraw();
	}
	
	private void chooseFile() {
		// Choose the file.
		selectInput("Select a file to process:", "fileSelected");
	}
}
