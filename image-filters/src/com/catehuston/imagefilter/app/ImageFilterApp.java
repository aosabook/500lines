package com.catehuston.imagefilter.app;

import java.io.File;

import processing.core.PApplet;

import com.catehuston.imagefilter.color.ColorHelper;
import com.catehuston.imagefilter.color.PixelColorHelper;
import com.catehuston.imagefilter.model.ImageState;


@SuppressWarnings("serial")
public class ImageFilterApp extends PApplet {
	
	static final String instructions = "R: increase red filter\nE: reduce red filter\n"
			+ "G: increase green filter\nF: reduce green filter\nB: increase blue filter\n"
			+ "V: reduce blue filter\nI: increase hue tolerance\nU: reduce hue tolerance\n"
			+ "C: choose a new file\nW: save file\nSPACE: reset image";

	static final int filterHeight = 2;
	static final int filterIncrement = 5;
	static final int hueIncrement = 2;
	static final int hueRange = 100; 
	static final int imageMax = 640;
	static final int rgbColorRange = 100;
	static final int sideBarPadding = 10;
	static final int sideBarWidth = rgbColorRange + 2 * sideBarPadding + 50;
	
	private ImageState imageState;

	public void setup() {
		noLoop();
		imageState = new ImageState(new ColorHelper(new PixelColorHelper()));

		// Set up the view.
		size(imageMax + sideBarWidth, imageMax);
		background(0);

		chooseFile();
	}
	
	public void draw() {
		background(0);

		colorMode(RGB, rgbColorRange);
		stroke(rgbColorRange);
		line(imageMax, 0, imageMax, imageMax);

		// Draw red line
		int x = imageMax + sideBarPadding;
		int y = 2 * sideBarPadding;
		stroke(rgbColorRange, 0, 0);
		line(x, y, x + rgbColorRange, y);
		line(x + imageState.redFilter(), y - filterHeight,
				x + imageState.redFilter(), y + filterHeight);

		// Draw green line
		y += 2 * sideBarPadding;
		stroke(0, rgbColorRange, 0);
		line(x, y, x + rgbColorRange, y);
		line(x + imageState.greenFilter(), y - filterHeight,
				x + imageState.greenFilter(), y + filterHeight);

		// Draw blue line
		y += 2 * sideBarPadding;
		stroke(0, 0, rgbColorRange);
		line(x, y, x + rgbColorRange, y);
		line(x + imageState.blueFilter(), y - filterHeight,
				x + imageState.blueFilter(), y + filterHeight);
		
		// Draw white line.
		y += 2 * sideBarPadding;
		stroke(hueRange);
		line(x, y, x + 100, y);
		line(x + imageState.hueTolerance(), y - filterHeight,
				x + imageState.hueTolerance(), y + filterHeight);

		y += 4 * sideBarPadding;
		text(instructions, x, y);

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
			imageState.setUpImage(this, imageMax);
			redraw();
		}
	}
	
	private void drawImage() {
		imageMode(CENTER);
		imageState.updateImage(this, hueRange, rgbColorRange);;
		image(imageState.image().image(), imageMax/2, imageMax/2, imageState.image().getWidth(),
				imageState.image().getHeight());
	}
	
	public void keyPressed() {
		if (key == 'c') {
			chooseFile();
		} else if (key == 'w') {
			imageState.image().save(imageState.filepath() + "-new.png");
		} else if (key == ' ') {
			imageState.resetImage(this, imageMax);
		}
		 imageState.processKeyPress(key, filterIncrement, rgbColorRange, hueIncrement, hueRange);
		 redraw();
	}
	
	private void chooseFile() {
		// Choose the file.
		selectInput("Select a file to process:", "fileSelected");
	}
}
