package com.catehuston.imagefilter.model;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

import processing.core.PApplet;

import com.catehuston.imagefilter.color.ColorHelper;
import com.catehuston.imagefilter.model.ImageState.ColorMode;

@RunWith(MockitoJUnitRunner.class)
public class ImageStateTest {

	@Mock PApplet applet;
	@Mock ColorHelper colorHelper;
	@Mock IFAImage image;

	private ImageState imageState;

	@Before public void setUp() throws Exception {
		imageState = new ImageState(colorHelper);
	}

	@Test public void testUpdateImageDominantHueHidden() {
		imageState.setFilepath("filepath");
		imageState.set(image, ColorMode.HIDE_DOMINANT_HUE, 5, 10, 15, 10);

		imageState.updateImage(applet, 100, 100);

		verify(image).update(applet, "filepath");
		verify(colorHelper).processImageForHue(applet, image, 100, 10, false);
		verify(colorHelper).applyColorFilter(applet, image, 5, 10, 15, 100);
		verify(image).updatePixels();
	}

	@Test public void testUpdateDominantHueShowing() {
		imageState.setFilepath("filepath");
		imageState.set(image, ColorMode.SHOW_DOMINANT_HUE, 5, 10, 15, 10);

		imageState.updateImage(applet, 100, 100);

		verify(image).update(applet, "filepath");
		verify(colorHelper).processImageForHue(applet, image, 100, 10, true);
		verify(colorHelper).applyColorFilter(applet, image, 5, 10, 15, 100);
		verify(image).updatePixels();
	}

	@Test public void testUpdateRGBOnly() {
		imageState.setFilepath("filepath");
		imageState.set(image, ColorMode.COLOR_FILTER, 5, 10, 15, 10);

		imageState.updateImage(applet, 100, 100);

		verify(image).update(applet, "filepath");
		verify(colorHelper, never()).processImageForHue(any(PApplet.class), any(IFAImage.class),
				anyInt(), anyInt(), anyBoolean());
		verify(colorHelper).applyColorFilter(applet, image, 5, 10, 15, 100);
		verify(image).updatePixels();
	}

	@Test public void testKeyPress() {
		imageState.processKeyPress('r', 5, 100, 2, 200);
		assertState(ColorMode.COLOR_FILTER, 5, 0, 0);

		imageState.processKeyPress('e', 5, 100, 2, 200);
		assertState(ColorMode.COLOR_FILTER, 0, 0, 0);

		imageState.processKeyPress('g', 5, 100, 2, 200);
		assertState(ColorMode.COLOR_FILTER, 0, 5, 0);

		imageState.processKeyPress('f', 5, 100, 2, 200);
		assertState(ColorMode.COLOR_FILTER, 0, 0, 0);

		imageState.processKeyPress('b', 5, 100, 2, 200);
		assertState(ColorMode.COLOR_FILTER, 0, 0, 5);

		imageState.processKeyPress('v', 5, 100, 2, 200);
		assertState(ColorMode.COLOR_FILTER, 0, 0, 0);

		imageState.processKeyPress('h', 5, 100, 2, 200);
		assertState(ColorMode.HIDE_DOMINANT_HUE, 0, 0, 0);

		imageState.processKeyPress('s', 5, 100, 2, 200);
		assertState(ColorMode.SHOW_DOMINANT_HUE, 0, 0, 0);
	}

	private void assertState(ColorMode colorMode, int redFilter,
			int greenFilter, int blueFilter) {
		assertEquals(colorMode, imageState.getColorMode());
		assertEquals(redFilter, imageState.redFilter());
		assertEquals(greenFilter, imageState.greenFilter());
		assertEquals(blueFilter, imageState.blueFilter());
	}

	@Test public void testSetupImage() {
		imageState.set(image, ColorMode.SHOW_DOMINANT_HUE, 5, 10, 15, 10);
		when(image.getWidth()).thenReturn(20);
		when(image.getHeight()).thenReturn(8);
		imageState.setUpImage(applet, 10);
		verify(image).update(applet, null);
		verify(image).resize(10, 4);
	}

	@Test public void testResetImage() {
		imageState.set(image, ColorMode.SHOW_DOMINANT_HUE, 5, 10, 15, 10);
		imageState.resetImage(applet, 10);
		assertState(ColorMode.COLOR_FILTER, 0, 0, 0);
	}
}
