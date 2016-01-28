package com.catehuston.imagefilter.color;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

import processing.core.PApplet;

import com.catehuston.imagefilter.model.HSBColor;
import com.catehuston.imagefilter.model.IFAImage;

@RunWith(MockitoJUnitRunner.class)
public class ColorHelperTest {

	@Mock PApplet applet;
	@Mock IFAImage image;
	@Mock PixelColorHelper pixelColorHelper;

	ColorHelper colorHelper;

	private static final int px1 = 1000;
	private static final int px2 = 1010;
	private static final int px3 = 1030;
	private static final int px4 = 1040;
	private static final int px5 = 1050;
	private static final int[] pixels = { px1, px2, px3, px4, px5 };

	@Before public void setUp() throws Exception {
		colorHelper = new ColorHelper(pixelColorHelper);
		when(image.getPixels()).thenReturn(pixels);
		setHsbValuesForPixel(0, px1, 30F, 5F, 10F);
		setHsbValuesForPixel(1, px2, 20F, 6F, 11F);
		setHsbValuesForPixel(2, px3, 30F, 7F, 12F);
		setHsbValuesForPixel(3, px4, 50F, 8F, 13F);
		setHsbValuesForPixel(4, px5, 30F, 9F, 14F);
	}

	private void setHsbValuesForPixel(int px, int color, float h, float s, float b) {
		when(image.getPixel(px)).thenReturn(color);
		when(pixelColorHelper.hue(applet, color)).thenReturn(h);
		when(pixelColorHelper.saturation(applet, color)).thenReturn(s);
		when(pixelColorHelper.brightness(applet, color)).thenReturn(b);
	}

	private void setRgbValuesForPixel(int px, int color, float r, float g, float b, float alpha) {
		when(image.getPixel(px)).thenReturn(color);
		when(pixelColorHelper.red(applet, color)).thenReturn(r);
		when(pixelColorHelper.green(applet, color)).thenReturn(g);
		when(pixelColorHelper.blue(applet, color)).thenReturn(b);
		when(pixelColorHelper.alpha(applet, color)).thenReturn(alpha);
	}

	@Test public void testHueInRange() {
		// In range.
		assertTrue(colorHelper.hueInRange(10, 20, 5, 15));
		// Lowest end of range.
		assertTrue(colorHelper.hueInRange(6, 20, 5, 15));
		// Highest end of range.
		assertTrue(colorHelper.hueInRange(14, 20, 5, 15));
		// In range, looping over.
		assertTrue(colorHelper.hueInRange(4, 20, 10, 25));
		// In range, looping under.
		assertTrue(colorHelper.hueInRange(4, 20, -5, 5));
		// Out of range - below.
		assertFalse(colorHelper.hueInRange(4, 20, 5, 15));
		// Out of range - above.
		assertFalse(colorHelper.hueInRange(16, 20, 5, 15));
		// Out of range, looping over.
		assertFalse(colorHelper.hueInRange(10, 20, 15, 25));
		// Out of range, looping under.
		assertFalse(colorHelper.hueInRange(6, 20, -5, 5));
	}

	@Test public void testHsbColorFromImage() {
		HSBColor color = colorHelper.getDominantHue(applet, image, 100);
		verify(image).loadPixels();

		// Check results.
		assertEquals(30F, color.h, 0);
		assertEquals(7F, color.s, 0);
		assertEquals(12F, color.b, 0);
	}

	@Test public void testProcessImageNoHue() {
		when(pixelColorHelper.color(applet, 11F)).thenReturn(11);
		when(pixelColorHelper.color(applet, 13F)).thenReturn(13);
		colorHelper.processImageForHue(applet, image, 60, 2, false);
		verify(applet).colorMode(PApplet.HSB, 59);
		verify(image, times(2)).loadPixels();
		verify(image).setPixel(1, 11);
		verify(image).setPixel(3, 13);
	}

	@Test public void testProcessImageWithHue() {
		when(pixelColorHelper.color(applet, 10F)).thenReturn(10);
		when(pixelColorHelper.color(applet, 12F)).thenReturn(12);
		when(pixelColorHelper.color(applet, 14F)).thenReturn(14);
		colorHelper.processImageForHue(applet, image, 60, 2, true);
		verify(applet).colorMode(PApplet.HSB, 59);
		verify(image, times(2)).loadPixels();
		verify(image).setPixel(0, 10);
		verify(image).setPixel(2, 12);
		verify(image).setPixel(4, 14);
	}

	@Test public void testApplyColorFilter() {
		setRgbValuesForPixel(0, px1, 10F, 12F, 14F, 60F);
		setRgbValuesForPixel(1, px2, 20F, 22F, 24F, 70F);
		setRgbValuesForPixel(2, px3, 30F, 32F, 34F, 80F);
		setRgbValuesForPixel(3, px4, 40F, 42F, 44F, 90F);
		setRgbValuesForPixel(4, px5, 50F, 52F, 54F, 100F);

		when(pixelColorHelper.color(applet, 0F, 0F, 0F, 60F)).thenReturn(5);
		when(pixelColorHelper.color(applet, 20F, 0F, 0F, 70F)).thenReturn(15);
		when(pixelColorHelper.color(applet, 30F, 32F, 0F, 80F)).thenReturn(25);
		when(pixelColorHelper.color(applet, 40F, 42F, 44F, 90F)).thenReturn(35);
		when(pixelColorHelper.color(applet, 50F, 52F, 54F, 100F)).thenReturn(45);

		colorHelper.applyColorFilter(applet, image, 15, 25, 35, 100);
		verify(applet).colorMode(PApplet.RGB, 100);
		verify(image).loadPixels();

		verify(image).setPixel(0, 5);
		verify(image).setPixel(1, 15);
		verify(image).setPixel(2, 25);
		verify(image).setPixel(3, 35);
		verify(image).setPixel(4, 45);
	}
}
