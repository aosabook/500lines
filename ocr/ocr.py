import csv
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import numpy as np
from numpy import matrix
from math import pow
import math
import random

# Shuffle the indices of the 5000 samples and use the first 3500 for
# training and the rest for testing.
sampleIndices = list(range(5000))
random.shuffle(sampleIndices)

learningRate = 0.1

# Load data samples and labels into matrix
dataMatrix = np.loadtxt(open('data.csv', 'rb'), delimiter = ',')
dataLabels = np.loadtxt(open('dataLabels.csv', 'rb'))

# Convert from numpy ndarrays to python lists
dataMatrix = dataMatrix.tolist()
dataLabels = dataLabels.tolist()

def randInitializeWeights(sizeIn, sizeOut):
	return [x * 0.12 - 0.06 for x in np.random.rand(sizeOut, sizeIn)]

# The sigmoid activation function
def sigmoid(z):
	return 1 / (1 + math.e ** -z)

def sigmoidPrime(z):
	return sigmoid(z) * (1 - sigmoid(z))

def train(theta1, theta2, b1, b2):
	for i in sampleIndices[:3500]:
		y0 = dataMatrix[i]
		label = int(dataLabels[i])

		# Step 2: Forward propagation
		y1 = np.dot(np.mat(theta1), np.mat(y0).T)
		sum1 =  y1 + np.mat(b1) # Add the bias
		y1 = sigmoid(sum1)

		y2 = np.dot(np.array(theta2), y1)
		y2 = np.add(y2, b2) # Add the bias
		y2 = sigmoid(y2)

		# Step 3: Back propagation
		actualVals = [0] * 10
		actualVals[label] = 1
		outputErrors = np.mat(actualVals).T - np.mat(y2)
		hiddenErrors = np.multiply(np.dot(np.mat(theta2).T, outputErrors), sigmoidPrime(sum1))

		# Step 4: Update weights
		theta1 += learningRate * np.dot(np.mat(hiddenErrors), np.mat(y0))
		theta2 += learningRate * np.dot(np.mat(outputErrors), np.mat(y1).T)
		b2 += learningRate * outputErrors
		b1 += learningRate * hiddenErrors

	return (theta1, theta2, b1, b2)

def test(theta1, theta2, b1, b2):
	avgSum = 0
	for j in range(100):
		correctGuessCount = 0
		for i in sampleIndices[3500:]:
			test = dataMatrix[i]
			y1 = np.dot(np.mat(theta1), np.mat(test).T)
			y1 =  y1 + np.mat(b1) # Add the bias
			y1 = sigmoid(y1)

			y2 = np.dot(np.array(theta2), y1)
			y2 = np.add(y2, b2) # Add the bias
			y2 = sigmoid(y2)

			results = y2.T.tolist()[0]
			if dataLabels[i] == results.index(max(results)):
				correctGuessCount += 1

		avgSum += (correctGuessCount / float(1500))
	print avgSum / float(100)


if __name__ == "__main__":
	sigmoid = np.vectorize(sigmoid)
	sigmoidPrime = np.vectorize(sigmoidPrime)

	for j in range(5, 100, 5):		
		# Step 1: Initialize weights to small numbers
		theta1 = randInitializeWeights(400, j)
		theta2 = randInitializeWeights(j, 10)
		b1 = randInitializeWeights(1, j)
		b2 = randInitializeWeights(1, 10)

		theta1, theta2, b1, b2 = train(theta1, theta2, b1, b2)
		test(theta1, theta2, b1, b2)
