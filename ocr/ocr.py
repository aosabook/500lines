import csv
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import numpy as np
from numpy import matrix
from math import pow
import math
import random
import os.path
import json

"""
This class does some initial training of a neural network for predicting drawn
digits based on a data set in dataMatrix and dataLabels. It can then be used to
train the network further by calling train() with any array of data or to predict
what a drawn digit is by calling predict().

The weights that define the neural network can be saved to a file, NN_FILE_PATH,
to be reloaded upon initilization.
"""
class ocrNeuralNetwork:
    # Shuffle the indices of the 5000 samples and use the first 3500 for
    # training and the rest for testing.
    sampleIndices = list(range(5000))
    random.shuffle(sampleIndices)

    LEARNING_RATE = 0.1
    WIDTH_IN_PIXELS = 20
    NN_FILE_PATH = 'nn.json'

    def __init__(self, numHiddenNodes, dataMatrix, dataLabels):
        self.sigmoid = np.vectorize(self._sigmoidScalar)
        self.sigmoidPrime = np.vectorize(self._sigmoidPrimeScalar)
        self.dataMatrix = dataMatrix
        self.dataLabels = dataLabels

        if (not os.path.isfile(ocrNeuralNetwork.NN_FILE_PATH)):
            # Step 1: Initialize weights to small numbers
            self.theta1 = self._randInitializeWeights(400, numHiddenNodes)
            self.theta2 = self._randInitializeWeights(numHiddenNodes, 10)
            self.input_layer_bias = self._randInitializeWeights(1, numHiddenNodes)
            self.hidden_layer_bias = self._randInitializeWeights(1, 10)

            # Train using sample data
            self.train([{"y0":self.dataMatrix[i], "label":int(ocrNeuralNetwork.dataLabels[i])} for i in ocrNeuralNetwork.sampleIndices[:3500]])
            self.save()
        else:
            self._load()
        #self._test()

    def _randInitializeWeights(self, sizeIn, sizeOut):
        return [((x * 0.12) - 0.06) for x in np.random.rand(sizeOut, sizeIn)]

    # The sigmoid activation function. Operates on scalars.
    def _sigmoidScalar(self, z):
        return 1 / (1 + math.e ** -z)

    def _sigmoidPrimeScalar(self, z):
        return self.sigmoid(z) * (1 - self.sigmoid(z))

    def _draw(self, sample):
        pixelArray = [sample[j:j+WIDTH_IN_PIXELS] for j in xrange(0, len(sample), WIDTH_IN_PIXELS)]
        plt.imshow(zip(*pixelArray), cmap = cm.Greys_r, interpolation="nearest")
        plt.show()

    def train(self, trainingDataArray):
        for data in trainingDataArray:
            # Step 2: Forward propagation
            y1 = np.dot(np.mat(self.theta1), np.mat(data['y0']).T)
            sum1 =  y1 + np.mat(self.input_layer_bias) # Add the bias
            y1 = self.sigmoid(sum1)

            y2 = np.dot(np.array(self.theta2), y1)
            y2 = np.add(y2, self.hidden_layer_bias) # Add the bias
            y2 = self.sigmoid(y2)

            # Step 3: Back propagation
            actualVals = [0] * 10
            actualVals[data['label']] = 1
            outputErrors = np.mat(actualVals).T - np.mat(y2)
            hiddenErrors = np.multiply(np.dot(np.mat(self.theta2).T, outputErrors), self.sigmoidPrime(sum1))

            # Step 4: Update weights
            self.theta1 += ocrNeuralNetwork.LEARNING_RATE * np.dot(np.mat(hiddenErrors), np.mat(data['y0']))
            self.theta2 += ocrNeuralNetwork.LEARNING_RATE * np.dot(np.mat(outputErrors), np.mat(y1).T)
            self.hidden_layer_bias += ocrNeuralNetwork.LEARNING_RATE * outputErrors
            self.input_layer_bias += ocrNeuralNetwork.LEARNING_RATE * hiddenErrors

    def _test(self):
        avgSum = 0
        for j in range(100):
            correctGuessCount = 0
            for i in ocrNeuralNetwork.sampleIndices[3500:]:
                test = ocrNeuralNetwork.dataMatrix[i]
                prediction = self.predict(test)
                if ocrNeuralNetwork.dataLabels[i] == prediction:
                    correctGuessCount += 1

            avgSum += (correctGuessCount / float(1500))
        print avgSum / float(100)

    def predict(self, test):
        y1 = np.dot(np.mat(self.theta1), np.mat(test).T)
        y1 =  y1 + np.mat(self.input_layer_bias) # Add the bias
        y1 = self.sigmoid(y1)

        y2 = np.dot(np.array(self.theta2), y1)
        y2 = np.add(y2, self.hidden_layer_bias) # Add the bias
        y2 = self.sigmoid(y2)

        results = y2.T.tolist()[0]
        return results.index(max(results))

    def _normalize(self, intensity, newMax, newMin):
        return intensity * (float(newMax - newMin) / float(255)) + newMin

    def save(self):
        nnFile = open(ocrNeuralNetwork.NN_FILE_PATH,'w');
        json.dump({
            "theta1":[npMat.tolist()[0] for npMat in self.theta1],
            "theta2":[npMat.tolist()[0] for npMat in self.theta2],
            "b1":self.input_layer_bias[0].tolist()[0],
            "b2":self.hidden_layer_bias[0].tolist()[0]
        }, nnFile)
        nnFile.close()

    def _load(self):
        nnFile = open(ocrNeuralNetwork.NN_FILE_PATH);
        nn = json.load(nnFile)
        self.theta1 = [np.array(li) for li in nn['theta1']]
        self.theta2 = [np.array(li) for li in nn['theta2']]
        self.input_layer_bias = [np.array(nn['b1'][0])]
        self.hidden_layer_bias = [np.array(nn['b2'][0])]
        nnFile.close()
