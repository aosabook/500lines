"""
In order to decide how many hidden nodes the hidden layer should have,
split up the data set into training and testing data and create networks
with various hidden node counts (5, 10, 15, ... 45), testing the performance
for each.

The best-performing node count is used in the actual system. If multiple counts
perform similarly, choose the smallest count for a smaller network with fewer computations.
"""

import numpy as np
from ocr import ocrNeuralNetwork
from sklearn.cross_validation import train_test_split

def test(dataMatrix, dataLabels, testIndices, nn):
    avgSum = 0
    for j in range(100):
        correctGuessCount = 0
        for i in testIndices:
            test = dataMatrix[i]
            prediction = nn.predict(test)
            if dataLabels[i] == prediction:
                correctGuessCount += 1

        avgSum += (correctGuessCount / float(len(testIndices)))
    return avgSum / float(100)


# Load data samples and labels into matrix
dataMatrix = np.loadtxt(open('data.csv', 'rb'), delimiter = ',').tolist()
dataLabels = np.loadtxt(open('dataLabels.csv', 'rb')).tolist()

# Create training and testing sets.
trainIndices, testIndices = train_test_split(list(range(5000)))

print "PERFORMANCE"
print "-----------"

# Try various number of hidden nodes and see what performs best
for i in xrange(5, 50, 5):
	nn = ocrNeuralNetwork(i, dataMatrix, dataLabels, trainIndices, False)
	print str(i) + " Hidden Nodes: " + str(test(dataMatrix, dataLabels, testIndices, nn))