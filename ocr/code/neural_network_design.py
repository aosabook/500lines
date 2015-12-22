"""
In order to decide how many hidden nodes the hidden layer should have,
split up the data set into training and testing data and create networks
with various hidden node counts (5, 10, 15, ... 45), testing the performance
for each.

The best-performing node count is used in the actual system. If multiple counts
perform similarly, choose the smallest count for a smaller network with fewer computations.
"""

import numpy as np
from ocr import OCRNeuralNetwork
from sklearn.cross_validation import train_test_split

def test(data_matrix, data_labels, test_indices, nn):
    avg_sum = 0
    for j in xrange(100):
        correct_guess_count = 0
        for i in test_indices:
            test = data_matrix[i]
            prediction = nn.predict(test)
            if data_labels[i] == prediction:
                correct_guess_count += 1

        avg_sum += (correct_guess_count / float(len(test_indices)))
    return avg_sum / 100


# Load data samples and labels into matrix
data_matrix = np.loadtxt(open('data.csv', 'rb'), delimiter = ',').tolist()
data_labels = np.loadtxt(open('dataLabels.csv', 'rb')).tolist()

# Create training and testing sets.
train_indices, test_indices = train_test_split(list(range(5000)))

print "PERFORMANCE"
print "-----------"

# Try various number of hidden nodes and see what performs best
for i in xrange(5, 50, 5):
    nn = OCRNeuralNetwork(i, data_matrix, data_labels, train_indices, False)
    performance = str(test(data_matrix, data_labels, test_indices, nn))
    print "{i} Hidden Nodes: {val}".format(i=i, val=performance)