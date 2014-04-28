import BaseHTTPServer
import json
from ocr import ocrNeuralNetwork
import numpy as np

HOST_NAME = 'localhost'
PORT_NUMBER = 8000

# Load data samples and labels into matrix
dataMatrix = np.loadtxt(open('data.csv', 'rb'), delimiter = ',')
dataLabels = np.loadtxt(open('dataLabels.csv', 'rb'))

# Convert from numpy ndarrays to python lists
dataMatrix = dataMatrix.tolist()
dataLabels = dataLabels.tolist()

# If a neural network file does not exist, train it using all 5000 existing data samples.
nn = ocrNeuralNetwork(15, dataMatrix, dataLabels, list(range(5000)));

class JSONHandler(BaseHTTPServer.BaseHTTPRequestHandler):
    def do_POST(s):
        responseCode = 200
        response = ""
        varLen = int(s.headers.get('Content-Length'))
        content = s.rfile.read(varLen);
        payload = json.loads(content);

        if payload.get('train'):
            nn.train(payload['trainArray'])
            nn.save()
        elif payload.get('predict'):
            response = {"type":"test", "result":nn.predict(str(payload['image']))}
        else:
            responseCode = 400

        s.send_response(responseCode)
        s.send_header("Content-type", "application/json")
        s.send_header("Access-Control-Allow-Origin", "*")
        s.end_headers()
        if response:
            s.wfile.write(json.dumps(response))
        return

if __name__ == '__main__':
    server_class = BaseHTTPServer.HTTPServer;
    httpd = server_class((HOST_NAME, PORT_NUMBER), JSONHandler)

    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        pass
    else:
        print "Unexpected server exception occurred."
    finally:
        httpd.server_close()
