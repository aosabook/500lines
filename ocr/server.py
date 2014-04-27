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

nn = ocrNeuralNetwork(15, dataMatrix, dataLabels);

class JSONHandler(BaseHTTPServer.BaseHTTPRequestHandler):
    def do_POST(s):
        s.send_response(200)
        s.send_header("Content-type", "application/json")
        s.send_header("Access-Control-Allow-Origin", "*")
        s.end_headers()

        varLen = int(s.headers.get('Content-Length'))
        content = s.rfile.read(varLen);
        payload = json.loads(content);

        if payload['train']:
            nn.train(payload['trainArray'])
            nn.save()
        else:
            response = {"type":"test", "result":nn.predict(str(payload['image']))}
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
