Time to start digging down to the socket and parsing layers.  This
"version" shows how socket connections are handled and how HTTP
requests are parsed.

To run:

*   `python echo-server.py 8080` in one window, `python echo-client.py 127.0.0.1 8080 hello` in another.
*   `python telnet-server.py 8080` in one window, `python telnet-client.py 8080 < test.txt` in another.
