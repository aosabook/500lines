# testcgi.py : simple CGI script.

import time

print "Content-type: text/html"
print
print "<html>"
print "<body>"
print "<h1>%s</h1>" % time.asctime()
print "</body>"
print "</html>"
