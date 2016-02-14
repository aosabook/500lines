Authors: A. Jesse Jiryu Davis and Guido van Rossum
Project: Web crawler
Requirements:
  * Python 3.4+
  * aiohttp

This is a web crawler. You give it a URL and it will crawl that
website by following href links in the HTML pages.

It doesn't do anything with the crawled pages, and the algorithm for
finding links is intentionally naive -- those parts are easily
modified, and not of particular interest (just use your favorite HTML
parser instead of a regular expression).

The point of the example is to show off how to write a reasonably complex HTTP
client application using the asyncio module. This module, originally nicknamed
Tulip, is new in the Python 3.4 standard library, based on PEP 3156. The
example uses an HTTP client implementation for asyncio called "aiohttp", by
Andrew Svetlov, Nikolay Kim, and others.

Install the crawler's requirements like:

    python3 -m pip install -r requirements.txt

In order to be fast and efficient, the program opens multiple parallel
connections to the server and reuses connections for multiple
requests.

On Jesse's Macbook Pro (2.3GHz Intel Core i7), with a fast network
connection, it can visit all HTML pages of xkcd.com (over 1500 at the
time of writing, and growing at a rate of three per week) in under 3
seconds.  It can also scan all public pages on dropbox.com (about 2500
URLs) in under 50 seconds.

Example command line (the -q reduces log output):

    python3 crawl.py -q xkcd.com

Use --help to see all options.

See also the files in "supplemental".
