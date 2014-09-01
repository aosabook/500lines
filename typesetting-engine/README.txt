Author: Jill-Jênn Vie
Project: Typesetting Engine
Requirements: Python 3.3

This is a simple typesetting engine. It converts Markdown text into PostScript
slides while determining the best way to break paragraphs into lines (achieving
the minimum raggedness).

It follows the algorithm described in the article: “Breaking Paragraphs into
Lines” (Knuth & Plass, 1981).

An example of DAG constructed by the algorithm: http://i.imgur.com/1lznGbz.png

Usage:

    python3 breakpoints.py < slides.md
    dot -Tpng knuth.dot -o knuth.png
