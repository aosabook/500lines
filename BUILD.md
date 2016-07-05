# Building 500 Lines or Less

500 Lines or Less has been successfully built on several Linux systems, as well
as OSX 10.10+. Almost all of the software mentioned in the below instructions
should be available via your favorite package manager, with the possible
exception of LaTeX on OS X (common practice seems to be installing
[MacTex](https://tug.org/mactex/) via the pkg installer.)

## Base instructions:
- Install [Python 2.7](https://www.python.org/downloads/). 
- You may want to use [virtualenv](https://virtualenv.pypa.io/en/stable/) to
  isolate your Python build environment for 500Lines.
  [pyenv](https://github.com/yyuu/pyenv) is a simple way to manage this:
   - `cd 500lines`
   - `pyenv virtualenv 500l`
   - `pyenv local 500l`
- `pip install -r requirements.txt`. If you haven't used virtualenv, you may
  need to `sudo pip install -r requirements.txt`.
- Install [pandoc](http://pandoc.org/) and
  [pandoc-citeproc](https://github.com/jgm/pandoc-citeproc). These should both
  be available via your favorite package manager.

## Building the PDF:
- Install a LaTeX distribution. 
- `python build.py --pdf`
- Output will be in `output/500L.pdf`

## Building the HTML:
- `python build.py --html`
- `cd html`
- `make serve`

## Building the ePub:
Warning: the ePub version of 500Lines is an "unsupported product"; the print
and code formatting is poor, raw LaTeX is displayed in several places, the code
is difficult to read, and images have not been optimized for the format.
However, if you really want 500Lines in your favorite reader, this is one way
to do it.

- `python build.py --epub --pandoc-epub`
- Output will be in `output/500L.epub`
