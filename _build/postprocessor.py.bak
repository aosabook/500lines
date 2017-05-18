import re

def fix_latex_macros(text):
    '''Use the AOSA style macros for headers, etc.'''
    rs = (
        (r'\\section\{', r'\\aosasecti{'),
        (r'\\subsection\{', r'\\aosasectii{'),
        (r'\\subsubsection\{', r'\\aosasectiii{'),
        (r'\\begin\{itemize\}', r'\\begin{aosaitemize}'),
        (r'\\end\{itemize\}', r'\\end{aosaitemize}'),
        (r'\\begin\{enumerate\}', r'\\begin{aosaenumerate}'),
        (r'\\end\{enumerate\}', r'\\end{aosaenumerate}'),
        (r'\\begin\{description\}', r'\\begin{aosadescription}'),
        (r'\\end\{description\}', r'\\end{aosadescription}'),
        (r'\\itemsep1pt\\parskip0pt\\parsep0pt', ''),
        )
    for (old, new) in rs:
        text = re.sub(old, new, text)
    return text

def test_fix_latex_macros():
    test = r'''
burble burble burble

section this is a section or a subsection{

\begin{itemize}

\begin{enumerate}

\section{This is a header First}

\subsection{This is a header First}

\subsubsection{This is a header}

burble burble burble
'''
    expect = r'''
burble burble burble

section this is a section or a subsection{

\begin{aosaitemize}

\begin{aosaenumerate}

\aosasecti{This is a header First}

\aosasectii{This is a header First}

\aosasectiii{This is a header}

burble burble burble
'''
    result = fix_latex_macros(test)
    print result
    print expect
    assert (result == expect)

if __name__ == '__main__':
    import sys
    import argparse
    parser = argparse.ArgumentParser(description="Fix output for latex")
    parser.add_argument('doc', nargs='*')
    parser.add_argument('--output', dest='output', required=True)
    args = parser.parse_args()
    destination_file = open(args.output, 'w')
    if len(args.doc) > 0:
        input_file = open(args.doc[0])
    else:
        input_file = sys.stdin
    input_document = input_file.read()
    input_document = fix_latex_macros(input_document)
    out = input_document
    destination_file.write(out)
