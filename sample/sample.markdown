title: A Sample Chapter
author: Michael DiBernardo

## Introduction

A lot of people like to start chapters with a quote like this, or something
similarly inspiring.

> "Good chapters will make judicious use of formatting..."
> Anonymous

There are a lot of quotes that can be used for inspiration. [This
page](http://www.brainyquote.com/quotes/keywords/computer_science.html) has
quite a few.

## Important

Images usually make something look more important.

\aosafigure[240pt]{sample-images/image.png}{Extreme Gravity}{500l.sample.image}

It's especially useful if you can refer to images like \aosafigref{500l.sample.image} later in the text.[^invented]

<markdown>
[^invented]: Hi, I'm not a PDF.
</markdown>
<latex>
[^invented]: You can learn more about this in \aosachapref{s:template-engine}.
</latex>


## Data stuff

We can render a lot of useful stuff in tables like \aosatblref{500l.tbl.tablesample}.

<markdown>
|Delay         | User Reaction |
|:-------------|:--------------|
| 0 - 100 ms    |  Instant |
| 100 - 300 ms  |  Small perceptible delay |
| 300 - 1000 ms |  Machine is working |
| 1 s+          |  Mental context switch |
| 10 s+         |  I'll come back later... |

: \label{500l.tbl.tablesample} User perception of latency
</markdown>
<latex>
\begin{table}
\centering
{\footnotesize
\rowcolors{2}{TableOdd}{TableEven}
\begin{tabular}{rl}
\hline
\textbf{Delay}
& \textbf{User Reaction}
\\
\hline
0--100 ms
& Instant
\\
100--300 ms
& Small perceptible delay
\\
300--1000 ms
& Machine is working
\\
1 s+
& Mental context switch
\\
10 s+
& I'll come back later{\ldots}
\\
\hline
\end{tabular}
}
\caption{User Perception of Latency}
\label{500l.tbl.tablesample}
\end{table}
</latex>

Sometimes things are difficult to put in the PDF, but are fine to put into HTML. We can use `markdown` or `latex` tags for these cases, which let us choose what to show in each context. An example of this might be a long link, like a reference to <markdown>[part of the HTML5 spec](http://www.whatwg.org/specs/web-apps/current-work/multipage/links.html#link-type-prefetch)</markdown><latex>part of the HTML5 spec \footnote{\url{http://www.whatwg.org/specs/web-apps/current-work/multipage/links.html#link-type-prefetch}} </latex>.

You can use LaTeX dollar-style rendering inline in the source markdown and it will "just work" in the PDF, such as with this relation taken from POSA: $S(N) = \frac{1}{(1 - P) + \frac{P}{N}}$. The HTML version plays some tricks with MathJax to achieve the same thing. 

A lot of authors will cite their references via footnotes. However, if you'd rather use normal bibtex-style citations, you can use normal LaTeX citation formatting and this will reference the resources in the final bibliography \cite{Schmidt:02g}.

<markdown>
## References

</markdown>

