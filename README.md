*500 Lines or Less*
===================

> "What I cannot create, I do not understand."
>
> -- Richard Feynman

This is the source for the book *500 Lines or Less*, the fourth in the
[Architecture of Open Source Applications](http://aosabook.org) series.  As
with other books in the series, all written material will be covered by the
Creative Commons - Attribution license, and all code by the MIT License: please
see the [license description](LICENSE.md) for details.  In addition, all
royalties from paid-for versions will all go to Amnesty International.

The production of this book has been made possible by the financial support of
[PagerDuty](http://www.pagerduty.com/company/work-with-us/).

<p align="center">
    <img src="https://github.com/aosabook/500lines/raw/master/resource/pagerduty_logo.png" alt="PagerDuty Logo" />
</p>


Mission
-------

Every architect studies family homes, apartments, schools, and other common
types of buildings during her training.  Equally, every programmer ought to
know how a compiler turns text into instructions, how a spreadsheet updates
cells, and how a database efficiently persists data.

Previous books in the AOSA series have done this by describing the high-level
architecture of several mature open-source projects. While the lessons learned
from those stories are valuable, they are sometimes difficult to absorb for
programmers who have not yet had to build anything at that scale.

"500 Lines or Less" focuses on the design decisions and tradeoffs that
experienced programmers make when they are writing code: 

*   Why divide the application into these particular modules with these
    particular interfaces?
*   Why use inheritance here and composition there?
*   How do we predict where our program might need to be extended, and how can
    we make that easy for other programmers?

Each chapter consists of a walkthrough of a program that solves a canonical
problem in software engineering in at most 500 source lines of code. We hope
that the material in this book will help readers understand the varied
approaches that engineers take when solving problems in different domains, and
will serve as a basis for projects that extend or modify the contributions
here.

Contributors
------------

<table>
  <tr>
    <th>Name</th>
    <th>Affiliation</th>
    <th>Project</th>
    <th>Online</th>
    <th>GitHub</th>
    <th>Email (if you choose)</th>
  </tr>
  <tr>
    <td>Mike DiBernardo</td>
    <td>freelance</td>
    <td>editorial</td>
    <td>
        <ul>
            <li><a href="https://twitter.com/mdibernardo">@mdibernardo</a></li>
            <li><a href="http://mikedebo.ca">mikedebo.ca</a></li>
        </ul>
    </td>
    <td><a href="https://github.com/MichaelDiBernardo">MichaelDiBernardo</a></td>
    <td>mikedebo@gmail.com</td>
  </tr>
   <tr>
    <td>Amy Brown</td>
    <td>indie</td>
    <td>editorial</td>
    <td><ul><li><a href="http://www.amyrbrown.ca/">amyrbrown.ca</a></li>
        <li><a href="http://www.twitter.com/amyrbrown">@amyrbrown</a></li></ul></td>
    <td><a href="https://github.com/amyrbrown">amyrbrown</a></td>
    <td>amy@amyrbrown.ca</td>
  </tr>
  <tr>
    <td>Dustin Mitchell</td>
    <td>Mozilla</td>
    <td>cluster</td>
    <td>&nbsp;</td>
    <td><a href="https://github.com/djmitche">djmitche</a></td>
    <td>dustin@mozila.com</td>
  </tr>
  <tr>
    <td>Audrey Tang</td>
    <td>g0v.tw, Socialtext, Apple</td>
    <td>spreadsheet</td>
    <td>
        <ul>
            <li><a href="https://twitter.com/audreyt">@audreyt</a></li>
        </ul>
    </td>
    <td><a href="https://github.com/audreyt">audreyt</a></td>
    <td>audreyt@audreyt.org</td>
  </tr>
  <tr>
    <td>Greg Wilson</td>
    <td>Mozilla</td>
    <td>web-server</td>
    <td>
        <ul>
            <li><a href="https://twitter.com/gvwilson">@gvwilson</a></li>
        </ul>
    </td>
    <td><a href="https://github.com/gvwilson">gvwilson</a></td>
    <td>gvwilson@third-bit.com</td>
  </tr>
  <tr>
    <td>Taavi Burns</td>
    <td>Previously at Points, now at PagerDuty</td>
    <td>data-store</td>
    <td>
        <ul>
            <li><a href="https://twitter.com/jaaaarel">@jaaaarel</a></li>
        </ul>
    </td>
    <td><a href="https://github.com/taavi">taavi</a></td>
    <td>taavi.burns@gmail.com</td>
  </tr>
  <tr>
    <td>Guido van Rossum</td>
    <td>Dropbox</td>
    <td>crawler</td>
    <td>
        <ul>
            <li><a href="https://twitter.com/gvanrossum">@gvanrossum</a></li>
        </ul>
    </td>
    <td><a href="https://github.com/gvanrossum">gvanrossum</a></td>
    <td>guido@python.org</td>
  </tr>
  <tr>
    <td>A. Jesse Jiryu Davis</td>
    <td>MongoDB</td>
    <td>crawler</td>
    <td>
        <ul>
            <li><a href="https://twitter.com/jessejiryudavis">@jessejiryudavis</a></li>
        </ul>
    </td>
    <td><a href="https://github.com/ajdavis">ajdavis</a></td>
    <td>jesse@emptysquare.net</td>
  </tr>
  <tr>
    <td>Erick Dransch</td>
    <td>Upverter</td>
    <td>Modeller</td>
    <td>
        <ul>
            <li><a href="https://twitter.com/ErickDransch">@ErickDransch</a></li>
        </ul>
    </td>
    <td><a href="https://github.com/EkkiD">EkkiD</a></td>
    <td>erick.dransch@upverter.com</td>
  </tr>
  <tr>
    <td>Ned Batchelder</td>
    <td>edX</td>
    <td>templating engine</td>
    <td>
        <ul>
            <li><a href="http://nedbatchelder.com">nedbatchelder.com</a></li>
            <li><a href="https://twitter.com/nedbat">@nedbat</a></li>
        </ul>
    </td>
    <td><a href="https://github.com/nedbat">nedbat</a></td>
    <td>ned@nedbatchelder.com</td>
  </tr>
  <tr>
    <td>Leah Hanson</td>
    <td>Google</td>
    <td>static analysis</td>
    <td>
        <ul>
            <li><a href="https://twitter.com/astrieanna">@astrieanna</a></li>
        </ul>
    </td>
    <td><a href="https://github.com/astrieanna">astrieanna</a></td>
    <td>leah.a.hanson@gmail.com</td>
  </tr>
  <tr>
    <td>Christian Muise</td>
    <td>University of Melbourne</td>
    <td>flow-shop</td>
    <td>
        <ul>
            <li><a href="https://twitter.com/cjmuise">@cjmuise</a></li>
        </ul>
    </td>
    <td><a href="https://github.com/haz">haz</a></td>
    <td>christian.muise@gmail.com</td>
  </tr>
  <tr>
    <td>Marina Samuel</td>
    <td>Mozilla</td>
    <td>ocr</td>
    <td>
        <ul>
            <li><a href="http://marinasamuel.com">www.marinasamuel.com</a></li>
            <li><a href="https://twitter.com/emtwos">@emtwos</a></li>
        </ul>
    </td>
    <td><a href="https://github.com/emtwo">emtwo</a></td>
    <td>msamuel@mozilla.com</td>
  </tr>
  <tr>
    <td>Cate Huston</td>
    <td></td>
    <td>Image Filter app</td>
    <td>
        <ul>
            <li><a href="http://www.accidentallyincode.com/">www.accidentallyincode.com/</a></li>
            <li><a href="https://twitter.com/catehstn">@catehstn</a></li>
        </ul>
    </td>
    <td><a href="https://github.com/catehstn">catehstn</a></td>
    <td>catehuston@gmail.com</td>
  </tr>
  <tr>
    <td>Yoav Rubin</td>
    <td>Microsoft</td>
    <td>In-memory functional database</td>
    <td>
        <ul>
            <li><a href="https://twitter.com/yoavrubin">@yoavrubin</a></li>
        </ul>
    </td>
    <td><a href="https://github.com/yoavrubin">yoavrubin</a></td>
    <td></td>
  </tr>
  <tr>
    <td>Dessy Daskalov</td>
    <td>Nudge Rewards</td>
    <td>Pedometer</td>
    <td>
        <ul>
            <li><a href="http://www.dessydaskalov.com/">www.dessydaskalov.com</a></li>
            <li><a href="https://twitter.com/dess_e">@dess_e</a></li>
        </ul>
    </td>
    <td><a href="https://github.com/dessy">dessy</a></td>
    <td>dessy.daskalov@gmail.com</td>
  </tr>
  <tr>
    <td>Carl Friedrich Bolz</td>
    <td>King's College London</td>
    <td>object model</td>
    <td>
        <ul>
            <li><a href="https://cfbolz.de">cfbolz.de</a></li>
            <li><a href="https://twitter.com/cfbolz">@cfbolz</a></li>
        </ul>
    </td>
    <td><a href="https://github.com/cfbolz">cfbolz</a></td>
    <td>cfbolz@gmx.de</td>
  </tr>
  <tr>
    <td>Jessica Hamrick</td>
    <td>University of California, Berkeley</td>
    <td>sampler</td>
    <td>
        <ul>
            <li><a href="http://www.jesshamrick.com">www.jesshamrick.com</a></li>
            <li><a href="https://twitter.com/jhamrick">@jhamrick</a></li>
        </ul>
    </td>
    <td><a href="https://github.com/jhamrick">jhamrick</a></td>
    <td>jhamrick@berkeley.edu</td>
  </tr>
  <tr>
    <td>Allison Kaptur</td>
    <td>Hacker School</td>
    <td>byterun</td>
    <td>@akaptur</td>
    <td>@akaptur</td>
    <td>allison.kaptur@gmail.com</td>
  </tr>
  <tr>
    <td>Daniel Rocco</td>
    <td>BrightLink Technology</td>
    <td>contingent</td>
    <td>@drocco007</td>
    <td>drocco007</td>
    <td>drocco@gmail.com</td>
  </tr>
  <tr>
    <td>Brandon Rhodes</td>
    <td>Dropbox</td>
    <td>contingent</td>
    <td>@brandon_rhodes</td>
    <td>brandon-rhodes</td>
    <td>brandon@rhodesmill.org</td>
  </tr>
    <tr>
    <td>Dann Toliver</td>
    <td>Bento Box</td>
    <td>dagoba</td>
    <td>
        <ul>
            <li><a href="http://danntoliver.com">danntoliver.com</a></li>
            <li><a href="https://twitter.com/dann">@dann</a></li>
        </ul>
    </td>
    <td><a href="https://github.com/dxnn">dxnn</a></td>
    <td>aosa@mrmuster.com</td>
  </tr>
</table>
