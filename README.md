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
  </tr>
  <tr>
    <td>Mike DiBernardo</td>
    <td>Wave</td>
    <td>editorial</td>
    <td>
        <ul>
            <li><a href="https://twitter.com/mdibernardo">@mdibernardo</a></li>
            <li><a href="http://mikedebo.ca">mikedebo.ca</a></li>
        </ul>
    </td>
    <td><a href="https://github.com/MichaelDiBernardo">MichaelDiBernardo</a></td>
  </tr>
   <tr>
    <td>Amy Brown</td>
    <td>indie</td>
    <td>editorial</td>
    <td><ul><li><a href="http://www.amyrbrown.ca/">amyrbrown.ca</a></li>
        <li><a href="http://www.twitter.com/amyrbrown">@amyrbrown</a></li></ul></td>
    <td><a href="https://github.com/amyrbrown">amyrbrown</a></td>
  </tr>
  <tr>
    <td>Allison Kaptur</td>
    <td>Dropbox</td>
    <td>byterun</td>
    <td><ul><li><a href="https://twitter.com/akaptur">@akaptur</a></li></ul></td>
    <td><a href="https://github.com/akaptur">akaptur</a></td>
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
  </tr>
  <tr>
    <td>Brandon Rhodes</td>
    <td>Dropbox</td>
    <td>contingent</td>
    <td><ul><li><a href="https://twitter.com/brandon_rhodes">@brandon_rhodes</a></li></ul></td>
    <td><a href="https://github.com/brandon-rhodes">brandon-rhodes</a></td>
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
  </tr>
  <tr>
    <td>Cate Huston</td>
    <td>&nbsp;</td>
    <td>Image Filter app</td>
    <td>
        <ul>
            <li><a href="http://www.accidentallyincode.com/">www.accidentallyincode.com/</a></li>
            <li><a href="https://twitter.com/catehstn">@catehstn</a></li>
        </ul>
    </td>
    <td><a href="https://github.com/catehstn">catehstn</a></td>
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
  </tr>
  <tr>
    <td>Daniel Jackson</td>
    <td>&nbsp;</td>
    <td>same-origin-policy</td>
    <td>&nbsp;</td>
    <td>&nbsp;</td>
  </tr>
  <tr>
    <td>Daniel Rocco</td>
    <td>BrightLink Technology</td>
    <td>contingent</td>
    <td><ul><li><a href="https://twitter.com/drocco007">@drocco007</a></li></ul></td>
    <td><a href="https://github.com/drocco007">drocco007</a></td>
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
  </tr>
  <tr>
    <td>Dethe Elza</td>
    <td>&nbsp;</td>
    <td>blockcode</td>
    <td>&nbsp;</td>
    <td><a href="https://github.com/dethe">dethe</a></td>
  </tr>
  <tr>
    <td>Dustin Mitchell</td>
    <td>Mozilla</td>
    <td>cluster</td>
    <td>&nbsp;</td>
    <td><a href="https://github.com/djmitche">djmitche</a></td>
  </tr>
  <tr>
    <td>Erick Dransch</td>
    <td>&nbsp;</td>
    <td>Modeller</td>
    <td>
        <ul>
            <li><a href="https://twitter.com/ErickDransch">@ErickDransch</a></li>
        </ul>
    </td>
    <td><a href="https://github.com/EkkiD">EkkiD</a></td>
  </tr>
  <tr>
    <td>Eunsuk Kang</td>
    <td>&nbsp;</td>
    <td>same-origin-policy</td>
    <td>&nbsp;</td>
    <td>&nbsp;</td>
  </tr>
  <tr>
    <td>Greg Wilson</td>
    <td>&nbsp;</td>
    <td>web-server</td>
    <td>
        <ul>
            <li><a href="https://twitter.com/gvwilson">@gvwilson</a></li>
        </ul>
    </td>
    <td><a href="https://github.com/gvwilson">gvwilson</a></td>
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
  </tr>
  <tr>
    <td>Leo Zovic</td>
    <td>&nbsp;</td>
    <td>event-web-framework</td>
    <td>&nbsp;</td>
    <td>&nbsp;</td>
  </tr>
  <tr>
    <td>Malini Das</td>
    <td>Twitch</td>
    <td>ci</td>
    <td>
        <ul>
            <li><a href="http://malinidas.com">malinidas.com</a></li>
            <li><a href="https://twitter.com/malinidas">@malinidas</a></li>
        </ul>
    </td>
    <td><a href="https://github.com/malini">malini</a></td>
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
  </tr>
  <tr>
    <td>Santiago Perez De Rosso</td>
    <td>&nbsp;</td>
    <td>same-origin-policy</td>
    <td>&nbsp;</td>
    <td>&nbsp;</td>
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
  </tr>
</table>


Technical Reviewers
-------------------

<table>
  <tr>
    <td>Amber Yust</td>
    <td>Andrew Gwozdziewycz</td>
    <td>Andrew Kuchling</td>
  </tr>
  <tr>
    <td>Andrew Svetlov</td>
    <td>Andy Shen</td>
    <td>Anton Beloglazov</td>
  </tr>
  <tr>
    <td>Ben Trofatter</td>
    <td>Borys Pierov</td>
    <td>Carise Fernandez</td>
  </tr>
  <tr>
    <td>Charles Stanhope</td>
    <td>Chris Atlee</td>
    <td>Chris Seaton</td>
  </tr>
  <tr>
    <td>Cyryl Płotnicki-Chudyk</td>
    <td>Dan Langer</td>
    <td>Dan Shapiro</td>
  </tr>
  <tr>
    <td>David Pokorny</td>
    <td>Eric Bouwers</td>
    <td>Frederic De Groef</td>
  </tr>
  <tr>
    <td>Graham Lee</td>
    <td>Gregory Eric Sanderson</td>
    <td>James O'Beirne</td>
  </tr>
  <tr>
    <td>Jan de Baat</td>
    <td>Jana Beck</td>
    <td>Jessica McKellar</td>
  </tr>
  <tr>
    <td>Jo Van Eyck</td>
    <td>Joel Crocker</td>
    <td>Johan Thelin</td>
  </tr>
  <tr>
    <td>Johannes Fürmann</td>
    <td>John Morrissey</td>
    <td>Joseph Kaptur</td>
  </tr>
  <tr>
    <td>Josh Crompton</td>
    <td>Joshua T. Corbin</td>
    <td>Kevin Huang</td>
  </tr>
  <tr>
    <td>Maggie Zhou</td>
    <td>Marc Towler</td>
    <td>Marcin Milewski</td>
  </tr>
  <tr>
    <td>Marco Lancini</td>
    <td>Mark Reid</td>
    <td>Matthias Bussonnier</td>
  </tr>
  <tr>
    <td>Max Mautner</td>
    <td>Meggin Kearney</td>
    <td>Mike Aquino</td>
  </tr>
  <tr>
    <td>Natalie Black</td>
    <td>Nick Presta</td>
    <td>Nikhil Almeida</td>
  </tr>
  <tr>
    <td>Nolan Prescott</td>
    <td>Paul Martin</td>
    <td>Piotr Banaszkiewicz</td>
  </tr>
  <tr>
    <td>Preston Holmes</td>
    <td>Pulkit Sethi</td>
    <td>Rail Aliiev</td>
  </tr>
  <tr>
    <td>Ronen Narkis</td>
    <td>Rose Ames</td>
    <td>Sina Jahan</td>
  </tr>
  <tr>
    <td>Stefan Turalski</td>
    <td>William Lachance</td>
  </tr>
</table>
