import re
from HTMLParser import HTMLParser
from pygments import highlight
from pygments.lexers import get_lexer_by_name, guess_lexer
from pygments.formatters import HtmlFormatter

html_parser = HTMLParser()


def pygmentize_pre_blocks(html):
    formatter = HtmlFormatter()

    def _highlight(match):
        code = match.group(2).strip('\n')
        already_marked_up = '<' in code
        if already_marked_up:
            return u'<pre{}>{}</pre>'.format(match.group(1), code)
        code = html_parser.unescape(code)
        if code.startswith('#!'):
            lexer_name, code = code[2:].split('\n', 1)
            lexer = get_lexer_by_name(lexer_name)
        else:
            lexer = guess_lexer(code)
        return highlight(code, lexer, formatter)
        # TODO: return u'<pre{}>{}</pre>'.format(match.group(1), h)?

    return re.sub(r'(?s)<pre([^>]*)>(.*?)</pre>', _highlight, html)
