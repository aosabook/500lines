"""An implementation of the Knuth-Plass algorithm for breaking
paragraphs into lines, used in TeX."""

from collections import namedtuple
from enum import Enum
import json
import sys

PAGE_WIDTH = 842
PAGE_HEIGHT = 595
HEADER_HEIGHT = 100
LINE_HEIGHT = 36
MARGIN = 72
FONT_SIZE = 20
HEADER_FONT_SIZE = 42
FONT_FACE = 'Times-Roman'

INFINITY = 10000000
MAX_RATIO = 1
PENALTY_ALPHA = 3000
PENALTY_GAMMA = 1000
ADJUSTMENT = 0
RANK = 1000

SPACE_STRETCH = 150
SPACE_SHRINK = 100


class Breakpoint:
    """A suitable position for a breakpoint."""
    def __init__(self, **kwargs):
        self.__dict__.update(kwargs)


class Type(Enum):
    box = 1  # Characters that do not belong to the other categories
    glue = 2  # Spaces
    penalty = 3  # Hyphens, dashes, final break


class Typesetting:
    """A typesetting engine.

    It takes text and figures out where to break in order to justify the
    text at best. With low shrink/stretch ratio comes great readability."""
    def __init__(self, line_length, breakpoints=None):
        # Main attributes
        self.line_length = line_length
        self.blocks = []
        self.current_position = 0
        self.breakpoints = breakpoints
        # Metrics
        self.current_line = None
        self.current_width = 0
        self.current_stretch = 0
        self.current_shrink = 0
        self.ratios = []
        self.demerits = None
        # Pointers
        self.first_candidate = Breakpoint(position=-1, line=0, fitness=1,
                                          total_width=0, total_stretch=0,
                                          total_shrink=0, total_demerits=0,
                                          previous=None, link=None)
        self.previous_candidate = None
        self.next_candidate = None
        # Candidates
        self.best_candidate_of_class = [None] * 4
        self.least_demerit_of_class = None
        self.least_demerit = None
        # Saves for visualization
        self.graph = {}

    def add_block(self, character, block_type, width=0, stretch=0, shrink=0,
                  penalty=0, flag=False):
        Block = namedtuple('Block', ['character', 'type', 'width', 'stretch',
                                     'shrink', 'penalty', 'flag', 'position'])
        self.blocks.append(Block(character=character, type=block_type,
                                 width=width, stretch=stretch, shrink=shrink,
                                 penalty=penalty, flag=flag,
                                 position=self.current_position))
        self.current_position += 1

    def add_indent_block(self, width):
        self.add_block(character=' ', block_type=Type.box, width=width)

    def add_forced_break_blocks(self):
        self.add_block(character='', block_type=Type.glue,
                       stretch=INFINITY, shrink=0)
        self.add_block(character='\n', block_type=Type.penalty,
                       penalty=-INFINITY, flag=True)

    def compute_blocks(self, text, indent=False):
        """Convert text into blocks: boxes, glues (spaces) and penalties
        (hyphens, dashes, final break)."""
        character_width = json.load(open('character_width.json'))
        if indent:
            self.add_indent_block(width=4 * character_width[' '])
        last_seen_character = None
        for character in text:
            if character == '\n':
                last_seen_character = '\n'
                continue
            if character == '·':  # Possible hyphen
                self.add_block(character='-', block_type=Type.penalty,
                               width=character_width['-'],
                               penalty=50, flag=True)
            elif character == ' ':  # Glue
                self.add_block(character=' ', block_type=Type.glue,
                               width=character_width[character],
                               stretch=SPACE_STRETCH, shrink=SPACE_SHRINK)
            else:
                # Extra work if this is a bullet point
                if character == '-' and (last_seen_character == '\n' or
                                         last_seen_character is None):
                    self.add_forced_break_blocks()
                    self.add_indent_block(width=8 * character_width[' '])
                self.add_block(character=character, block_type=Type.box,
                               width=character_width[character])
                if character == '-':
                    self.add_block(character='-', block_type=Type.penalty,
                                   width=0,
                                   penalty=50, flag=True)
            last_seen_character = character
        self.add_forced_break_blocks()

    def compute_breakpoints(self):
        """Compute the best possible breakpoints.

        Basically, it determines the shortest path in a directed
        acyclic graph while constructing it."""
        for block in self.blocks:
            if block.type is Type.box:
                self.current_width += block.width
            if self.is_legal_breakpoint(block):
                # self.verbose(block.position)
                self.find_best_previous_breakpoints(block)
                if not self.first_candidate:
                    raise Exception(
                        'Your contraints are too harsh. Please relax.')
            if block.type is Type.glue:
                self.current_width += block.width
                self.current_stretch += block.stretch
                self.current_shrink += block.shrink
        best_candidate = self.choose_best_candidate()
        if ADJUSTMENT != 0:
            best_candidate = self.choose_adjusted_candidate(best_candidate)
        self.breakpoints = (
            [-1] + self.determine_breakpoint_sequence(best_candidate))

    def find_best_previous_breakpoints(self, block):
        """For a certain block that is a legal breakpoint, compute the
        best previous breakpoint possible among the linked list of
        candidates."""
        current_last_breakpoint = self.first_candidate
        self.previous_candidate = None
        while current_last_breakpoint:
            # This loop iterates through the candidate breakpoints that could
            # precede the legal breakpoint block
            self.least_demerit_of_class = [float('inf')] * 4
            self.least_demerit = min(self.least_demerit_of_class)
            self.current_line = None
            while current_last_breakpoint and (
                    not self.current_line or
                    current_last_breakpoint.line < self.current_line or
                    self.current_line >= RANK):
                # This loop iterates through the candidate breakpoints within
                # the same line (self.current_line) that could precede the
                # breakpoint (pos: block.position, line: self.current_line + 1)
                self.next_candidate = current_last_breakpoint.link
                self.consider_breakpoint(current_last_breakpoint, block)
                current_last_breakpoint = self.next_candidate
            if self.least_demerit < float('inf'):
                self.insert_new_candidates(current_last_breakpoint,
                                           block.position)

    def consider_breakpoint(self, last_breakpoint, block):
        """Is last_breakpoint a suitable candidate before block?"""
        r = self.adjustment_ratio(last_breakpoint, block)
        if r < -1 or self.is_forced_break(block):
            self.deactivate(last_breakpoint)
        else:
            self.previous_candidate = last_breakpoint
        if -1 <= r <= MAX_RATIO:
            self.update_best_breakpoints(last_breakpoint, block, r)

    def verbose(self, current_position):
        print('Current candidates at [%s], position %d:' % (
            self.word_before(current_position), current_position))
        candidate = self.first_candidate
        while candidate:
            print('[%s](%d:%d)' % (self.word_before(candidate.position),
                                   candidate.line, candidate.position),
                  end=' ')
            candidate = candidate.link
        print()

    def update_best_breakpoints(self, last_breakpoint, block, r):
        """Modifies: least_demerit_of_class, least_demerit"""
        d, c = self.demerits_fitness_class(last_breakpoint, block, r)
        if d < self.least_demerit_of_class[c]:
            self.least_demerit_of_class[c] = d
            self.best_candidate_of_class[c] = last_breakpoint
            self.least_demerit = min(self.least_demerit_of_class)

    def is_legal_breakpoint(self, block):
        return ((block.type is Type.glue and block.position > 0 and
                 self.blocks[block.position - 1].type is Type.box) or
                (block.type is Type.penalty and block.penalty != INFINITY))

    def adjustment_ratio(self, last_breakpoint, block):
        """Compute the ratio of stretchability/shrinkability for the
        current line in order to justify the text.

        It needs to know the current_line we're on (because line_length
        may vary) and updates it for the second loop of
        find_best_previous_breakpoints

        Hopefully the ratio is between -1 and MAX_RATIO.
        Else, it will not be aesthetically pleasant."""
        width = self.current_width - last_breakpoint.total_width
        if block.type is Type.penalty:
            width += block.width
        self.current_line = last_breakpoint.line + 1
        current_line_length = (self.line_length[self.current_line - 1]
                               if self.current_line < len(self.line_length)
                               else self.line_length[-1])
        if width < current_line_length:
            stretch = self.current_stretch - last_breakpoint.total_stretch
            if not stretch:
                return float('inf')
            return (current_line_length - width) / stretch
        elif width > current_line_length:
            shrink = self.current_shrink - last_breakpoint.total_shrink
            if not shrink:
                return float('inf')
            return (current_line_length - width) / shrink
        else:
            return 0

    def is_forced_break(self, block):
        return block.type is Type.penalty and block.penalty == -INFINITY

    def demerits_fitness_class(self, last_breakpoint, block, r):
        """Determine the demerit value of the current line, along with
        its fitness class.

        An abrupt change of fitness class from one line to another is
        penalized by PENALTY_GAMMA."""
        if block.penalty >= 0:
            d = (1 + 100 * abs(r) ** 3 + block.penalty) ** 2
        elif block.penalty != -INFINITY:
            d = (1 + 100 * abs(r) ** 3) ** 2 - block.penalty ** 2
        else:
            d = (1 + 100 * abs(r) ** 3) ** 2
        if block.flag and self.blocks[last_breakpoint.position].flag:
            d += PENALTY_ALPHA
        c = [r < -0.5, r <= 0.5, r <= 1, True].index(True)  # Fitness class
        if abs(c - last_breakpoint.fitness) > 1:
            d += PENALTY_GAMMA
        d += last_breakpoint.total_demerits
        return d, c

    def insert_new_candidates(self, last_breakpoint, pos):
        """Insert new candidates to linked list if suitable."""
        total_width, total_stretch, total_shrink = self.get_values_after(pos)
        for c in range(4):
            if (self.least_demerit_of_class[c] <=
                    self.least_demerit + PENALTY_GAMMA):
                new_candidate = Breakpoint(
                    position=pos,
                    line=self.best_candidate_of_class[c].line + 1,
                    fitness=c, total_width=total_width,
                    total_stretch=total_stretch, total_shrink=total_shrink,
                    total_demerits=self.least_demerit_of_class[c],
                    previous=self.best_candidate_of_class[c],
                    link=last_breakpoint)
                if not self.previous_candidate:
                    self.first_candidate = new_candidate
                else:
                    self.previous_candidate.link = new_candidate
                self.previous_candidate = new_candidate

    def get_values_after(self, pos):
        """Compute width, stretch, shrink until the next box (neither
        whitespace, nor dash, nor hyphen)."""
        total_width = self.current_width
        total_stretch = self.current_stretch
        total_shrink = self.current_shrink
        i = pos
        while i < len(self.blocks) and self.blocks[i].type is not Type.box:
            if self.blocks[i].type is Type.glue:
                total_width += self.blocks[i].width
                total_stretch += self.blocks[i].stretch
                total_shrink += self.blocks[i].shrink
            elif self.blocks[i].penalty == -INFINITY and i > pos:
                break
            i += 1
        return total_width, total_stretch, total_shrink

    def deactivate(self, last_breakpoint):
        """Remove last_breakpoint from linked list."""
        if not self.previous_candidate:
            self.first_candidate = self.next_candidate
        else:
            self.previous_candidate.link = self.next_candidate

    def choose_best_candidate(self):
        """Choose the candidate breakpoint with the fewest total demerits."""
        best_candidate = self.first_candidate
        d = self.first_candidate.total_demerits
        candidate = self.first_candidate.link
        while candidate:
            if candidate.total_demerits < d:
                d = candidate.total_demerits
                best_candidate = candidate
            candidate = candidate.link
        return best_candidate

    def choose_adjusted_candidate(self, best_candidate):
        """Choose another candidate breakpoint if adjustment is required."""
        line = best_candidate.line
        candidate = self.first_candidate
        d = self.first_candidate.total_demerits
        s = 0
        while candidate:
            delta = candidate.line - line
            if ADJUSTMENT <= delta < s or s < delta <= ADJUSTMENT:
                s = delta
                d = candidate.total_demerits
                best_candidate = candidate
            elif delta == s and candidate.total_demerits < d:
                d = candidate.total_demerits
                best_candidate = candidate
            candidate = candidate.link
        return best_candidate

    def determine_breakpoint_sequence(self, best_candidate):
        """Determine the best breakpoint sequence."""
        line = best_candidate.line
        seq = []
        for j in range(line):
            seq.append(best_candidate.position)
            best_candidate = best_candidate.previous
        return seq[::-1]

    def substring(self, begin, end):
        """Get the subtext between two breakpoints."""
        return ''.join([block.character for block in self.blocks[begin + 1:end]
                        if block.type is not Type.penalty
                        or block.character == '\t'])

    def word_before(self, pos):
        """Get the word before a breakpoint."""
        i = pos - 1
        while self.blocks[i].type is not Type.glue:
            i -= 1
        return self.substring(i, pos)

    def compute_metrics(self):
        index = 1
        ratios = []
        self.current_width = 0
        self.current_stretch = 0
        self.current_shrink = 0
        self.first_candidate = Breakpoint(position=-1, line=0, fitness=1,
                                          total_width=0, total_stretch=0,
                                          total_shrink=0, total_demerits=0,
                                          previous=None, link=None)
        for block in self.blocks:
            if block.type is Type.box:
                self.current_width += block.width
            if block.position == self.breakpoints[index]:
                current_last_breakpoint = self.first_candidate
                self.previous_candidate = None
                self.next_candidate = current_last_breakpoint.link
                r = self.adjustment_ratio(current_last_breakpoint, block)
                ratios.append(r)
                self.least_demerit_of_class = [float('inf')] * 4
                self.least_demerit = min(self.least_demerit_of_class)
                self.update_best_breakpoints(current_last_breakpoint, block, r)
                current_last_breakpoint = self.next_candidate
                self.insert_new_candidates(current_last_breakpoint,
                                           block.position)
                index += 1
            if block.type is Type.glue:
                self.current_width += block.width
                self.current_stretch += block.stretch
                self.current_shrink += block.shrink
        self.ratios = ratios
        self.demerits = self.first_candidate.total_demerits


class Rendering:
    """A rendering engine.

    It paints the blocks in the PostScript file, according to the
    shrink/stretch ratios."""
    def __init__(self):
        self.f = None

    def __enter__(self):
        self.f = open('output.ps', 'w')
        self.f.write('%!PS\n')
        self.f.write('<< /PageSize [{} {}] /ImagingBBox null '
                     '>> setpagedevice\n'.format(PAGE_WIDTH, PAGE_HEIGHT))
        self.f.write('/{}\n'.format(FONT_FACE))
        self.f.write('{} selectfont\n'.format(FONT_SIZE))
        self.f.write('/xcur { currentpoint pop } def\n')
        self.f.write('/center { '
                     'dup '
                     '/str exch def '
                     '/sw str stringwidth pop def ' +
                     '/xpos {} sw sub 2 div xcur sub def '.format(PAGE_WIDTH) +
                     'xpos 0 rmoveto '
                     '} def\n')
        self.f.write('/header { ' +
                     '/ypos {} {} sub def '.format(PAGE_HEIGHT,
                                                   HEADER_HEIGHT) +
                     '0 0 0 setrgbcolor '
                     '0 setlinewidth '
                     'newpath ' +
                     '0 {} moveto '.format(PAGE_HEIGHT) +
                     '{} {} lineto '.format(PAGE_WIDTH, PAGE_HEIGHT) +
                     '{} ypos lineto '.format(PAGE_WIDTH) +
                     '0 ypos lineto '
                     'closepath fill stroke ' +
                     '{} {} moveto '.format(MARGIN,
                                            PAGE_HEIGHT - (HEADER_HEIGHT +
                                                           HEADER_FONT_SIZE)
                                            / 2 + 8) +
                     '/{} {} selectfont '.format(FONT_FACE, HEADER_FONT_SIZE) +
                     '0.5 0.5 0.9 setrgbcolor '
                     'center show ' +
                     '{} ypos moveto '.format(MARGIN) +
                     '0 0 0 setrgbcolor ' +
                     '/{} {} selectfont '.format(FONT_FACE, FONT_SIZE) +
                     '} def\n')
        return self

    def __exit__(self, type, value, traceback):
        self.f.close()

    def add_header(self, title):
        self.f.write('({}) header\n'.format(title))

    def paint(self, blocks, breakpoints, ratios):
        x, y = 0, PAGE_HEIGHT - HEADER_HEIGHT - MARGIN
        for line, ratio in zip(range(len(breakpoints) - 1), ratios):
            for i in range(breakpoints[line] + 1,
                           breakpoints[line + 1] + 1):
                if (blocks[i].type == Type.penalty and
                        i < breakpoints[line + 1]):
                    continue
                if blocks[i].character != ' ':
                    self.f.write('{} {} moveto ({}) show\n'.format(
                        MARGIN + x * FONT_SIZE / 1000, y,
                        blocks[i].character))
                if ratio > 0:
                    x += blocks[i].width + ratio * blocks[i].stretch
                else:
                    x += blocks[i].width + ratio * blocks[i].shrink
            y -= LINE_HEIGHT
            x = 0
        self.f.write('showpage\n')


def read_input():
    slides = []
    text = ''
    for line in sys.stdin:
        if line and line[0] == '#':  # New slide
            if text:
                slides.append((title, text.strip()))
                text = ''
            title = line[2:-1]
        else:
            text += line
    slides.append((title, text.strip()))
    return slides


def main():
    """Main program."""
    slides = read_input()

    line_length = [float(PAGE_WIDTH - 2 * MARGIN) * 1000 / FONT_SIZE]
    with Rendering() as postscript:
        for title, text in slides:
            postscript.add_header(title)
            tex = Typesetting(line_length)
            tex.compute_blocks(text)
            tex.compute_breakpoints()
            tex.compute_metrics()
            postscript.paint(tex.blocks, tex.breakpoints, tex.ratios)

if __name__ == '__main__':
    main()
