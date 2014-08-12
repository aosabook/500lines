"""An implementation of the Knuth-Plass algorithm for breaking
paragraphs into lines, used in TeX."""

from collections import namedtuple
from enum import Enum
import json

PAPER_WIDTH = 842
PAPER_HEIGHT = 595
LINE_HEIGHT = 36
MARGIN = 72
FONT_SIZE = 20
FONT_FACE = 'Verdana'

INFINITY = 10000000
MAX_RATIO = 1.3
PENALTY_ALPHA = 3000
PENALTY_GAMMA = 1000
ADJUSTMENT = 0


class Breakpoint:
    """A node, referring to a suitable position for a breakpoint."""
    def __init__(self, **kwargs):
        self.__dict__.update(kwargs)

    def __str__(self):
        if self:
            return ('<pos: {}, line: {}, c: {}, tw: {}, tst: {}, tsh: {}, '
                    'td: {}, prev: {}, link: {}>').format(
                        self.position,
                        self.line, self.fitness, self.total_width,
                        self.total_stretch, self.total_shrink,
                        self.total_demerits,
                        self.previous.position if self.previous else None,
                        self.link.position if self.link else None)
        else:
            return None


class Type(Enum):
    box = 1  # Characters that do not belong to the other categories
    glue = 2  # Spaces
    penalty = 3  # Hyphens, dashes, final break


class Typesetting:
    """A typesetting engine.

    It takes text and figures out where to break in order to justify the
    text at best. With low shrink/stretch ratio comes great readability."""
    def __init__(self, text, line_length, breakpoints=None):
        # Main attributes
        self.line_length = line_length
        self.blocks = self.convert_to_blocks(text)
        self.breakpoints = breakpoints
        # Metrics
        self.current_line = None
        self.current_width = 0
        self.current_stretch = 0
        self.current_shrink = 0
        self.ratios = []
        self.demerits = None
        # Pointers
        self.first_active_node = Breakpoint(position=-1, line=0, fitness=1,
                                            total_width=0, total_stretch=0,
                                            total_shrink=0, total_demerits=0,
                                            previous=None, link=None)
        self.first_passive_node = None
        self.prev_node = None
        self.next_node = None
        # Candidates
        self.best_node_of_class = [None] * 4
        self.least_demerit_of_class = None
        self.least_demerit = None
        # Saves for visualization
        self.graph = {}

    def convert_to_blocks(self, text, indent=True):
        """Convert text into blocks: boxes, glues (spaces) and penalties
        (hyphens, dashes, final break)."""
        character_width = json.load(open('character_width.json'))
        Block = namedtuple('Block', ['character', 'type', 'width', 'stretch',
                                     'shrink', 'penalty', 'flag', 'position'])
        blocks = [Block(character='\t', type=Type.box,
                        width=4 * character_width[' '], stretch=0, shrink=0,
                        penalty=0, flag=False, position=0)] if indent else []
        position = 1 if indent else 0
        for character in text:
            if character == '·':  # Possible hyphen
                block = Block(character='-', type=Type.penalty,
                              width=character_width['-'], stretch=0, shrink=0,
                              penalty=50, flag=True, position=position)
            elif character == ' ':
                block = Block(character=' ', type=Type.glue,
                              width=character_width[character], stretch=300,
                              shrink=200, penalty=0, flag=False,
                              position=position)
            else:
                block = Block(character=character, type=Type.box,
                              width=character_width[character], stretch=0,
                              shrink=0, penalty=0, flag=False,
                              position=position)
            blocks.append(block)
            position += 1
            if character == '-':
                blocks.append(Block(character='-', type=Type.penalty, width=0,
                                    stretch=2, shrink=3, penalty=50, flag=True,
                                    position=position))
                position += 1
        blocks.append(Block(character='', type=Type.glue, width=0,
                            stretch=INFINITY, shrink=0, penalty=0, flag=False,
                            position=position))
        blocks.append(Block(character='\n', type=Type.penalty, width=0,
                            stretch=0, shrink=0, penalty=-INFINITY, flag=True,
                            position=position + 1))
        return blocks

    def compute_breakpoints(self):
        """Compute the best possible breakpoints.

        Basically, it's computing the shortest path in a DAG while
        constructing it."""
        rank = 0
        for block in self.blocks:
            if block.type is Type.box:
                self.current_width += block.width
            if self.is_legal_breakpoint(block):
                last_breakpoint = self.first_active_node
                self.prev_node = None
                while last_breakpoint:
                    self.least_demerit_of_class = [INFINITY] * 4
                    self.least_demerit = min(self.least_demerit_of_class)
                    while last_breakpoint and (
                            not self.current_line or
                            last_breakpoint.line < self.current_line or
                            self.current_line >= rank):  # TODO why
                        self.next_node = last_breakpoint.link
                        r = self.adjustment_ratio(last_breakpoint, block)
                        if r < -1 or self.is_forced_break(block):
                            last_breakpoint = self.deactivate(last_breakpoint)
                        else:
                            self.prev_node = last_breakpoint
                        if -1 <= r <= MAX_RATIO:
                            self.update_best_breakpoints(last_breakpoint,
                                                         block, r)
                        last_breakpoint = self.next_node
                    if self.least_demerit < INFINITY:
                        self.insert_new_active_nodes(last_breakpoint,
                                                     block.position)
                if not self.first_active_node:
                    raise Exception(
                        'Your contraints are too harsh. Please relax.')
            if block.type is Type.glue:
                self.current_width += block.width
                self.current_stretch += block.stretch
                self.current_shrink += block.shrink
        self.make_visualization()
        best_node = self.choose_best_node()
        if ADJUSTMENT != 0:
            best_node = self.choose_appropriate_node(best_node)
        self.breakpoints = self.determine_breakpoint_sequence(best_node)

    def update_best_breakpoints(self, last_breakpoint, block, r):
        """Modifies: least_demerit_of_class, least_demerit"""
        d, c = self.demerits_fitness_class(last_breakpoint, block, r)
        self.backup_for_visualization(last_breakpoint, block, r, d, c)
        if d < self.least_demerit_of_class[c]:
            self.least_demerit_of_class[c] = d
            self.best_node_of_class[c] = last_breakpoint
            self.least_demerit = min(self.least_demerit_of_class)

    def backup_for_visualization(self, last_breakpoint, block, r, d, c):
        """Needs: last_breakpoint, blocks"""
        if d < INFINITY:
            self.graph.setdefault("{} {} {}".format(
                last_breakpoint.fitness,
                self.word_before(last_breakpoint.position),
                last_breakpoint.position), []).append(
                    (round(d), "{} {} {}".format(
                        c, self.word_before(block.position), block.position)))

    def make_visualization(self):
        with open('knuth.dot', 'w') as f:  # Just for visualization
            f.write('digraph G {\n')
            for node in self.graph:
                for w, child in self.graph[node]:
                    f.write('"{}" -> "{}" [label=" {}"];\n'.format(
                        node, child, w))
            f.write('}')

    def is_legal_breakpoint(self, block):
        return ((block.type is Type.glue and block.position > 0 and
                 self.blocks[block.position - 1].type is Type.box) or
                (block.type is Type.penalty and block.penalty != INFINITY))

    def adjustment_ratio(self, last_breakpoint, block):
        """Compute the ratio of stretchability/shrinkability for the
        current line in order to justify the text.

        Needs: last_breakpoint, block, current_width, current_stretch,
               current_shrink, line_length
        Modifies: current_line

        Hopefully it is between -1 and MAX_RATIO.
        Else, it won't be aesthetically pleasant."""
        width = self.current_width - last_breakpoint.total_width
        if block.type is Type.penalty:
            width += block.width
        self.current_line = last_breakpoint.line + 1
        current_line_length = self.line_length[self.current_line]
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

        Needs: last_breakpoint, block, r, blocks
        Modifies: None

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
        if r < -0.5:
            c = 0
        elif r <= 0.5:
            c = 1
        elif r <= 1:
            c = 2
        else:
            c = 3
        if abs(c - last_breakpoint.fitness) > 1:
            d += PENALTY_GAMMA
        d += last_breakpoint.total_demerits
        return d, c

    def insert_new_active_nodes(self, last_breakpoint, pos):
        """Insert new nodes to active list if suitable.

        Needs: last_breakpoint, pos, current_width, current_stretch,
               current_shrink, best_node_of_class,
               least_demerit_of_class, least_demerit, blocks,
               first_active_node, prev_node
        Modifies: first_active_node, prev_node"""
        total_width, total_stretch, total_shrink = self.get_values_after(pos)
        for c in range(4):
            if (self.least_demerit_of_class[c] <=
                    self.least_demerit + PENALTY_GAMMA):
                new_node = Breakpoint(
                    position=pos,
                    line=self.best_node_of_class[c].line + 1 if
                    self.best_node_of_class[c] else 1,
                    # TODO Is this really necessary?
                    fitness=c, total_width=total_width,
                    total_stretch=total_stretch, total_shrink=total_shrink,
                    total_demerits=self.least_demerit_of_class[c],
                    previous=self.best_node_of_class[c], link=last_breakpoint)
                if not self.prev_node:  # TODO Why?
                    self.first_active_node = new_node
                else:
                    self.prev_node.link = new_node
                self.prev_node = new_node

    def get_values_after(self, pos):
        """Compute width, stretch, shrink until the next box (neither
        whitespace, nor dash, nor hyphen).
        Needs: pos, current_width, current_stretch, current_shrink
        Modifies: None"""
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
        """Move last breakpoint from active list to passive list.
        Needs: first_active_node, first_passive_node, last_breakpoint,
               prev_node, next_node
        Modifies: first_active_node, first_passive_node,
                  last_breakpoint, prev_node"""
        if not self.prev_node:
            self.first_active_node = self.next_node
        else:
            self.prev_node.link = self.next_node
        last_breakpoint.link = self.first_passive_node
        self.first_passive_node = last_breakpoint
        return last_breakpoint

    def choose_best_node(self):
        """Choose the node with the fewest total demerits.
        Needs: first_active_node
        Modifies: None"""
        best_node = self.first_active_node
        d = self.first_active_node.total_demerits
        node = self.first_active_node.link
        while node:
            if node.total_demerits < d:
                d = node.total_demerits
                best_node = node
            node = node.link
        return best_node

    def choose_appropriate_node(self, best_node):
        """Choose another node if adjustment is required.
        Needs: first_active_node
        Modifies: None"""
        line = best_node.line
        node = self.first_active_node
        s = 0
        while node:
            delta = node.line - line
            if ADJUSTMENT <= delta < s or s < delta <= ADJUSTMENT:
                s = delta
                d = node.total_demerits
                best_node = node
            elif delta == s and node.total_demerits < d:
                d = node.total_demerits
                best_node = node
            node = node.link
        return best_node

    def determine_breakpoint_sequence(self, best_node):
        """Determine the best breakpoint sequence."""
        line = best_node.line
        seq = []
        for j in range(line):
            seq.append(best_node.position)
            best_node = best_node.previous
        return seq[::-1]

    def substring(self, begin, end):
        """Get the subtext between two breakpoints.
        Needs: blocks"""
        return ''.join([block.character for block in self.blocks[begin + 1:end]
                        if block.type is not Type.penalty
                        or block.character == '\t'])

    def word_before(self, pos):
        """Get the word before a breakpoint.
        Needs: blocks"""
        i = pos - 1
        while self.blocks[i].type is not Type.glue:
            i -= 1
        return self.substring(i, pos)

    def compute_metrics(self):
        index = 1
        ratios = []
        for block in self.blocks:
            if block.type is Type.box:
                self.current_width += block.width
            if block.position == self.breakpoints[index]:
                last_breakpoint = self.first_active_node
                self.prev_node = None
                self.next_node = last_breakpoint.link
                r = self.adjustment_ratio(last_breakpoint, block)
                ratios.append(r)
                self.least_demerit_of_class = [INFINITY] * 4
                self.least_demerit = min(self.least_demerit_of_class)
                self.update_best_breakpoints(last_breakpoint, block, r)
                last_breakpoint = self.next_node
                self.insert_new_active_nodes(last_breakpoint, block.position)
                index += 1
            if block.type is Type.glue:
                self.current_width += block.width
                self.current_stretch += block.stretch
                self.current_shrink += block.shrink
        self.ratios = ratios
        self.demerits = self.first_active_node.total_demerits


class Rendering:
    def __init__(self, blocks, breakpoints, ratios):
        self.f = None
        self.blocks = blocks
        self.breakpoints = breakpoints
        self.ratios = ratios

    def __enter__(self):
        self.f = open('output.ps', 'w')
        self.f.write('%!PS\n')
        self.f.write('<< /PageSize [{} {}] /ImagingBBox null '
                     '>> setpagedevice\n'.format(PAPER_WIDTH, PAPER_HEIGHT))
        self.f.write('/{}\n'.format(FONT_FACE))
        # f.write('{} {} moveto {} {} lineto stroke\n'.format(
        #    MARGIN, MARGIN, MARGIN, PAPER_HEIGHT - MARGIN)) # Help lines
        # f.write('{} {} moveto {} {} lineto stroke\n'.format(
        #    PAPER_WIDTH - MARGIN, MARGIN, PAPER_WIDTH - MARGIN,
        #        PAPER_HEIGHT - MARGIN))
        self.f.write('{} selectfont\n'.format(FONT_SIZE))
        return self

    def __exit__(self, type, value, traceback):
        self.f.close()

    def paint(self):
        x, y = 0, PAPER_HEIGHT - MARGIN - 15
        for line, ratio in zip(range(len(self.breakpoints) - 1), self.ratios):
            for i in range(self.breakpoints[line] + 1,
                           self.breakpoints[line + 1]):
                if self.blocks[i].type == Type.penalty:
                    continue
                if self.blocks[i].character != ' ':
                    self.f.write('{} {} moveto ({}) show\n'.format(
                        MARGIN + x * FONT_SIZE / 1000, y,
                        self.blocks[i].character))
                if ratio > 0:
                    x += self.blocks[i].width + ratio * self.blocks[i].stretch
                else:
                    x += self.blocks[i].width + ratio * self.blocks[i].shrink
            y -= LINE_HEIGHT
            x = 0


def main():
    """Main program."""

    text = ("In olden times when wish·ing still helped one, there lived "
            "a king whose daugh·ters were all beau·ti·ful; and the "
            "young·est was so beau·ti·ful that the sun it·self, which "
            "has seen so much, was aston·ished when·ever it shone in "
            "her face. Close by the king's castle lay a great dark "
            "for·est, and un·der an old lime-tree in the for·est was a "
            "well, and when the day was very warm, the king's child "
            "went out into the for·est and sat down by the side of the "
            "cool foun·tain; and when she was bored she took a golden "
            "ball, and threw it up on high and caught it; and this "
            "ball was her favor·ite play·thing.")

    line_length = [float(PAPER_WIDTH - 2 * MARGIN) * 1000 / FONT_SIZE] * 20
    tex = Typesetting(text, line_length, [-1, 66, 142, 214, 289,
                                          362, 433, 500, 572, 599])
    tex.compute_breakpoints()
    tex.compute_metrics()
    with Rendering(tex.blocks, tex.breakpoints, tex.ratios) as postscript:
        postscript.paint()
    print('Breakpoints:', tex.breakpoints)
    for i in range(len(tex.breakpoints) - 1):
        print(tex.substring(tex.breakpoints[i], tex.breakpoints[i + 1]),
              tex.ratios[i])

if __name__ == '__main__':
    main()
