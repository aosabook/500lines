"""An implementation of the Knuth-Plass algorithm for breaking paragraphs into lines, used in TeX."""

from collections import namedtuple
from enum import Enum

INFINITY = 10000000
RHO = 1.3
ALPHA = 3000
GAMMA = 1000
ADJUSTMENT = 0

class Node:
	"""A node, referring to a suitable position for a breakpoint."""
	def __init__(self, **kwargs):
		self.__dict__.update(kwargs)
	def __str__(self):
		if self:
			return '<pos: {}, line: {}, c: {}, tw: {}, tst: {}, tsh: {}, td: {}, prev: {}, link: {}>'.format(self.position, self.line, self.fitness, self.total_width, self.total_stretch, self.total_shrink, self.total_demerits, self.previous.position if self.previous else None, self.link.position if self.link else None)
		else:
			return None

class Type(Enum):
	box = 1 # Characters that do not belong to the other categories
	glue = 2 # Spaces
	penalty = 3 # Hyphens, dashes, final break

def convert(text):
	"""Convert text into blocks: boxes, glues (spaces) and penalties (hyphens, dashes, final break)."""
	character_width = {' ': 6, '0': 10, '1': 9, '2': 10, '3': 10, '4': 10, '5': 10, '6': 10, '7': 10, '8': 10, '9': 10, '!': 5, '"': 6, '#': 10, '$': 10, '%': 16, '&': 12, '\'': 3, '(': 6, ')': 6, '*': 7, '+': 11, ',': 5, '-': 6, '.': 5, '/': 5, ':': 5, ';': 5, '<': 11, '=': 11, '>': 11, '?': 10, '@': 18, 'A': 12, 'B': 12, 'C': 13, 'D': 13, 'E': 12, 'F': 11, 'G': 14, 'H': 13, 'I': 5, 'J': 9, 'K': 12, 'L': 10, 'M': 15, 'N': 13, 'O': 14, 'P': 12, 'Q': 14, 'R': 13, 'S': 12, 'T': 11, 'U': 13, 'V': 12, 'W': 17, 'X': 12, 'Y': 12, 'Z': 11, '[': 5, '\\': 5, ']': 5, '^': 9, '_': 10, '`': 6, 'a': 10, 'b': 10, 'c': 9, 'd': 10, 'e': 10, 'f': 5, 'g': 10, 'h': 10, 'i': 4, 'j': 4, 'k': 9, 'l': 4, 'm': 15, 'n': 10, 'o': 10, 'p': 10, 'q': 10, 'r': 6, 's': 9, 't': 5, 'u': 10, 'v': 9, 'w': 13, 'x': 9, 'y': 9, 'z': 9, '{': 6, '|': 5, '}': 6, '~': 11}
	Item = namedtuple('Item', ['character', 'type', 'width', 'stretch', 'shrink', 'penalty', 'flag'])
	items = [Item(character='\t', type=Type.box, width=18, stretch=0, shrink=0, penalty=0, flag=False)]
	for character in text:
		if character == '·':
			items.append(Item(character='-', type=Type.penalty, width=6, stretch=0, shrink=0, penalty=50, flag=True))
		elif character == ' ':
			items.append(Item(character=' ', type=Type.glue, width=character_width[character], stretch=3, shrink=2, penalty=0, flag=False))
		else:
			items.append(Item(character=character, type=Type.box, width=character_width[character], stretch=0, shrink=0, penalty=0, flag=False))
		if character == '-':
			items.append(Item(character='-', type=Type.penalty, width=0, stretch=2, shrink=3, penalty=50, flag=True))
	items.append(Item(character='', type=Type.glue, width=0, stretch=INFINITY, shrink=0, penalty=0, flag=False))
	items.append(Item(character='\n', type=Type.penalty, width=0, stretch=0, shrink=0, penalty=-INFINITY, flag=True))
	return items

def compute_breakpoints(text):
	"""Compute the best possible breakpoints.

	Basically, it's computing the shortest path in a DAG while constructing it."""
	first_active_node = Node(position=0, line=0, fitness=1, total_width=0, total_stretch=0, total_shrink=0, total_demerits=0, previous=None, link=None)
	first_passive_node = None
	line_length = [421] * 20
	rank = 0
	current_line = None
	best_node_of_class = [None] * 4
	ratios = {}
	graph = {}
	current_width = current_stretch = current_shrink = 0
	items = convert(text)
	for pos, item in enumerate(items):
		if item.type is Type.box or item.type is Type.glue:
			current_width += item.width
			current_stretch += item.stretch
			current_shrink += item.shrink
		if is_legal_breakpoint(item, items[pos - 1]):
			current_node = first_active_node
			prev_node = None
			while current_node:
				least_demerit_of_class = [INFINITY] * 4
				least_demerit = min(least_demerit_of_class)
				while current_node and (not current_line or current_node.line < current_line or current_line >= rank):
					next_node = current_node.link
					r, current_line = adjustment_ratio(current_node, item, current_width, current_stretch, current_shrink, line_length)
					if abs(r) < RHO:
						ratios[(current_node.position, pos)] = r
					if r < -1 or is_forced_break(item):
						first_active_node, first_passive_node, current_node, prev_node = deactivate_current_node(first_active_node, first_passive_node, current_node, prev_node, next_node)
					else:
						prev_node = current_node
					if -1 <= r <= RHO:
						d, c = demerits_fitness_class(current_node, item, r, items)
						if d < INFINITY:
							graph.setdefault("{} {} {}".format(current_node.fitness, word_before(current_node.position, items), current_node.position), []).append((round(d), "{} {} {}".format(c, word_before(pos, items), pos)))
						if d < least_demerit_of_class[c]:
							least_demerit_of_class[c] = d
							best_node_of_class[c] = current_node
						least_demerit = min(least_demerit_of_class)
					current_node = next_node
				if least_demerit < INFINITY:
					first_active_node, prev_node = insert_new_active_nodes(current_node, pos, current_width, current_stretch, current_shrink, best_node_of_class, least_demerit_of_class, least_demerit, items, first_active_node, prev_node)
			if not first_active_node:
				print('ZOMG')
	with open('knuth.dot', 'w') as f: # Just for visualization
		f.write('digraph G {\n')
		for node in graph:
			for w, child in graph[node]:
				f.write('"{}" -> "{}" [label=" {}"];\n'.format(node, child, w))
		f.write('}')
	best_node = choose_best_node(first_active_node)
	if ADJUSTMENT != 0:
		best_node = choose_appropriate_node(best_node, first_active_node)
	return determine_breakpoint_sequence(best_node), items, ratios

def is_legal_breakpoint(cur, prec):
	return (cur.type is Type.glue and prec.type is Type.box) or (cur.type is Type.penalty and cur.penalty != INFINITY)

def adjustment_ratio(current_node, item, current_width, current_stretch, current_shrink, line_length):
	"""Compute the ratio of stretchability/shrinkability so far.

	Hopefully it is between -1 and RHO."""
	width = current_width - current_node.total_width
	if item.type is Type.penalty:
		width += item.width
	current_line = current_node.line + 1
	if width < line_length[current_line]:
		stretch = current_stretch - current_node.total_stretch
		ratio = (line_length[current_line] - width) / stretch if stretch > 0 else INFINITY
	elif width > line_length[current_line]:
		shrink = current_shrink - current_node.total_shrink
		ratio = (line_length[current_line] - width) / shrink if shrink > 0 else INFINITY
	else:
		ratio = 0
	return ratio, current_line

def is_forced_break(item):
	return item.type is Type.penalty and item.penalty == -INFINITY

def demerits_fitness_class(current_node, item, r, items):
	"""Determine the demerit value of the current line, along with its fitness class.

	An abrupt change of fitness class from one line to another is penalized by GAMMA."""
	if item.penalty >= 0:
		d = (1 + 100 * abs(r) ** 3 + item.penalty) ** 2
	elif item.penalty != -INFINITY:
		d = (1 + 100 * abs(r) ** 3) ** 2 - item.penalty ** 2
	else:
		d = (1 + 100 * abs(r) ** 3) ** 2
	d += ALPHA * item.flag * items[current_node.position].flag # TODO
	if r < -0.5:
		c = 0
	elif r <= 0.5:
		c = 1
	elif r <= 1:
		c = 2
	else:
		c = 3
	if abs(c - current_node.fitness) > 1:
		d += GAMMA
	d += current_node.total_demerits
	return d, c

def insert_new_active_nodes(current_node, pos, current_width, current_stretch, current_shrink, best_node_of_class, least_demerit_of_class, least_demerit, items, first_active_node, prev_node):
	"""Insert new nodes to active list if suitable."""
	total_width, total_stretch, total_shrink = compute_values_after(pos, current_width, current_stretch, current_shrink, items)
	for c in range(4):
		if least_demerit_of_class[c] <= least_demerit + GAMMA:
			new_node = Node(position=pos, line=best_node_of_class[c].line + 1 if best_node_of_class[c] else 1, fitness=c, total_width=total_width, total_stretch=total_stretch, total_shrink=total_shrink, total_demerits=least_demerit_of_class[c], previous=best_node_of_class[c], link=current_node)
			if not prev_node:
				first_active_node = new_node
			else:
				prev_node.link = new_node
			prev_node = new_node
	return first_active_node, prev_node

def compute_values_after(pos, current_width, current_stretch, current_shrink, items):
	"""Compute width, stretch, shrink until the next box (neither whitespace, nor dash, nor hyphen)."""
	total_width, total_stretch, total_shrink = current_width, current_stretch, current_shrink
	i = pos
	while i < len(items) and items[i].type is not Type.box:
		if items[i].type is Type.glue:
			total_width += items[i].width
			total_stretch += items[i].stretch
			total_shrink += items[i].shrink
		elif items[i].penalty == -INFINITY and i > pos:
			break
		i += 1
	return total_width, total_stretch, total_shrink

def deactivate_current_node(first_active_node, first_passive_node, current_node, prev_node, next_node):
	"""Move current node from active list to passive list."""
	if not prev_node:
		first_active_node = next_node
	else:
		prev_node.link = next_node
	current_node.link = first_passive_node
	first_passive_node = current_node
	return first_active_node, first_passive_node, current_node, prev_node

def choose_best_node(first_active_node):
	"""Choose the node with the fewest total demerits."""
	best_node = first_active_node
	d = first_active_node.total_demerits
	node = first_active_node.link
	while node:
		if node.total_demerits < d:
			d = node.total_demerits
			best_node = node
		node = node.link
	return best_node

def choose_appropriate_node(best_node, first_active_node):
	"""Choose another node if adjustment is required."""
	line = best_node.line
	node = first_active_node
	s = 0
	while True:
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

def determine_breakpoint_sequence(best_node):
	"""Determine the best breakpoint sequence."""
	line = best_node.line
	seq = []
	for j in range(line):
		seq.append(best_node.position)
		best_node = best_node.previous
	return seq[::-1]

def substring(begin, end, items):
	"""Get the subtext between two breakpoints."""
	return ''.join([item.character for item in items[begin:end] if item.type is not Type.penalty or item.character == '\t'])

def word_before(breakpoint, items):
	"""Get the word before a breakpoint."""
	i = breakpoint - 1
	while items[i].type is not Type.glue:
		i -= 1
	return substring(i, breakpoint, items)

def main():
	"""Main program.

	"""

	text = "In olden times when wish·ing still helped one, there lived a king whose daugh·ters were all beau·ti·ful; and the young·est was so beau·ti·ful that the sun it·self, which has seen so much, was aston·ished when·ever it shone in her face. Close by the king's castle lay a great dark for·est, and un·der an old lime-tree in the for·est was a well, and when the day was very warm, the king's child went out into the for·est and sat down by the side of the cool foun·tain; and when she was bored she took a golden ball, and threw it up on high and caught it; and this ball was her favor·ite play·thing."

	breakpoints, items, ratios = compute_breakpoints(text)
	breakpoints = [0] + breakpoints
	print('Breakpoints:', breakpoints)
	for i in range(len(breakpoints) - 1):
		print(substring(breakpoints[i], breakpoints[i + 1], items), ratios[(breakpoints[i], breakpoints[i + 1])])

if __name__ == '__main__':
	main()
