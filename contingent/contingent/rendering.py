"""Output routines related to the graph type."""

from types import FunctionType

def as_graphviz(graph):
    """Render this ``contingent.Graph`` object as graphviz code.

    To turn the output of this routine into an image, you might save the
    text in a file named "output.dot" and then run:

    $ dot -Tpng output.dot > output.png

    """
    edges = graph.edges()
    inputs = set(input for input, consequence in edges)
    consequences = set(consequence for input, consequence in edges)
    lines = ['digraph {', 'graph [rankdir=LR];']
    append = lines.append

    def node(task):
        """Recognize (func, arg_tuple) nodes and format them prettily."""

        is_function_and_args = (isinstance(task, tuple) and len(task) == 2
                                and isinstance(task[0], FunctionType))
        if is_function_and_args:
            function, args = task
            name = task[0].__name__
            args = repr(args)
            if args.endswith(',)'):
                args = args[:-2] + ')'
            return '"{}{}"'.format(name, args)
        return '"{}"'.format(task)

    append('node [shape=rect penwidth=2 color="#DAB21D"')
    append('      style=filled fillcolor="#F4E5AD"]')

    append('{rank=same')
    for task in graph.sorted(inputs - consequences):
        append(node(task))
    append('}')

    append('node [shape=rect penwidth=2 color="#708BA6"')
    append('      style=filled fillcolor="#DCE9ED"]')

    append('{rank=same')
    for task in graph.sorted(consequences - inputs):
        append(node(task))
    append('}')

    append('node [shape=oval penwidth=0 style=filled fillcolor="#E8EED2"')
    append('      margin="0.05,0"]')

    for task, consequence in edges:
        append('{} -> {}'.format(node(task), node(consequence)))

    append('}')
    return '\n'.join(lines)
