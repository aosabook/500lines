import doctest
from glob import glob
import subprocess

from docutils.core import publish_doctree
from docutils.parsers.rst.directives import register_directive
from docutils.parsers.rst.directives.misc import Include

from contingent.io import looping_wait_on
from contingent.projectlib import Project
from contingent.rendering import as_graphviz


project = Project()
task = project.task


class RSTIncludeSpy(Include):
    """Tracing reStructuredText include directive

    Determine the exact content included by a docutils publishing run.

    Include directive that tracks the contents included into a published
    reStructuredText document. As include directives are processed, the
    spy saves the output of each ``run`` call before handing them back
    to the directive caller.

    Calling ``get_include_contents`` retrieves the included contents as
    a concatenated string and clears its cache for the next run.

    The chapter builder below uses the spy to determine the exact
    content included from various external files into the chapter, which
    allows it to detect when a change to an included file will *not*
    impact the output and halt the build.

    """

    include_contents = []

    @classmethod
    def get_include_contents(cls):
        val = ''.join(cls.include_contents)
        cls.include_contents = []
        return val

    def run(self):
        # docutils doesn't provide a way for our subclass to replace the
        # file reading routine, so we are forced to do our own read here
        # to maintain the task graph. Alternatives would be to
        # reimplement the entire long superclass method or to
        # monkeypatch docutils. This redundancy seems like the best of
        # the three choices.
        read_text_file(self.arguments[0])
        val = super().run()
        self.include_contents.append(str(val[0]))
        return val

register_directive('include', RSTIncludeSpy)


@task
def read_text_file(path):
    with open(path) as f:
        return f.read()


@task
def check_rst_includes(path):
    publish_doctree(read_text_file(path))
    return RSTIncludeSpy.get_include_contents()


@task
def chapter_doctests(path):
    read_text_file(path)
    doctest.testfile(
        path,
        module_relative=False,
        optionflags=doctest.ELLIPSIS,
        )

    with project.cache_off():
        for dot in glob('*.dot'):
            read_text_file(dot)


@task
def render(path):
    if path.endswith('.dot'):
        read_text_file(path)
        png = path[:-3] + 'png'
        subprocess.call(['dot', '-Tpng', '-o', png, path])
    elif path.endswith('.rst'):
        read_text_file(path)
        chapter_doctests(path)
        check_rst_includes(path)
        subprocess.call(['rst2html.py', 'chapter.rst', 'chapter.html'])


def get_paths():
    return tuple(glob('*.rst') + glob('contingent/*.py') + glob('*.dot'))


def main():
    project.verbose = True

    project.start_tracing()
    for path in get_paths():
        render(path)
    print(project.stop_tracing(True))

    open('chapter.dot', 'w').write(as_graphviz(project._graph))

    while True:
        print('=' * 72)
        print('Watching for files to change')
        changed_paths = looping_wait_on(get_paths())
        print('=' * 72)
        print('Reloading:', ' '.join(changed_paths))
        with project.cache_off():
            for path in changed_paths:
                read_text_file(path)
        project.start_tracing()
        project.rebuild()
        print(project.stop_tracing(True))


if __name__ == '__main__':
    main()
