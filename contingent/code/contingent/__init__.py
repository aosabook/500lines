"""Contingent: a build system that responds to changes with minimal rebuilding.

Most build systems allow only absolute rules, like "If ``aux.c`` has
been modified, then *always* rebuild ``aux.o``."  But when documents are
built from a collection of files that contain cross references, absolute
rules are overly limiting.

Contingent allows a programmer to instead describe a build process as a
network of Python function calls, that each describe one task.  The
return values alone determine whether a given downstream task needs to
be rebuilt.  Contingent will:

* Automatically learn the dependencies between tasks.

* Re-run every downstream task when an upstream output changes.

* Prevent a routine from re-running if none of its inputs has changed.

* Re-learn the inputs to a step that is re-run, in case dependencies
  between resources are added or deleted in the course of a project's
  history.

"""
