"""Contingent: a build system that responds to changes with minimal rebuilding.

Many build systems respond to an edited input file by either doing too
much, like recompiling your entire project, or rebuilding a whole
collection of documents.  Others do too little, regenerating only the
most obviously affected output files but missing subtle ways in which
other build products also need to be regenerated.

Contingent allows a programmer to describe a build process as a network
of Python function calls.  Contingent will automatically learn the
dependencies between these calls; re-run every necessary downstream step
when an input file changes; prevent a routine from re-running if none of
its inputs has changed; and re-learn the inputs to a step that is re-run
in case cross references between resources are added or deleted in the
course of a project's history.

"""
