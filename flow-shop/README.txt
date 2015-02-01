Author: Christian Muise
Project: Flow Shop Scheduler
Requirements: Python


 -{ Flow Shop Scheduling }-

The flow shop scheduling problem is a well-studied optimization
problem in which we must determine the processing time for various
tasks on a set of machines. In particular, every flow shop problem
consists of n machines and m jobs. Each job is made up of exactly
n tasks, and we can assume that the ith task of a job must use
machine i and requires a predetermined amount of processing time.
Further, the order of the tasks for any given job should follow
the order of the machines available (i.e., the first task uses
machine 1, and so on) -- for a given job, task i must be completed
prior to the start of task i+1. The final restriction is that no
two tasks can be processed on a machine at a given time.

The objective in flow shop scheduling is to minimize the total time
it takes to process all of the tasks from every job to completion.
This objective is typically referred to as the 'makespan'. The
general problem has many applications, but is most related to
optimizing production facilities for the construction of various
commercial products.

Because the order of tasks within a job is predetermined, a solution
to the flowshop scheduling problem can be represented as a permutation
of the jobs. The order of jobs processed on a machine will be the same
for every machine, and given a permutation, a task for machine i in job
j is scheduled to be the latest of the following two possibilities:

1) The completion of the task for machine i in job j-1 (i.e., the most
   recent task on the same machine), or

2) The completion of the task for machine i-1 in job j (i.e., the most
   recent task on the same job)

Due to the simple form of the problem, any permutation of jobs is
a valid solution to the problem, and the optimal solution will
correspond to some permutation. We thus search for improved solutions
by changing the permutation of jobs and measuring the corresponding
makespan.


 -{ Implementation }-

This directory holds a basic implementation of a flow shop scheduler
that parses a flow shop problem from a standard benchmark set, solves
it heuristically, and then displays the resulting schedule. A key
feature of the solver is a dynamic selection of the various search
strategies available that attempts to give preference to those
strategies that have worked well in the past.


 -{ Extra Resources }-

A wonderful website to visually see how flow shop scheduling works:
* http://posh-wolf.herokuapp.com/

More information on the flow shop scheduling (and related) problems
can be found on wikipedia:
* http://en.wikipedia.org/wiki/Flow_shop_scheduling
