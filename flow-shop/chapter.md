# A Flow Shop Scheduler
*Flow shop scheduling* is one of the most challenging and well-studied problems in operations research. In this chapter we consider the implementation of a flow shop scheduling solver that uses a technique called *local search*. The solver attempts to heuristically improve an existing solution for a given amount of time using a variety of strategies. The implementation is written in Python, and has no external requirements. By leveraging some of Python's lesser known functionality, the solver is able to dynamically change its search strategy during the the solving process based on what strategies work well.

First we provide some background material on the flow shop scheduling problem and local search techniques. We then look in detail at the general solver code and the various heuristics / neighbourhood selection strategies that we consider. Next we consider the dynamic strategy selection that the solver uses to tie everything together. Finally, we conclude with a summary of the project and some lessons learned through the implementation process.


## Background
### Flow Shop Scheduling
The flow shop scheduling problem is a well-studied optimization problem in which we must determine the processing time for various tasks on a set of machines. Take for example a car manufacturer that has an assembly line where each part of the car is completed in sequence on different machines. Different orders, however, may have custom requirements making the task of painting the outside, for example, variable from one car to the next. In our example, each car is a new *job* and each part for the car is called a *task*. Every job will have the same sequence of tasks to complete.

The objective in flow shop scheduling is to minimize the total time it takes to process all of the tasks from every job to completion. Typically, this objective is referred to as the *makespan*. This general problem has many applications, but is most related to optimizing production facilities for the construction of various commercial products.

Every flow shop problem consists of *n* machines and *m* jobs. Each job is made up of exactly *n* tasks, and we can assume that the *i*-th task of a job must use machine *i* and requires a predetermined amount of processing time: *p*(*j*,*i*) is the processing time for the *i*-th task of job *j*. Further, the order of the tasks for any given job should follow the order of the machines available -- for a given job, task *i* must be completed prior to the start of task *i*+1. The final restriction is that no two tasks can be processed on a machine simultaneously.

Because the order of tasks within a job is predetermined, a solution to the flow shop scheduling problem can be represented as a permutation of the jobs. The order of jobs processed on a machine will be the same for every machine, and given a permutation, a task for machine *i* in job *j* is scheduled to be the latest of the following two possibilities:

1. The completion of the task for machine *i* in job *j*-1 (i.e., the most recent task on the same machine), or

2. The completion of the task for machine *i*-1 in job *j* (i.e., the most recent task on the same job)

Due to the simple form of the problem, any permutation of jobs is a valid solution, and the optimal solution will correspond to *some* permutation. Thus, we search for improved solutions by changing the permutation of jobs and measuring the corresponding makespan. In what follows, we refer to a permutation of the jobs interchangeably as a *candidate*.


### Local Search
Local search is a strategy for solving optimization problems. Intuitively, it refers to movement from one solution that seems "pretty good" to another solution that looks even better. Because any permutation of jobs is a valid solution, we can view any mechanism that shuffles the jobs around as a local search procedure (this is in fact what we do below).

To use local search formally, we must answer a few questions:

1. What solution should we start with?
2. Given a solution, what are the neighbouring solutions that we should consider?
3. Given the set of candidate neighbours, which one should we consider next?

The following three sections address these questions in turn.


## General Solver
In this section we provide the general framework for the flow shop scheduler. Various aspects of the standard Python libraries are used throughout, and along with the required imports we also provide the settings for the solver at the top:

````python
import sys, os, time, random

from functools import partial
from collections import namedtuple
from itertools import product

import neighbourhood as neigh
import heuristics as heur

##############
## Settings ##
##############
TIME_LIMIT = 300.0 # Time (in seconds) to run the solver
TIME_INCREMENT = 13.0 # Time (in seconds) in between heuristic measurements
DEBUG_SWITCH = False # Displays intermediate heuristic info when True
MAX_LNS_NEIGHBOURHOODS = 1000 # Maximum number of neighbours to explore in LNS
````

There are two settings that should be explained further: (1) the TIME_INCREMENT setting will be used as part of the dynamic strategy selection and (2) the MAX_LNS_NEIGBOURHOODS setting will be used as part of the neighbourhood selection strategy. Both are described in more detail below.

These settings could be exposed to the user as command line parameters, but at this stage we instead focus on providing the input data as parameters to the program. The following code is used as the \_\_main\_\_ method for the solver file, and calls the appropriate functions based on the number of parameters input to the program:

````python
if __name__ == '__main__':

    if 2 == len(sys.argv):
        data = parse_problem(sys.argv[1], 0)
    elif 3 == len(sys.argv):
        data = parse_problem(sys.argv[1], int(sys.argv[2]))
    else:
        print "\nUsage: python flow.py <Taillard problem file> [<instance number>]\n"
        sys.exit(0)

    (perm, ms) = solve(data)
    print_solution(data, perm)
````

The input can be any one of the Taillard problem files (a standard in flow shop scheduling), and we describe the parsing shortly. The files can be found at the following URL:
* http://mistic.heig-vd.ch/taillard/problemes.dir/ordonnancement.dir/ordonnancement.html

The solve method expects the ```data``` variable to be a list containing the activity durations for each job -- one list of integers for each job will be in ```data```. The ```solve``` method starts by initializing a global set of strategies (to be described below). The key aspect to consider here, is that we use ```strat_*``` variables to maintain statistics on each of the strategies. This aids in selecting the strategy dynamically during the solving process.

````python
def solve(data):
    """Solves an instance of the flow shop scheduling problem"""

    # We initialize the strategies here to avoid cyclic import issues
    initialize_strategies()
    global STRATEGIES

    # Record the following for each strategy:
    #  improvements: The amount a solution was improved by this strategy
    #  time_spent: The amount of time spent on the strategy
    #  weights: The weights that correspond to how good a strategy is
    #  usage: The number of times we use a strategy
    strat_improvements = {strategy: 0 for strategy in STRATEGIES}
    strat_time_spent = {strategy: 0 for strategy in STRATEGIES}
    strat_weights = {strategy: 1 for strategy in STRATEGIES}
    strat_usage = {strategy: 0 for strategy in STRATEGIES}
```

One appealing feature of the flow shop scheduling problem is that *every* permutation is a valid solution. At least one will have the optimal makespan (though many permutations will have horrible makespans!), and every permutation is a solution. Thankfully, this allows us to forgo staying within the space of feasible solutions when going from one permutation to another (everything is feasible!).

However, to start a local search in the space of permutations, we must have an initial permutation to begin with. To keep things simple, we seed our local search by shuffling the list of jobs randomly:

```python
    # Start with a random permutation of the jobs
    perm = range(len(data))
    random.shuffle(perm)
```

Next, we initialize the variables that allow us to keep track of the best permutation found so far, as well as the timing information for providing informative output.

```python
    # Keep track of the best solution
    best_make = makespan(data, perm)
    best_perm = perm
    res = best_make

    # Maintain statistics and timing for the iterations
    iteration = 0
    time_limit = time.time() + TIME_LIMIT
    time_last_switch = time.time()

    time_delta = TIME_LIMIT / 10
    checkpoint = time.time() + time_delta
    percent_complete = 10

    print "\nSolving..."
```

As this is a local search solver, we simply continue to try and improve solutions as long as the time limit has not been reached. We further provide output indicating the progress of the solver and keep track of the number of iterations we have computed:

```python
    while time.time() < time_limit:

        if time.time() > checkpoint:
            print " %d %%" % percent_complete
            percent_complete += 10
            checkpoint += time_delta

        iteration += 1
```

Below we describe how the strategy is picked, but for now it is sufficient to know that the strategy provides a ```neighbourhood``` function and a ```heuristic``` function. The former gives us a set of *next candidates* to consider while the latter chooses the *best candidate* from the set. From these functions, we have a new permutation (```perm```) and a new makespan result (```res```):

```python
        # Heuristically choose the best strategy
        strategy = pick_strategy(STRATEGIES, strat_weights)

        old_val = res
        old_time = time.time()

        # Use the current strategy's heuristic to pick the next permutation from
        #  the set of candidates generated by the strategy's neighbourhood
        candidates = strategy.neighbourhood(data, perm)
        perm = strategy.heuristic(data, candidates)
        res = makespan(data, perm)
```

To help select a strategy, we keep statistics on (1) how much the strategy has improved the solution, (2) how much time the strategy has spent computing information, and (3) how many times the strategy was used. We also update the variables for the best permutation if we stumble upon a better solution:

```python
        # Record the statistics on how the strategy did
        strat_improvements[strategy] += res - old_val
        strat_time_spent[strategy] += time.time() - old_time
        strat_usage[strategy] += 1

        if res < best_make:
            best_make = res
            best_perm = perm[:]
```

At regular intervals, the statistics for strategy use are updated. We removed the associated snippet for readability, and detail the code below. As a final step, once the while-loop is complete (i.e., the time limit is reached) we output some statistics about the solving process and return the best permutation (along with its makespan):

```python
    print " %d %%\n" % percent_complete
    print "\nWent through %d iterations." % iteration

    print "\n(usage) Strategy:"
    results = sorted([(strat_weights[STRATEGIES[i]], i)
                      for i in range(len(STRATEGIES))], reverse=True)
    for (w, i) in results:
        print "(%d) \t%s" % (strat_usage[STRATEGIES[i]], STRATEGIES[i].name)

    return (best_perm, best_make)
````



### Parsing Problems
As input to the parsing procedure, we provide the file name where the input can be found and the example number that should be used from the file (each file contains a number of instances).

```python
def parse_problem(filename, k=1):
    """Parse the kth instance of a Taillard problem file

    The Taillard problem files are a standard benchmark set for the problem
    of flow shop scheduling. They can be found online at the following address:
    - http://mistic.heig-vd.ch/taillard/problemes.dir/ordonnancement.dir/ordonnancement.html"""

    print "\nParsing..."
```

We start the parsing by reading in the file and identifying the line that separates each of the problem instances:

```python
    with open(filename, 'r') as f:
        # Identify the string that separates instances
        problem_line = '/number of jobs, number of machines, initial seed, upper bound and lower bound :/'

        # Strip spaces and newline characters from every line
        lines = map(str.strip, f.readlines())
```

To make locating the correct instance easier, we assume that every line will be separated by a '/' character. This allows us to split the file based on a common string that appears at the top of every instance, and adding a '/' character to the start of the first line allows the string processing below to work correctly regardless of the instance we choose. We also detect when an instance number is provided that is out of range given the collection of instances found in the file.

```python
        # We prep the first line for later
        lines[0] = '/' + lines[0]

        # We also know '/' does not appear in the files, so we can use it as
        #  a separator to find the right lines for the kth problem instance
        try:
            lines = '/'.join(lines).split(problem_line)[k].split('/')[2:]
        except IndexError:
            max_instances = len('/'.join(lines).split(problem_line)) - 1
            print "\nError: Instance must be within 1 and %d\n" % max_instances
            sys.exit(0)
```

Finally, we parse the data directly -- converting each of the tasks processing times to an integer and storing them in a list. As a final step, we zip the data to invert the rows and columns so that the format respects what is expected by the solving code above (i.e., every item in data should correspond to a particular job).

```python
        # Split every line based on spaces and convert each item to an int
        data = [map(int, line.split()) for line in lines]

    # We return the zipped data to rotate the rows and columns, making each
    #  item in data the durations of tasks for a particular job
    return zip(*data)
```


### Compiling Solutions

```python
def compile_solution(data, perm):
    """Compiles a scheduling on the machines given a permutation of jobs"""

    num_machines = len(data[0])

    # Note that using [[]] * range(k) would be incorrect, as it would simply
    #  copy the same list k times (as opposed to creating k distinct lists).
    machine_times = [[] for _ in range(num_machines)]

    # Assign the initial job to the machines
    machine_times[0].append(0)
    for mach in range(1,num_machines):
        # Start the next task in the job when the previous finishes
        machine_times[mach].append(machine_times[mach-1][0] +
                                   data[perm[0]][mach-1])

    # Assign the remaining jobs
    for i in range(1, len(perm)):

        # The first machine never contains any idle time
        job = perm[i]
        machine_times[0].append(machine_times[0][-1] + data[perm[i-1]][0])

        # For the remaining machines, the start time is the max of when the
        #  previous task in the job completed, or when the current machine
        #  completes the task for the previous job.
        for mach in range(1, num_machines):
            machine_times[mach].append(max(machine_times[mach-1][i] + data[perm[i]][mach-1],
                                        machine_times[mach][i-1] + data[perm[i-1]][mach]))

    return machine_times
```

### Printing Solutions

```python
def print_solution(data, perm):
    """Prints statistics on the computed solution"""

    sol = compile_solution(data, perm)

    print "\nPermutation: %s\n" % str([i+1 for i in perm])

    print "Makespan: %d\n" % makespan(data, perm)

    row_format ="{:>15}" * 4
    print row_format.format('Machine', 'Start Time', 'Finish Time', 'Idle Time')
    for mach in range(len(data[0])):
        finish_time = sol[mach][-1] + data[perm[-1]][mach]
        idle_time = (finish_time - sol[mach][0]) - sum([job[mach] for job in data])
        print row_format.format(mach+1, sol[mach][0], finish_time, idle_time)

    results = []
    for i in range(len(data)):
        finish_time = sol[-1][i] + data[perm[i]][-1]
        idle_time = (finish_time - sol[0][i]) - sum([time for time in data[perm[i]]])
        results.append((perm[i]+1, sol[0][i], finish_time, idle_time))

    print "\n"
    print row_format.format('Job', 'Start Time', 'Finish Time', 'Idle Time')
    for r in sorted(results):
        print row_format.format(*r)

    print "\n\nNote: Idle time does not include initial or final wait time.\n"
```


## Neighbourhoods

````python
def neighbours_random(data, perm, num = 1):
    # Returns <num> random job permutations, including the current one
    candidates = [perm]
    for i in range(num):
        candidate = perm[:]
        random.shuffle(candidate)
        candidates.append(candidate)
    return candidates
````

````python
def neighbours_swap(data, perm):
    # Returns the permutations corresponding to swapping every pair of jobs
    candidates = [perm]
    for (i,j) in combinations(range(len(perm)), 2):
        candidate = perm[:]
        candidate[i], candidate[j] = candidate[j], candidate[i]
        candidates.append(candidate)
    return candidates
````

````python
def neighbours_idle(data, perm, size=4):
    # Returns the permutations of the most <size> idle jobs
    candidates = [perm]

    # Compute the idle time for each job
    sol = flow.compile_solution(data, perm)
    results = []

    for i in range(len(data)):
        finish_time = sol[-1][i] + data[perm[i]][-1]
        idle_time = (finish_time - sol[0][i]) - sum([time for time in data[perm[i]]])
        results.append((idle_time, perm[i]))

    # Take the <size> most idle jobs
    subset = [job for (idle, job) in list(reversed(results))[:size]]

    # Enumerate the permutations of the idle jobs
    for ordering in permutations(subset):
        candidate = perm[:]
        for i in range(len(ordering)):
            candidate[subset[i]] = perm[ordering[i]]
        candidates.append(candidate)

    return candidates
````

````python
def neighbours_LNS(data, perm, size = 2):
    # Returns the Large Neighbourhood Search neighbours
    candidates = [perm]

    # Bound the number of neighbourhoods in case there are too many jobs
    neighbourhoods = list(combinations(range(len(perm)), size))
    random.shuffle(neighbourhoods)

    for subset in neighbourhoods[:flow.MAX_LNS_NEIGHBOURHOODS]:

        # Keep track of the best candidate for each neighbourhood
        best_make = flow.makespan(data, perm)
        best_perm = perm

        # Enumerate every permutation of the selected neighbourhood
        for ordering in permutations(subset):
            candidate = perm[:]
            for i in range(len(ordering)):
                candidate[subset[i]] = perm[ordering[i]]
            res = flow.makespan(data, candidate)
            if res < best_make:
                best_make = res
                best_perm = candidate

        # Record the best candidate as part of the larger neighbourhood
        candidates.append(best_perm)

    return candidates
````

## Heuristics

A heuristic returns a single candidate permutation from a set of provided candidates. The heuristic is also given access to the problem data in order to evaluate which candidate might be preferred.

The first heuristic we consider is **heur_random**. This heuristic randomly selects a candidate from the list of candidates without evaluating which one might be preferred:

````python
def heur_random(data, candidates):
    # Returns a random candidate choice
    return random.choice(candidates)
````

The next heuristic uses the other extreme. Rather than randomly selecting a candidate, **heur_hillclimbing** selects the candidate that has the best makespan. Note that the list ```scores``` will contain tuples of the form *(m,p)* where *m* is the makespan value for permutation *p*. Sorting such a list will place the tuple with the best makespan at the start of the list, from which we return the permutation.

````python
def heur_hillclimbing(data, candidates):
    # Returns the best candidate in the list
    scores = [(flow.makespan(data, perm), perm) for perm in candidates]
    return sorted(scores)[0][1]
````

The final heuristic, **heur_random_hillclimbing**, combines both the random and hillclimbing heuristics above. When performing local search, you may not want to always choose a random candidate, or even the best one. The **heur_random_hillclimbing** heuristic returns a "pretty good" solution by choosing the best candidate with probability 0.5, then the second best with probability 0.25, and so on. The while-loop essentially flips a coin at every iteration to see if it should continue increasing the index (with a limit on the size of the list). The final index chosen corresponds to the candidate that the heuristic selects.

````python
def heur_random_hillclimbing(data, candidates):
    # Returns a candidate with probability proportional to its rank in sorted quality
    scores = [(flow.makespan(data, perm), perm) for perm in candidates]
    i = 0
    while (random.random() < 0.5) and (i < len(scores) - 1):
        i += 1
    return sorted(scores)[i][1]
````

## Dynamic Strategy Selection

```python

        # At regular intervals, switch the weighting on the strategies available.
        #  This way, the search can dynamically shift towards strategies that have
        #  proven more effective recently.
        if time.time() > time_last_switch + TIME_INCREMENT:

            # Normalize the improvements made by the time it takes to make them
            results = sorted([(float(strat_improvements[s]) / max(0.001, strat_time_spent[s]), s)
                              for s in STRATEGIES])

            if DEBUG_SWITCH:
                print "\nComputing another switch..."
                print "Best performer: %s (%d)" % (results[0][1].name, results[0][0])
                print "Worst performer: %s (%d)" % (results[-1][1].name, results[-1][0])

            # Boost the weight for the successful strategies
            for i in range(len(STRATEGIES)):
                strat_weights[results[i][1]] += len(STRATEGIES) - i

                # Additionally boost the unused strategies to avoid starvation
                if 0 == results[i][0]:
                    strat_weights[results[i][1]] += len(STRATEGIES)

            time_last_switch = time.time()

            if DEBUG_SWITCH:
                print results
                print sorted([strat_weights[STRATEGIES[i]] for i in range(len(STRATEGIES))])

            strat_improvements = {strategy: 0 for strategy in STRATEGIES}
            strat_time_spent = {strategy: 0 for strategy in STRATEGIES}
```

## Discussion

