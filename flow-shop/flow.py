import sys, os, time, random

from functools import partial
from itertools import combinations, permutations, product

##############
## Settings ##
##############
TIME_LIMIT = 300.0 # Time (in seconds) to run the solver
TIME_INCREMENT = 13.0 # Time (in seconds) in between heuristic measurements
DEBUG_SWITCH = False # Displays intermediate heuristic info when True
MAX_LNS_NEIGHBOURHOODS = 1000 # Maximum number of neighbours to explore in LNS


##############################
## Neighbourhood Generators ##
##############################

def _neighbours_random(data, perm, num = 1):
    # Returns <num> random job permutations, including the current one
    candidates = [perm]
    for i in range(num):
        candidate = perm[:]
        random.shuffle(candidate)
        candidates.append(candidate)
    return candidates

def _neighbours_swap(data, perm):
    # Returns the permutations corresponding to swapping every pair of jobs
    candidates = [perm]
    for (i,j) in combinations(range(len(perm)), 2):
        candidate = perm[:]
        candidate[i], candidate[j] = candidate[j], candidate[i]
        candidates.append(candidate)
    return candidates

def _neighbours_LNS(data, perm, size = 2):
    # Returns the Large Neighbourhood Search neighbours
    candidates = [perm]

    # Bound the number of neighbourhoods in case there are too many jobs
    neighbourhoods = list(combinations(range(len(perm)), size))
    random.shuffle(neighbourhoods)

    for subset in neighbourhoods[:MAX_LNS_NEIGHBOURHOODS]:

        # Keep track of the best candidate for each neighbourhood
        best_make = makespan(data, perm)
        best_perm = perm

        # Enumerate every permutation of the selected neighbourhood
        for ordering in permutations(subset):
            candidate = perm[:]
            for i in range(len(ordering)):
                candidate[subset[i]] = perm[ordering[i]]
            res = makespan(data, candidate)
            if res < best_make:
                best_make = res
                best_perm = candidate

        # Record the best candidate as part of the larger neighbourhood
        candidates.append(best_perm)

    return candidates

def _neighbours_idle(data, perm, size=4):
    # Returns the permutations of the most <size> idle jobs
    candidates = [perm]

    # Compute the idle time for each job
    sol = compile_solution(data, perm)
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


################
## Heuristics ##
################

def _heur_hillclimbing(data, candidates):
    # Returns the best candidate in the list
    scores = [(makespan(data, perm), perm) for perm in candidates]
    return sorted(scores)[0][1]

def _heur_random(data, candidates):
    # Returns a random candidate choice
    return random.choice(candidates)

def _heur_random_hillclimbing(data, candidates):
    # Returns a candidate with probability proportional to its rank in sorted quality
    scores = sorted([(makespan(data, perm), perm) for perm in candidates])
    i = 0
    while (random.random() < 0.5) and (i < len(scores) - 1):
        i += 1
    return scores[i][1]


################################

# Define the neighbourhoods (and parameters) we would like to investigate
NEIGHBOURHOODS = [
    ('Random Permutation', partial(_neighbours_random, num=100)),
    ('Swapped Pairs', _neighbours_swap),
    ('Large Neighbourhood Search (2)', partial(_neighbours_LNS, size=2)),
    ('Large Neighbourhood Search (3)', partial(_neighbours_LNS, size=3)),
    ('Idle Neighbourhood (3)', partial(_neighbours_idle, size=3)),
    ('Idle Neighbourhood (4)', partial(_neighbours_idle, size=4)),
    ('Idle Neighbourhood (5)', partial(_neighbours_idle, size=5))
]

# Define the heuristics we would like to investigate
HEURISTICS = [
    ('Hill Climbing', _heur_hillclimbing),
    ('Random Selection', _heur_random),
    ('Biased Random Selection', _heur_random_hillclimbing)
]

# Combine every neighbourhood and heuristic strategy
STRATEGIES = []
for (n, h) in product(NEIGHBOURHOODS, HEURISTICS):
    STRATEGIES.append({'name': "%s / %s" % (n[0], h[0]), # Unique name
                       'neigh': n[1], # Neighbourhood function
                       'heur': h[1], # Heuristic function
                       'weight': 1, # Weight to determine chance of usage
                       'usage': 0}) # Number of times the strategy is used


def _pick_strategy(strategies):
    # Picks a random strategy based on its weight: roulette wheel selection
    total = sum([strat['weight'] for strat in strategies])
    pick = random.uniform(0, total)
    count = strategies[0]['weight']

    i = 0
    while pick > count:
        count += strategies[i+1]['weight']
        i += 1

    return (strategies[i],i)


def parse_problem(filename):
    """Parse the first instance of a Taillard problem file"""

    print "\nParsing..."

    with open(filename, 'r') as f:
        problem_line = 'number of jobs, number of machines, initial seed, upper bound and lower bound :'
        lines = map(str.strip, f.readlines())
        lines = lines[3:lines.index(problem_line, 1)]
        data = map(lambda x: map(int, map(str.strip, x.split())), lines)

    return zip(*data)


def makespan(data, perm):
    """Computes the makespan of the provided solution"""
    return compile_solution(data, perm)[-1][-1] + data[perm[-1]][-1]


def compile_solution(data, perm):
    """Compiles a scheduling on the machines given a permutation of jobs"""

    nmach = len(data[0])

    mach_times = [[] for i in range(nmach)]

    # Assign the initial job to the machines
    mach_times[0].append(0)
    for mach in range(1,nmach):
        # Start the next task in the job when the previous finishes
        mach_times[mach].append(mach_times[mach-1][0] + data[perm[0]][mach-1])

    # Assign the remaining jobs
    for i in range(1, len(perm)):

        # The first machine never contains any idle time
        job = perm[i]
        mach_times[0].append(mach_times[0][-1] + data[perm[i-1]][0])

        # For the remaining machines, the start time is the max of when the
        #  previous task in the job completed, or when the current machine
        #  completes the task for the previous job.
        for mach in range(1, nmach):
            mach_times[mach].append(max(mach_times[mach-1][i] + data[perm[i]][mach-1],
                                        mach_times[mach][i-1] + data[perm[i-1]][mach]))

    return mach_times


def solve(data):
    """Solves an instance of the flow shop scheduling problem"""
    global STRATEGIES

    # Record the improvements made by each heuristic and the time they are used
    improvements = [0] * len(STRATEGIES)
    time_spent = [0] * len(STRATEGIES)

    # Start with a random permutation of the jobs
    perm = range(len(data))
    random.shuffle(perm)

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

    while time.time() < time_limit:

        if time.time() > checkpoint:
            print " %d %%" % percent_complete
            percent_complete += 10
            checkpoint += time_delta

        iteration += 1

        # Heuristically choose the best strategy
        (s,i) = _pick_strategy(STRATEGIES)

        # Use the strategy to change the solution
        old_val = res
        old_time = time.time()
        perm = s['heur'](data, s['neigh'](data, perm))
        res = makespan(data, perm)

        # Record the statistics on how the strategy did
        improvements[i] += res - old_val
        time_spent[i] += time.time() - old_time
        STRATEGIES[i]['usage'] += 1

        if res < best_make:
            best_make = res
            best_perm = perm[:]

        # At regular intervals, switch the weighting on the strategies available
        if time.time() > time_last_switch + TIME_INCREMENT:

            # Normalize the improvements made by the time it takes to make them
            results = sorted([(float(improvements[i]) / max(0.001, time_spent[i]), i) for i in range(len(STRATEGIES))])

            if DEBUG_SWITCH:
                print "\nComputing another switch..."
                print "Best performer: %s (%d)" % (STRATEGIES[results[0][1]]['name'], results[0][0])
                print "Worst performer: %s (%d)" % (STRATEGIES[results[-1][1]]['name'], results[-1][0])

            # Boost the weight for the successful strategies
            for i in range(len(STRATEGIES)):
                STRATEGIES[results[i][1]]['weight'] += len(STRATEGIES) - i

                # Additionally boost the unused strategies to avoid starvation
                if 0 == results[i][0]:
                    STRATEGIES[results[i][1]]['weight'] += len(STRATEGIES)

            time_last_switch = time.time()

            if DEBUG_SWITCH:
                print results
                print sorted([STRATEGIES[i]['weight'] for i in range(len(STRATEGIES))])

            improvements = [0] * len(STRATEGIES)
            time_spent = [0] * len(STRATEGIES)


    print " %d %%\n" % percent_complete
    print "\nWent through %d iterations." % iteration

    print "\n(usage) Strategy:"
    results = sorted([(STRATEGIES[i]['weight'], i) for i in range(len(STRATEGIES))], reverse=True)
    for (w, i) in results:
        print "(%d) \t%s" % (STRATEGIES[i]['usage'], STRATEGIES[i]['name'])

    return (best_perm, best_make)


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


if __name__ == '__main__':
    data = parse_problem(sys.argv[1])
    (perm, ms) = solve(data)
    print_solution(data, perm)
