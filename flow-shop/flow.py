import sys, os, time, random

from functools import partial
from itertools import combinations, permutations

##############
## Settings ##
##############
TIME_LIMIT = 30
TIME_INCREMENT = 5
DEBUG_SWITCH = True

##############################
## Neighbourhood Generators ##
##############################

def _neighbours_random(data, perm, num = 1):
    candidates = [perm]
    for i in range(num):
        candidate = perm[:]
        random.shuffle(candidate)
        candidates.append(candidate)
    return candidates

def _neighbours_swap(data, perm):
    candidates = [perm]
    for (i,j) in combinations(range(len(perm)), 2):
            candidate = perm[:]
            candidate[i], candidate[j] = candidate[j], candidate[i]
            candidates.append(candidate)
    return candidates

def _neighbours_LNS(data, perm, size = 2):
    candidates = []
    for subset in combinations(range(len(perm)), size):
        best_make = makespan(data, perm)
        best_perm = perm
        for ordering in permutations(subset):
            candidate = perm[:]
            for i in range(len(ordering)):
                candidate[subset[i]] = perm[ordering[i]]
            res = makespan(data, candidate)
            if res < best_make:
                best_make = res
                best_perm = candidate
        candidates.append(best_perm)
    return candidates



################
## Heuristics ##
################

def _heur_hillclimbing(data, candidates):
    scores = [(makespan(data, perm), perm) for perm in candidates]
    return sorted(scores)[0][1]

def _heur_random(data, candidates):
    return random.choice(candidates)

def _heur_random_hillclimbing(data, candidates):
    scores = sorted([(makespan(data, perm), perm) for perm in candidates])
    i = 0
    while (random.random() < 0.5) and (i < len(scores)):
        i += 1
    return scores[i][1]


################################


STRATEGIES = [
    {
        'name': 'Random search.',
        'heur': _heur_random,
        'neigh': partial(_neighbours_random, num=1),
        'weight': 1
    },
    {
        'name': 'Random neighbours, randomly biased selection.',
        'heur': _heur_random_hillclimbing,
        'neigh': partial(_neighbours_random, num=100),
        'weight': 1
    },
    {
        'name': 'Random neighbours, hillclimbing selection.',
        'heur': _heur_hillclimbing,
        'neigh': partial(_neighbours_random, num=100),
        'weight': 1
    },
    {
        'name': 'Large Neighbourhood Search (size 2), hillclimbing selection.',
        'heur': _heur_hillclimbing,
        'neigh': partial(_neighbours_LNS, size=2),
        'weight': 1
    },
    {
        'name': 'Large Neighbourhood Search (size 3), hillclimbing selection.',
        'heur': _heur_hillclimbing,
        'neigh': partial(_neighbours_LNS, size=3),
        'weight': 1
    },
    {
        'name': 'Large Neighbourhood Search (size 2), randomly biased selection.',
        'heur': _heur_random_hillclimbing,
        'neigh': partial(_neighbours_LNS, size=2),
        'weight': 1
    },
    {
        'name': 'Large Neighbourhood Search (size 3), randomly biased selection.',
        'heur': _heur_random_hillclimbing,
        'neigh': partial(_neighbours_LNS, size=3),
        'weight': 1
    }
  ]


def _pick_strategy(strategies):
    total = sum([strat['weight'] for strat in strategies])
    pick = random.uniform(0, total)
    count = strategies[0]['weight']
    i = 0
    while pick > count:
        count += strategies[i+1]['weight']
        i += 1
    return (strategies[i],i)


def parse_problem(filename):

    with open(filename, 'r') as f:
        problem_line = 'number of jobs, number of machines, initial seed, upper bound and lower bound :'
        lines = map(str.strip, f.readlines())
        lines = lines[3:lines.index(problem_line, 1)]
        data = map(lambda x: map(int, map(str.strip, x.split())), lines)

    return zip(*data)


def makespan(data, perm):
    return compile_solution(data, perm)[-1][-1] + data[perm[-1]][-1]


def compile_solution(data, perm):

    nmach = len(data[0])

    mach_times = [[] for i in range(nmach)]

    # Assign the initial job to the machines
    mach_times[0].append(0)
    for mach in range(1,nmach):
        mach_times[mach].append(mach_times[mach-1][0] + data[perm[0]][mach-1])

    # Assign the remaining jobs
    for i in range(1, len(perm)):
        job = perm[i]
        mach_times[0].append(mach_times[0][-1] + data[perm[i-1]][0])
        for mach in range(1, nmach):
            mach_times[mach].append(max(mach_times[mach-1][i] + data[perm[i]][mach-1],
                                        mach_times[mach][i-1] + data[perm[i-1]][mach]))

    return mach_times


def solve(data):

    global STRATEGIES

    improvements = [0] * len(STRATEGIES)

    perm = range(len(data))

    best_make = makespan(data, perm)
    best_perm = perm
    res = best_make

    iteration = 0
    time_limit = time.time() + TIME_LIMIT
    time_last_switch = time.time()

    while time.time() < time_limit:

        iteration += 1

        (s,i) = _pick_strategy(STRATEGIES)

        old_val = res
        perm = s['heur'](data, s['neigh'](data, perm))
        res = makespan(data, perm)

        improvements[i] += res - old_val

        if time.time() > time_last_switch + TIME_INCREMENT:
            results = sorted([(improvements[i], i) for i in range(len(STRATEGIES))])

            if DEBUG_SWITCH:
                print "\nComputing another switch..."
                print "Best performer: %s (%d)" % (STRATEGIES[results[0][1]]['name'], results[0][0])
                print "Worst performer: %s (%d)" % (STRATEGIES[results[-1][1]]['name'], results[-1][0])

            for i in range(len(STRATEGIES)):
                STRATEGIES[results[i][1]]['weight'] += len(STRATEGIES) - i
            time_last_switch = time.time()

            print results
            print sorted([STRATEGIES[i]['weight'] for i in range(len(STRATEGIES))])

            improvements = [0] * len(STRATEGIES)


        if res < best_make:
            best_make = res
            best_perm = perm[:]



    print "\nWent through %d iterations." % iteration

    return (best_perm, best_make)


def print_solution(data, perm):

    sol = compile_solution(data, perm)

    print "\nPermutation: %s\n" % str([i+1 for i in perm])

    print "Makespan: %d\n" % makespan(data, perm)

    row_format ="{:>15}" * 4
    print row_format.format('Machine', 'Start Time', 'Finish Time', 'Idle Time')
    for mach in range(len(data[0])):
        finish_time = sol[mach][-1] + data[perm[-1]][mach]
        idle_time = (finish_time - sol[mach][0]) - sum([job[mach] for job in data])
        print row_format.format(mach+1, sol[mach][0], finish_time, idle_time)

    print "\n"
    print row_format.format('Job', 'Start Time', 'Finish Time', 'Idle Time')
    results = []
    for i in range(len(data)):
        finish_time = sol[-1][i] + data[perm[i]][-1]
        idle_time = (finish_time - sol[0][i]) - sum([time for time in data[perm[i]]])
        results.append((perm[i]+1, sol[0][i], finish_time, idle_time))

    for r in sorted(results):
        print row_format.format(*r)


    print "\n\nNote: Idle time does not include initial or final wait time.\n"




if __name__ == '__main__':
    data = parse_problem(sys.argv[1])
    (perm, ms) = solve(data)
    print_solution(data, perm)
