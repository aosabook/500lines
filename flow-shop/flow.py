import sys, os, time, random

from functools import partial

##############
## Settings ##
##############
TIME_LIMIT = 10


##############################
## Neighbourhood Generators ##
##############################

def _neighbours_random(perm, num = 1):
    candidates = [perm]
    for i in range(num):
        candidate = perm[:]
        random.shuffle(candidate)
        candidates.append(candidate)
    return candidates

def _neighbours_swap(perm):
    candidates = []
    for i in range(len(perm) - 1):
        for j in range(1, len(perm) - 1):
            candidate = perm[:]
            candidate[i], candidate[j] = candidate[j], candidate[i]
            candidates.append(candidate)
    return candidates

def _neighbours_LNS(perm):
    # TODO: Implement Large Neighbourhood Search
    return [perm]



################
## Heuristics ##
################

def _heur_hillclimbing(data, candidates, context):
    scores = [(makespan(data, perm), perm) for perm in candidates]
    return sorted(scores)[0][1]

def _heur_random(data, candidates, context):
    return random.choice(candidates)


################################


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

    context = {}

    neighbourhood = partial(_neighbours_random, num=50)
    #neighbourhood = _neighbours_swap

    heuristic = _heur_random
    #heuristic = _heur_hillclimbing

    perm = range(len(data))
    
    best_make = makespan(data, perm)
    best_perm = perm
    
    time_limit = time.time() + TIME_LIMIT
    while time.time() < time_limit:
        
        perm = heuristic(data, neighbourhood(perm), context)
        res = makespan(data, perm)
        
        if res < best_make:
            best_make = res
            best_perm = perm[:]

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
