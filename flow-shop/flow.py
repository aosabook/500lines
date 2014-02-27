import sys, os, time, random



################
## Heuristics ##
################

def _heur_random(perm):
    random.shuffle(perm)
    return perm




def parse_problem(filename):
    
    with open(filename, 'r') as f:
        problem_line = 'number of jobs, number of machines, initial seed, upper bound and lower bound :'
        lines = map(str.strip, f.readlines())
        lines = lines[3:lines.index(problem_line, 1)]
        data = map(lambda x: map(int, map(str.strip, x.split())), lines)
    
    return zip(*data)


def makespan(data, njobs, nmach, perm):
    
    mach_time = [0] * nmach
    
    for job in perm:
        mach_time[0] += data[job][0]
        for mach in range(1, nmach):
            mach_time[mach] = max(mach_time[mach-1], mach_time[mach]) + data[job][mach]
    
    return mach_time[-1]


def solve(data, heuristic = _heur_random):
    njobs = len(data)
    nmach = len(data[0])
    perm = range(njobs)
    
    best_make = makespan(data, njobs, nmach, perm)
    best_perm = perm
    
    time_limit = time.time() + 10
    while time.time() < time_limit:
        
        perm = heuristic(perm)
        res = makespan(data, njobs, nmach, perm)
        
        if res < best_make:
            best_make = res
            best_perm = perm[:]

    print (best_perm, best_make)


def print_solution(data, sol):
    pass






if __name__ == '__main__':
    data = parse_problem(sys.argv[1])
    sol = solve(data)
    print_solution(data, sol)
