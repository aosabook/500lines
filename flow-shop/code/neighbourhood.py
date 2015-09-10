
import random
from itertools import combinations, permutations

import flow

##############################
## Neighbourhood Generators ##
##############################

def neighbours_random(data, perm, num = 1):
    # Returns <num> random job permutations, including the current one
    candidates = [perm]
    for i in range(num):
        candidate = perm[:]
        random.shuffle(candidate)
        candidates.append(candidate)
    return candidates

def neighbours_swap(data, perm):
    # Returns the permutations corresponding to swapping every pair of jobs
    candidates = [perm]
    for (i,j) in combinations(range(len(perm)), 2):
        candidate = perm[:]
        candidate[i], candidate[j] = candidate[j], candidate[i]
        candidates.append(candidate)
    return candidates

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

def neighbours_idle(data, perm, size=4):
    # Returns the permutations of the most <size> idle jobs
    candidates = [perm]

    # Compute the idle time for each job
    sol = flow.compile_solution(data, perm)
    results = []

    for i in range(len(data)):
        finish_time = sol[-1][i] + data[perm[i]][-1]
        idle_time = (finish_time - sol[0][i]) - sum([t for t in data[perm[i]]])
        results.append((idle_time, i))

    # Take the <size> most idle jobs
    subset = [job_ind for (idle, job_ind) in reversed(sorted(results))][:size]

    # Enumerate the permutations of the idle jobs
    for ordering in permutations(subset):
        candidate = perm[:]
        for i in range(len(ordering)):
            candidate[subset[i]] = perm[ordering[i]]
        candidates.append(candidate)

    return candidates
