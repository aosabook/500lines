
import random

import flow

################
## Heuristics ##
################

################################################################
## A heuristic returns a single candidate permutation from
##  a set of candidates that is given. The heuristic is also
##  given access to the problem data in order to evaluate
##  which candidate might be preferred.

def heur_hillclimbing(data, candidates):
    # Returns the best candidate in the list
    scores = [(flow.makespan(data, perm), perm) for perm in candidates]
    return sorted(scores)[0][1]

def heur_random(data, candidates):
    # Returns a random candidate choice
    return random.choice(candidates)

def heur_random_hillclimbing(data, candidates):
    # Returns a candidate with probability proportional to its rank in sorted quality
    scores = [(flow.makespan(data, perm), perm) for perm in candidates]
    i = 0
    while (random.random() < 0.5) and (i < len(scores) - 1):
        i += 1
    return sorted(scores)[i][1]
