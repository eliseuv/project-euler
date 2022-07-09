#!/usr/bin/env python3

import timeit
import numpy as np

# Naive solution
def solution_naive(N, a, b):
    S = 0
    for k in range(1, N):
        if k % a == 0 or k % b == 0:
            S += k
    return S

# General solution
def solution_general(N, a, b):
    return sum([x for x in range(1, N) if x % a == 0 or x % b == 0])

# General solution using numpy arrays
def solution_general_np(N, xs):
    return np.sum(np.arange(1, N)[np.hstack([np.any(np.equal(np.mod(n, xs), 0)) for n in np.arange(1, N)])])

# Sum of all multiples of `x` below `N`
def mult_sum(x, N):
    return sum([k*x for k in range(1, (N-1)//x + 1)])

# Dani solution
def solution_dani(N, a, b):
    return mult_sum(a, N) + mult_sum(b, N) - mult_sum(a*b, N)

# Sum of an Arithmetic Progression
def AP_sum(a_1, r, n):
    a_n = a_1 + (n-1) * r
    return (n * (a_1 + a_n)) // 2

# Sum of all multiples of `x` below `N` using Arithmetic Progression
def mult_sum_AP(x, N):
    return AP_sum(x, x, (N-1)//x)

# Solution using sum of Arithmetic Progression
def solution_AP_sum(N, a, b):
    return mult_sum_AP(a, N) + mult_sum_AP(b, N) - mult_sum_AP(a*b, N)

# Parameters to use
N = 1_000
a = 3
b = 5

def main():

    # Parameters to timing
    trials = 1000
    kwargs = {'setup': 'from __main__ import N, a, b', 'globals': globals(), 'number': trials}

    print("--- Euler 1: Sum of all the numbers multiple of %s or %s below %s (Time average over %s trials) ---" %(a, b, N, trials))

    # Loop solution
    print("Naive solution = %s (%s seconds)" % (solution_naive(N, a, b), timeit.timeit("solution_naive(N, a, b)",  **kwargs)/trials))

    # General solution
    print("General solution = %s (%s seconds)" % (solution_general(N, a, b), timeit.timeit("solution_general(N, a, b)", **kwargs)/trials))

    # General solution
    print("General solution NumPy = %s (%s seconds)" % (solution_general_np(N, np.array([a,b])), timeit.timeit("solution_general_np(N, np.array([a,b]))", **kwargs)/trials))

    # Dani solution
    print("Dani solution = %s (%s seconds)" % (solution_dani(N, a, b), timeit.timeit("solution_dani(N, a, b)", **kwargs)/trials))

    # general solution numpy
    print("Sum of AP solution = %s (%s seconds)" % (solution_AP_sum(N, a, b), timeit.timeit("solution_AP_sum(N, a, b)", **kwargs)/trials))

if __name__ == "__main__":
    main()
