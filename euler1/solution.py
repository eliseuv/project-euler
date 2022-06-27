#!/usr/bin/env python3

import time

# Loop solution
def solution_loop(N, a, b):
    S = 0
    for k in range(1, N):
        if k % a == 0 or k % b == 0:
            S += k
    return S

# Dani solution
def solution_dani(N, a, b):
    mult_a = sum([a * x for x in range(1, (N-1)//a + 1)])
    mult_b = sum([b * x for x in range(1, (N-1)//b + 1)])
    mult_ab = sum([(a*b)*x for x in range(1, (N-1)//(a*b) + 1)])
    return mult_a + mult_b - mult_ab

# General solution
def solution_general(N, a, b):
    return sum([x for x in range(1, N) if x % a == 0 or x % b == 0])

def main():
    N = 1000
    a = 3
    b = 5

    # Loop solution
    start_time = time.time()
    S = solution_loop(N, a, b)
    finish_time = time.time()
    print("--- Loop solution = %s in %s seconds ---" % (S, (finish_time - start_time)))

    # Dani solution
    start_time = time.time()
    S = solution_dani(N, a, b)
    finish_time = time.time()
    print("--- Dani solution = %s in %s seconds ---" % (S, (finish_time - start_time)))

    # General solution
    start_time = time.time()
    S = solution_general(N, a, b)
    finish_time = time.time()
    print("--- General solution = %s in %s seconds ---" % (S, (finish_time - start_time)))

if __name__ == "__main__":
    main()
