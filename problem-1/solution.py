#!/usr/bin/env python3

N = 1000

S = 0
for k in range(1, N):
    if k % 3 == 0 or k % 5 == k:
        S += k

print(S)
