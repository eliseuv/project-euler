#!/usr/bin/env python3

import sys
sys.path.append('..')

import numpy as np

import methods as bib

DIGITS = '7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450'

# Number of digits
L = 13

# Split digits sequence into subsequences separated by zero
# Ex: '12034506780090' => ['12', '345', '678', '', '9', '']
subsequences = DIGITS.split('0')

# Store the maximum product
prod_max = 0
ss_arr_max = np.array([])

# Loop on all subsequences
for ss in subsequences:
    # Ignore subsequences smaller than L
    if len(ss) < L:
        continue
    # Calculate product of the first L digits in the subsequence
    prod = np.prod(np.fromiter(ss[:L], dtype=int))
    # Store if maximum product so far
    if prod > prod_max:
        ss_arr_max = np.fromiter(ss[:L], dtype=int)
        prod_max = prod
    # Continue on this subsequence in some kind of `moving product`
    for k in range(1, len(ss) - L + 1):
        # Divide by digit on the front of mask...
        prod //= int(ss[k-1])
        # And multiply by digit on the back of the mask
        prod *= int(ss[L+k-1])
        # Store if maximum product so far
        if prod > prod_max:
            ss_arr_max = np.fromiter(ss[k:L+k], dtype=int)
            prod_max = prod

print('Maximum:\n{} => {}'.format(ss_arr_max, prod_max))