import numpy as np

import sys
sys.path.append("..")
from methods import *

# Set of numbers to test functions
TEST_SET = np.array([1, 5, 10, 11, 13, 232, 233, 423, 424, 1000, 1001, 998001])

def largest_palindrome_below(n):
  # Convert to PTR
  n_ptr = to_ptr(n)
  # Even number of digits
  if n_ptr[1] == '':
    # If LHS < RHS
    if int(n_ptr[0][::-1]) < int(n_ptr[2]):
      # RHS = mirror of LHS
      return from_ptr((n_ptr[0], '', n_ptr[0][::-1]))
    # If LHS >= RHS
    else:
      # If LHS is a power of 10
      if n_ptr[0][0] == '1' and all(digit == '0' for digit in n_ptr[0][1:]):
        # 10^n - 1 = bunch of nines
        nines = str(int(n_ptr[0])-1)
        # If 10^0 = 1 => 10 - 1 = 9
        if nines == '0':
          return 9
        else:
          return from_ptr((nines, '9', nines))
      # Else RHS = mirror of (LHS - 1)
      else:
        p0_str = str(int(n_ptr[0])-1)
        return from_ptr((p0_str, '', p0_str[::-1]))
  # Odd number of digits, but longer than one digit
  elif n_ptr[0] != '':
    # If LHS < RHS
    if int(n_ptr[0][::-1]) < int(n_ptr[2]):
      # RHS = mirror of LHS
      return from_ptr((n_ptr[0], n_ptr[1], n_ptr[0][::-1]))
    # LHS >= RHS
    else:
      # If middle digit is '0'
      if n_ptr[1] == '0':
        # If LHS is a power of 10
        if n_ptr[0][0] == '1' and all(digit == '0' for digit in n_ptr[0][1:]):
          # Same number of 9's as number of digits in LHS
          nines = ''.join(['9' for _ in n_ptr[0]])
          return from_ptr((nines, '', nines))
        # LHS not a power of 10
        else:
          p_ptr0 = str(int(n_ptr[0])-1)
          return from_ptr((p_ptr0, '9', p_ptr0[::-1]))
      # Middle digit > 0
      else:
        # RHS = mirror of LHS
        # and decrease 1 from middle digit
        return from_ptr((n_ptr[0], str(int(n_ptr[1])-1), n_ptr[0][::-1]))
  # One digit number
  else:
    # There is no palindrome numbers below zero
    assert (n > 0), "Zero is the first natural number!"
    # Return the predecessor
    return n - 1

def largest_palindrome_below_naive(n):
  p = n - 1
  while not is_palindrome(p):
    p -= 1
  return p

# print('Testing largest_palindrome_below...\n')
# TEST_RANGE = range(1, 100001)
# largest_palindrome_below_test = [largest_palindrome_below(n) for n in TEST_RANGE]
# largest_palindrome_below_naive_test = [largest_palindrome_below_naive(n) for n in TEST_RANGE]
# if all(largest_palindrome_below_test[i] == largest_palindrome_below_naive_test[i] for i in range(len(TEST_RANGE))):
#   print('Ok in {}'.format(TEST_RANGE))
# else:
#   for i in range(len(TEST_RANGE)):
#     if largest_palindrome_below_test[i] != largest_palindrome_below_naive_test[i]:
#       print('\t{} => {}={}\n'.format(TEST_RANGE[i], largest_palindrome_below_test[i], largest_palindrome_below_naive_test[i]))

def largest_palindrome_product(digits):
  nines = int(''.join('9' for _ in range(digits)))
  p_test = nines * nines
  found_product = False
  while not found_product:
    p_test = largest_palindrome_below(p_test)
    for k in range(np.floor(np.sqrt(p_test)).astype(int), nines):
      if p_test % k == 0:
        found_product = True
        print('{} = {} * {}'.format(p_test, k, p_test//k))
        break
  return p_test

largest_palindrome_product(3)
