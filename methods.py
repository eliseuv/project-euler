import numpy as np

# Palindrome Tuple Representation

def to_ptr(n):
  # Convert the number to string
  n_str = str(n)
  # Get digit count
  l = len(n_str)
  # Auxiliary variables
  l_half_floor = np.floor(l/2, dtype=np.int64)
  l_half_ceil = np.ceil(l/2, dtype=np.int64)
  # Convert to palindrome tuple representation
  return (n_str[:l_half_floor], n_str[l_half_floor:l_half_ceil], n_str[l_half_ceil:])

def from_ptr(ptr, dtype=np.int64):
  # Just concatenate everything
  return dtype(ptr[0] + ptr[1] + ptr[2])

def is_palindrome(n):
  # Convert to PTR
  n_ptr = to_ptr(n)
  # Perform check
  return n_ptr[0] == n_ptr[2][::-1]


# Eratosthenes prime numbers sieve
def next_prime(primes):
  # Number to be tested
  p_test = primes[-1]
  # This loop will run until a number is found to be prime
  prime_found = False
  while not prime_found:
    # Go to the next natural
    p_test += 1
    # Check against all previous primes
    for p in primes:
      # No need to keep testing for primes larger than sqrt(p_test)
      if p > np.sqrt(p_test):
        # p_test is not divisible by any prime <= sqrt(p_test) => p_test is prime
        prime_found = True
        break
      # If it is divisible by some prime
      if p_test % p == 0:
        break
  # Once the above loop is escaped, we have found a prime number
  return np.append(primes, p_test)

# Eratosthenes prime numbers sieve
def primes_sieve(primes, counts):
  # Original number of primes
  n_prev = primes.size
  # Allocate memory for new primes
  primes = np.resize(primes, n_prev + counts)
  # Number to be tested
  p_test = primes[n_prev-1]
  # Repeat sieve however many time needed
  for i in range(counts):
    # This loop will run until a number is found to be prime
    prime_found = False
    while not prime_found:
      # Go to the next natural
      p_test += 1
      # Check against all previous primes
      for j in range(n_prev + i):
        # The prime number we are testing against
        p = primes[j]
        # No need to keep testing for primes larger than sqrt(p_test)
        if p > np.sqrt(p_test):
          # p_test is not divisible by any prime <= sqrt(p_test) => p_test is prime
          prime_found = True
          break
        # If it is divisible by some prime
        if p_test % p == 0:
          break
    # Once the above loop is escaped, we have found a prime number
    primes[n_prev + i] = p_test
  return primes

# Returns an array with the first n prime numbers
def first_primes(n):
  return primes_sieve(np.array([2], dtype=int), n-1)
