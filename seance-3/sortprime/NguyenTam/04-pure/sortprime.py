''' import os
    output = os.popen(’ls’).read() # If you want to get the output data
'''

import os
import math

# Read ouput from genrandom and store into numbers
numbers = os.popen('ocaml genrandom 1').read()

# Remove new lines
numbers = numbers.splitlines() 

lstNumbers = []

# Convert string to int
for n in numbers:
	lstNumbers.append(int(n))

# Check if a number is prime
def is_prime(n):
    if n % 2 == 0 and n > 2: 
        return False
    for i in range(3, int(math.sqrt(n)) + 1, 2):
        if n % i == 0:
            return False
    return True

lstPrime = []

# Not use the Eratosthene algorithm
def findPrime(lst):
    for l in lst:
        if (is_prime(l)):
            lstPrime.append(l)

# Sort the result
lstNumbers = sorted(lstNumbers)

