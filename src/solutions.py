import numpy as np
import math
from typing import Set

ten_powers = [10**i for i in range(100)]

def is_prime(p: int)-> bool:
    for i in range(2,int(np.sqrt(p))+1):
        if p%i == 0:
            return False
    return True


# Problem 35
def is_circular_prime(digits: list[int]) -> Set[int]:
    """Returns set of circular primes with all rotations."""
    tpwrs = ten_powers[:len(digits)]
    result = set([])
    for i in range(len(digits)):
        new_digits = digits[i+1:] +digits[:i+1]
        #print (digits, new_digits, tpwrs[::-1])
        num = np.dot(tpwrs[::-1], new_digits)
        if not is_prime(num):
            return set([])
        else:
            result.add(num)
    return result


def circular_primes(n:int)-> Set[int]:
    """Returns count of circular primes less than n digits long."""
    cprimes = set([2,3,5,7])
    
    for i in range(2,n):
        num = [1]*i
        max_num = [9]*i

        cprimes.update(is_circular_prime(num))
        while num < max_num:
            j = i-1
            while num[j] == 9:
                j = j-1
            num[j] += 2
            while num < max_num and j+1 < i:
                num[j+1] = 1
                j += 1
            cprimes.update(is_circular_prime(num))
    return cprimes
            


nums = np.random.random_integers(1,100,20)
#for i in nums:
#    print(i, is_prime(i))
print(circular_primes(7))
print(len(circular_primes(7)))


print(is_circular_prime([3,7]))
print(is_circular_prime([1,9]))

# Problem 37
# Truncatable prime
# not correct yet
# 313, 3137, 373, 53, 23, 73, 3797, 797, 379, 37, 317