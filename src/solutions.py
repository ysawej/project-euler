import numpy as np
import math
from typing import Set

ten_powers = [10**i for i in range(100)]

def is_prime(p: int)-> bool:
    if p < 2:
        return False
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
# 313, 3137, 373, , 3797, 797, 379, 317
# 23, 37, 53, 73, 313, 317, 373, 379, 797, 3137, 3797
# sum([23, 37, 53, 73, 313, 317, 373, 797, 3137, 3797, 739397]) = 748317

def trunc_primes(i:int):
        trunc_prime = False
        if (i%10==3 or i%10==7) and is_prime(i):
            trunc_prime = True
            j = 0
            num = i
            while num // ten_powers[j] > 0:
                if ten_powers[j]>1 and not is_prime(num % ten_powers[j]):
                    trunc_prime = False
                if not is_prime(num // ten_powers[j]):
                    trunc_prime = False
                j = j+1
            if trunc_prime:
                return True
            return False

for i in range(1000000):
    if trunc_primes(i):
        print("yay found one ", i)

# Problem 38
def pandigital(strnum):
    if len(strnum) != 9:
        return False
    for i in range(1,10):
        if str(i) not in strnum:
            return False
    return True
def find_pandig(i):
    k = 1
    res = ''
    while len(res) < 9:
        res = res + str(i*k)
        k = k + 1
    if pandigital(res):
        print(res, i, k)

for i in range(9999,9001, -1):
    find_pandig(i)