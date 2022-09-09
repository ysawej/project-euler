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

def solution35():
    nums = np.random.random_integers(1,100,20)
    #for i in nums:
    #    print(i, is_prime(i))

    print(circular_primes(7))
    print(len(circular_primes(7)))


    print(is_circular_prime([3,7]))
    print(is_circular_prime([1,9]))

# Problem 36
def solution36():
    res = []
    for i in range(1024):
        a = "{0:b}".format(i)
        b = a[::-1]
        val = int("0b"+a+b, 2)
        a1 = str(val)
        a2 = a1[::-1]
        if a1==a2:
            res.append(val)
    print(res, np.sum(res))

# Problem 37
# Truncatable prime
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
            
def solution37():
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

def solution38():
    for i in range(9999,9001, -1):
        find_pandig(i)


# Problem 39 // ans:840 16 triplets
def solution39():
    max_triples = 0
    max_i = 120
    for i in range(120, 1001):
        start = i//3 +1
        end = i-1
        count = 0 
        for a1 in range(start, end):
            remaining = i - a1
            for k in range(1, remaining):
                a2 = k
                a3 = remaining - k
                if a1*a1 == a2*a2 + a3*a3:
                    count = count + 1
                    #print((a1+a2+a3), a1,a2, a3)
        if count > 0 and max_triples < count:
            max_triples = count
            max_i = i
            print(i, count)



# Problem 40
def solution40():
    n_dig_counts = [0, 9, 2*(100-10), 3*(1000-100), 4*(10000-1000), 5*(100000-10000), 6*(1000000-100000)]
    n_dig_starts = [0, 9, 99, 999, 9999, 99999, 999999]
    n_dig_digs = [1, 2, 3, 4, 5, 6, 7]
    boundaries = np.cumsum(n_dig_counts)
    print(boundaries)
    #for d in range(188, 210):
    #for d in range(2880, 2900):
    ans=[]
    for d in [1,10, 100, 1000, 10_000, 100_000, 1000_000]:
        for i in range(1,len(boundaries)):
            if d <= boundaries[i] and d > boundaries[i-1]:
                c = (d - boundaries[i-1])
                v_d = n_dig_starts[i-1] + (c // n_dig_digs[i-1])
                r = c%n_dig_digs[i-1]
                if r == 0:                    
                    print(v_d % 10,end=" ")
                    ans.append(v_d % 10)
                else:
                    print(((v_d+1) // (n_dig_starts[n_dig_digs[i-1] - r]+1)) % 10,end="")
                    ans.append(((v_d+1) // (n_dig_starts[n_dig_digs[i-1] - r]+1)) % 10)
    print("\nans, product=",ans, np.prod(ans))
    # ans 210
      
    #print(boundaries)

solution36()
