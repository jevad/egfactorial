import sys
import math
import itertools
from itertools import islice
from itertools import count
import time

def assert_nonnegative_integer(some_number):
	if (0 > some_number):
		raise StandardError("cannot computer factorial for a negative integer: " + str(some_number))

def math_factorial(some_number):
	return math.factorial(some_number)

def range_factorial(some_number):
	return reduce(lambda x, y: x*y, islice(count(1, 1), some_number))

def sanity_check(some_number):
	reply = True
	fac_via_math = math_factorial(some_number)
	fac_via_range = range_factorial(some_number)
	if (fac_via_range == fac_via_math):
		print "both functions returned the same value for the factorial of %d" % some_number
	else:
		print "the two functions returned different values for the factorial of %d: %d , %d" % (some_number, fac_via_math, fac_via_range)
		reply = False  
	return reply

def time_it(iteration_count, some_number, fac_func, name):
	print "timing %d iterations of %s for %d" % (iteration_count, name, some_number)
	# this is probably not the best way to time things in Python;
	# however, I will be comparing these numbers to other systems.
	start = time.time()
	for nnn in range(iteration_count):
		fac_func(some_number)
	end = time.time()
	print str(end - start)

def all_factorials(iteration_count, compute_factorial_for_these):
	for iii in compute_factorial_for_these:
		if (sanity_check(iii)):
			time_it(iteration_count, iii, math_factorial, "math factorial")
			time_it(iteration_count, iii, range_factorial, "range factorial")

def main():
	iteration_count = 0
	compute_factorial_for_these = []

	if 1 < len(sys.argv):
		iteration_count = int(sys.argv[1])
		if (0 >= iteration_count):
			raise StandardError("the iteration count must be strictly positive")
	else:
		raise StandardError("the first argument is the iteration count")

	if 2 < len(sys.argv):
		temp = sys.argv[2:]
		for t in temp:
			iii = int(t)
			assert_nonnegative_integer(iii)
			compute_factorial_for_these.append(iii)
	else:
		raise StandardError("the second and all succeeding arguments are number of which to calculate the factorial")

	all_factorials(iteration_count, compute_factorial_for_these)

if __name__ == '__main__':
	main()