import sys
import math
import time

def assert_nonnegative_integer(some_number):
	if (0 > some_number):
		raise StandardError("cannot compute factorial for a negative integer: " + str(some_number))

def math_factorial(some_number):
	return math.factorial(some_number)

def sanity_check(some_number, fun_one, fun_two):
	reply = True
	fac_via_math = fun_one(some_number)
	fac_via_range = fun_two(some_number)
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

def run_setup(compute_factorial_for_these, argv):
	iteration_count = 0

	if 1 < len(argv):
		iteration_count = int(argv[1])
		if (0 >= iteration_count):
			raise StandardError("the iteration count must be strictly positive")
	else:
		raise StandardError("the first argument is the iteration count")

	if 2 < len(argv):
		temp = argv[2:]
		for t in temp:
			iii = int(t)
			assert_nonnegative_integer(iii)
			compute_factorial_for_these.append(iii)
	else:
		raise StandardError("the second and all succeeding arguments are number of which to calculate the factorial")

	return iteration_count

