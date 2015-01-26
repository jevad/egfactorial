import sys
import itertools
from itertools import islice
from itertools import count
import factorial_common as fc

def range_factorial(some_number):
	# Instead of count/islice, you could use range; however, range,
	# at least in Python 2, will instantiate all the items in the 
	# resulting list all at once, so you wouldn't want to do that when
	# calculating the factorial of a large number.  (This may have
	# changed in Python 3, since Python 3 uses streams, but I'm not sure.)
	# Another option would be xrange, but xrange, for many implementations,
	# only handles ranges up to the maximum integral value the CPU handles.
	# In practice, I'm not going to try to calculate a factorial larger
	# than that, but to make this match up with my code in other languages, 
	# which, in principle, could calculate larger factorials, I chose to 
	# use count/islice.  
	return reduce(lambda x, y: x*y, islice(count(1, 1), some_number))

def all_factorials(iteration_count, compute_factorial_for_these):
	for iii in compute_factorial_for_these:
		if fc.sanity_check(iii, fc.math_factorial, range_factorial):
			fc.time_it(iteration_count, iii, fc.math_factorial, "math factorial")
			fc.time_it(iteration_count, iii, range_factorial, "range factorial")

def main():
	compute_factorial_for_these = []
	iteration_count = fc.run_setup(compute_factorial_for_these, sys.argv)

	all_factorials(iteration_count, compute_factorial_for_these)

if __name__ == '__main__':
	main()