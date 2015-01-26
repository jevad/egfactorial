import sys
from disco.core import Job
from disco.core import result_iterator
import time
import factorial_common as fc

# You need to have Disco running to use this script.
# see http://discoproject.org .
# I'm not really using Disco the way it was intended to
# be used, and, as it turns out, it isn't all that useful
# when used in the way I tried to use it.  It did, however,
# produce correct results.  

def d_map(line, params):
    # because this is "shipped across the wire",
    # the import statements need to be local to the function definition
    from itertools import islice
    from itertools import count
    begend = line.strip().split(',')
    first = int(begend[0]);
    last = int(begend[1]);
    how_many = last - first + 1;
    # You cannot use the standard function, reduce(), here.  It will fail.
    # I'm not sure, but it appears like the Disco Framework redifines "reduce()" on the other end.
    # yield "subresult", reduce(lambda x, y: x*y, range(int(begend[0]), 1+int(begend[1])))
    # So you have to do something like this instead:
    temp = 1
    # If the chunk size is larger, it may be more efficient to not use range 
    # (at least for Python 2.x):
    # for iii in range(int(begend[0]), 1+int(begend[1])):
    for iii in islice(count(first, 1), how_many):
        temp = temp*iii
    yield "subresult", temp

def d_reduce(iter, params):
    temp = 1
    for n, iii in iter:
        temp = temp*iii
    yield "factorial", temp

def d_factorial(facme):
    fac = 1
    # print "running factorial of " + str(facme)
    if 2 == facme:
        fac = 2
    elif 2 < facme:
        ipt = []
        # divide input into chunks of chk_sz and add to input
        chk_sz = 25000
        start = 1 
        end = chk_sz
        while end < facme:
            ipt.append("raw://" + str(start) + "," + str(end))
            start = start + chk_sz
            end = end + chk_sz
        ipt.append("raw://" + str(start) + "," + str(facme))
     
        # submit the disco job
        job = Job(name="factorial").run(input=ipt, map=d_map, reduce=d_reduce)
        # print "waiting"
        for title, rslt in result_iterator(job.wait(show=True)):
            fac = rslt
    return fac   

def main():
    compute_factorial_for_these = []
    iteration_count = fc.run_setup(compute_factorial_for_these, sys.argv)

    for iii in compute_factorial_for_these:
        if fc.sanity_check(iii, fc.math_factorial, d_factorial):
            fc.time_it(iteration_count, iii, d_factorial, "disco")

if __name__ == '__main__':
    main()