#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <gmp.h>

/*
To get this to build, you'll need GMP, which you can get here:  
https://gmplib.org

On my system, the following command line worked for building and linking:
gcc factorial.c -I /usr/local/include -L /usr/local/lib -lgmp
*/

void 
fac(mpz_t * p_some_number, mpz_t * p_the_factorial_of_it) 
	{
	mpz_t iii;
	mpz_init(iii);
	mpz_set_si(iii, 1);
	while (mpz_cmp(iii, *p_some_number) <= 0)
		{
		mpz_mul(*p_the_factorial_of_it, *p_the_factorial_of_it, iii);
		mpz_add_ui(iii, iii, 1);	
		}
	}

double elapsed_time(struct timeval * p_start, struct timeval * p_end)
	{
	// .TODO. verify that this actually does what I think it does
	double start_time = (p_start->tv_sec) * 1000 + (p_start->tv_usec) / 1000;
	double end_time = (p_end->tv_sec) * 1000 + (p_end->tv_usec) / 1000;
	return end_time - start_time;
	}

void 
run_test(int iteration_count, int number_count, mpz_t * p_numbers)
	{
	for (int i = 0; i < number_count; ++i) 
		{
		struct timeval start; 
		struct timeval end;
		// .WARNING. pointing onto stack!
		gettimeofday(&start, 0);
		for (int j = 0; j < iteration_count; ++j)
			{
			mpz_t the_factorial_of_it;
			mpz_init(the_factorial_of_it);
			mpz_set_si(the_factorial_of_it, 1);
			// .WARNING. pointing onto stack!
			fac(&(p_numbers[i]), &the_factorial_of_it); 
			// to verify this is actually calcuating the factorial:
			// gmp_printf("the factorial of %d is %Zd\n", number_count, the_factorial_of_it);
			}
		gettimeofday(&end, 0);
		gmp_printf("time to calculate the factorial of %Zd %d times: %f\n", p_numbers[i], iteration_count, elapsed_time(&start, &end));
		}
	}

int 
main(const int argc, const char ** pp_argv) 
	{
	// .TODO. argument checking
	int iteration_count = atoi(pp_argv[1]);

	int number_count = argc - 2;
	mpz_t * p_numbers = malloc(number_count * sizeof(mpz_t));
	for (int i = 0; i < number_count; ++i) 
		{
		mpz_init(p_numbers[i]);
		mpz_set_str(p_numbers[i], pp_argv[2 + i], 10);
		}
	run_test(iteration_count, number_count, p_numbers);

	free(p_numbers);
	return 0;
	}