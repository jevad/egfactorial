// default package
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

public class TimeThem {

	private static final BigInteger MAX_FOR_SINGLE_THREAD = BigInteger
			.valueOf(2000L);

	/**
	 * @param func
	 *            one of our factorial calculator functions
	 * @param facme
	 *            the target of the factorial function
	 * @return the calculated factorial
	 */
	public static BigInteger verifyIt(
			final Function<BigInteger, BigInteger> func, final BigInteger facme) {
		return func.apply(facme);
	}

	/**
	 * @param func
	 *            one of our factorial calculator functions
	 * @param facme
	 *            the target of the factorial function
	 * @param count
	 *            how many times you want to repeat the function call
	 * @return how long it took, in milliseconds to complete all the calls
	 */
	public static long timeIt(final Function<BigInteger, BigInteger> func,
			final BigInteger facme, final int count) {
		final long start = System.currentTimeMillis();

		for (int i = count; 0 < i; --i) {
			func.apply(facme);
		}

		final long end = System.currentTimeMillis();
		return end - start;
	}

	/**
	 * Runs each of our factorial calculators and verifies that, for the same
	 * target value, they return the same factorial value.
	 * 
	 * @param testValues
	 *            a list of target values
	 * @return true if the factorial values agree, false if they don't.
	 */
	public static boolean areTestsConsistent(final List<BigInteger> testValues) {
		boolean areConsistent = true;

		System.out.println("testing consistency");
		for (BigInteger val : testValues) {
			final BigInteger fstResult = verifyIt(FactorialSingleThread::fac,
					val);
			final BigInteger skipResult = verifyIt(FactorialSkipAlgorithm::fac,
					val);
			final BigInteger ffResult = verifyIt(FactorialFlexSplit::fac, val);
			final BigInteger strmResult = verifyIt(
					FactorialParallelStream::fac, val);

			if (fstResult.equals(skipResult) && skipResult.equals(ffResult)
					&& ffResult.equals(strmResult)) {
				// System.out.println("all tests agree that " + val
				// + " factorial is " + strmResult);
				System.out.println("all tests agree on factorial for " + val);
			} else {
				areConsistent = false;
				System.out.println("inconsistent results for " + val
						+ " factorial: " + fstResult + ", " + skipResult + ", "
						+ ffResult + ", " + strmResult);
			}
		}

		return areConsistent;
	}

	private static void takeabreak() {
		System.gc();
		try {
			Thread.sleep(5000);
		} catch (InterruptedException e) {
			// continue
		}
	}

	/**
	 * For each test value, runs each of our factorial calculators the same
	 * number of times and prints out the run times for each.
	 * 
	 * @param testValues
	 *            a list of target values
	 */
	public static void compare(final int iterationCount,
			final List<BigInteger> testValues) {
		System.out.println("running timing tests");
		System.out.println("available processors: "
				+ Runtime.getRuntime().availableProcessors());
		System.out.println("iteration count: " + iterationCount);

		for (BigInteger val : testValues) {
			System.out.println("running timing tests for: " + val);

			takeabreak();
			System.out
					.println("     relative result for parallel streams is: "
							+ timeIt(FactorialParallelStream::fac, val,
									iterationCount));

			takeabreak();
			System.out
					.println("     relative result for flex forking factorial is: "
							+ timeIt(FactorialFlexSplit::fac, val,
									iterationCount));

			takeabreak();
			System.out
					.println("     relative result for custom forking skip algorithm is: "
							+ timeIt(FactorialSkipAlgorithm::fac, val,
									iterationCount));

			// this one takes too long when the values get larger
			if (MAX_FOR_SINGLE_THREAD.compareTo(val) > 0) {
				takeabreak();
				System.out
						.println("     relative result for single thread is: "
								+ timeIt(FactorialSingleThread::fac, val,
										iterationCount));
			}
		}
	}

	private static void printhelp() {
		System.out.println("The first argument (required) is the number "
				+ "of iterations to run for each test.");
		System.out.println("Any remaining arguments "
				+ "(of which there must be at least one) "
				+ "are test values -- the factorial is calculated for "
				+ "each of these values.");
	}

	/**
	 * @param argv
	 *            the number of iterations you want to run, followed by a list
	 *            of target values
	 */
	public static void main(final String[] argv) {
		final int argct = argv.length;
		if (2 > argct) {
			printhelp();
		} else {
			try {
				int iterationCount = Integer.parseInt(argv[0]);
				final List<BigInteger> testValues = new ArrayList<BigInteger>();
				for (int i = 1; i < argct; ++i) {
					testValues.add(new BigInteger(argv[i]));
				}

				if (areTestsConsistent(testValues)) {
					compare(iterationCount, testValues);
				}
			} catch (NumberFormatException nfe) {
				printhelp();
			}
		}
	}

}