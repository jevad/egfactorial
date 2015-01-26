// default package
import java.lang.management.ManagementFactory;
import java.lang.management.MemoryPoolMXBean;
import java.lang.management.ThreadMXBean;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.LongConsumer;
import java.util.stream.LongStream;

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

	public static long timeIt(
			final BiFunction<BigInteger, BigInteger, BigInteger> func,
			final BigInteger facme, final int count, final BigInteger multiplier) {
		final long start = System.currentTimeMillis();

		for (int i = count; 0 < i; --i) {
			func.apply(facme, multiplier);
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
			final BigInteger skipResult = verifyIt(FactorialSkipAlgorithm::fac,
					val);
			final BigInteger ffResult = verifyIt(FactorialFlexSplit::fac, val);
			final BigInteger strmResult = verifyIt(
					FactorialParallelStream::fac, val);

			if (skipResult.equals(ffResult) && ffResult.equals(strmResult)) {
				System.out.println("all tests agree on factorial for " + val);
			} else {
				areConsistent = false;
				System.out.println("inconsistent results for " + val
						+ " factorial: " + skipResult + ", " + ffResult + ", "
						+ strmResult);
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

	private static void adjustParallelizationOp(final int iterationCount,
			final BigInteger val, final BigInteger multiplier) {
		takeabreak();
		System.out.println("     relative result for p-multiplier of "
				+ multiplier
				+ ": "
				+ timeIt(FactorialFlexSplit::fac, val, iterationCount,
						multiplier));

	}

	private static final long totalMemoryUsageAndReset(
			final List<MemoryPoolMXBean> m) {
		long runningTotal = 0;
		for (MemoryPoolMXBean b : m) {
			runningTotal += b.getUsage().getUsed();
			// .WARNING. side effect
			b.resetPeakUsage();
		}
		return runningTotal;
	}

	public static void adjustParallelization(final int iterationCount,
			final BigInteger val) {
		System.out.println("running tests with different parallelization, "
				+ "using flex forking factorial");

		long[] mutliplierDivisors = { 1L, 2L, 4L, 8L, 16L, 32L, 64L, 128L,
				256L, 512L, 1024L, 2048L, 4096L, 8192L, 16384L, 32768L };

		for (long i : mutliplierDivisors) {
			takeabreak();
			adjustParallelizationOp(iterationCount, val, BigInteger.valueOf(i));
		}
		
		LongStream.of(1L, 2L, 4L, 8L, 16L, 32L, 64L, 128L,
				256L, 512L, 1024L, 2048L, 4096L, 8192L, 16384L, 32768L).forEach(
						new LongConsumer() {

							@Override
							public void accept(long i) {
								takeabreak();
								adjustParallelizationOp(iterationCount, val, BigInteger.valueOf(i));
							}
							
						});

	}

	public static void resourceUsageImpl(
			final Function<BigInteger, BigInteger> func,
			final int iterationCount, final BigInteger val) {
		takeabreak();
		takeabreak();

		ThreadMXBean t = ManagementFactory.getThreadMXBean();
		final long startingMemoryUsage = totalMemoryUsageAndReset(ManagementFactory
				.getMemoryPoolMXBeans());
		final int startingThreadCount = t.getThreadCount();
		t.resetPeakThreadCount();

		timeIt(func, val, iterationCount);

		final int peakThreadCount = t.getPeakThreadCount();
		final long peakMemoryUsage = ManagementFactory.getMemoryPoolMXBeans()
				.stream().mapToLong(i -> i.getPeakUsage().getUsed()).sum();
		System.out.println("     starting Memory: " + startingMemoryUsage);
		System.out.println("     peak Memory: " + peakMemoryUsage);
		System.out.println("     Memory increase: "
				+ (peakMemoryUsage - startingMemoryUsage));
		System.out
				.println("     starting thread count: " + startingThreadCount);
		System.out.println("     peak thread count: " + peakThreadCount);
		System.out.println("     Thread increase: "
				+ (peakThreadCount - startingThreadCount));

	}

	public static void resourceUsage(final int iterationCount,
			final BigInteger val) {
		System.out.println("resource usage for parallel streaming");
		resourceUsageImpl(FactorialParallelStream::fac, iterationCount, val);
		System.out.println("resource usage for flex forking factorial");
		resourceUsageImpl(FactorialFlexSplit::fac, iterationCount, val);
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
					testValues.stream()
							.max(/*new Comparator<BigInteger>() {
								@Override
								public int compare(BigInteger o1, BigInteger o2) {
									return o1.compareTo(o2);
								}
							}*/ (left, right) -> left.compareTo(right)).
							ifPresent(new Consumer<BigInteger>() {
								@Override
								public void accept(BigInteger t) {
									// TODO Auto-generated method stub
									adjustParallelization(10 < iterationCount ? 10
											: iterationCount, t);
									resourceUsage(iterationCount, t);
								};								
							});
//					BigInteger biggest = testValues.stream()
//							.max(new Comparator<BigInteger>() {
//								@Override
//								public int compare(BigInteger o1, BigInteger o2) {
//									return o1.compareTo(o2);
//								}
//							}).get();
//					adjustParallelization(10 < iterationCount ? 10
//							: iterationCount, biggest);
//					resourceUsage(iterationCount, biggest);
				}
			} catch (NumberFormatException nfe) {
				printhelp();
			}
		}
	}

}