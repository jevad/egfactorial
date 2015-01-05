import java.math.BigInteger;
import java.util.LinkedList;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;
import java.util.concurrent.RecursiveTask;

public class FactorialSkipAlgorithm {
	private static final BigInteger TWO = BigInteger.valueOf(2L);
	private static final int PROC_CT = Runtime.getRuntime()
			.availableProcessors();
	private static final BigInteger PISM = BigInteger.valueOf((long) PROC_CT);

	private static final BigInteger factImpl(final BigInteger first,
			final BigInteger last, final BigInteger skipBy) {

		BigInteger rslt = first;
		BigInteger f = skipBy.add(rslt);
		while (last.compareTo(f) >= 0) {
			rslt = rslt.multiply(f);
			f = skipBy.add(f);
		}

		return rslt;
	}

	static class Multiply extends RecursiveTask<BigInteger> {
		private static final long serialVersionUID = 657399357965184091L;
		private BigInteger left;
		private BigInteger right;

		public Multiply(final BigInteger left, final BigInteger right) {
			this.left = left;
			this.right = right;
		}

		@Override
		public BigInteger compute() {
			return left.multiply(right);
		}
	}

	static class SubTask extends RecursiveTask<BigInteger> {
		private static final long serialVersionUID = -2178509177157426304L;

		private BigInteger first;
		private BigInteger last;
		private BigInteger skipBy;

		public SubTask(final BigInteger first, final BigInteger last,
				final BigInteger skipBy) {
			this.first = first;
			this.last = last;
			this.skipBy = skipBy;
		}

		@Override
		public BigInteger compute() {
			return FactorialSkipAlgorithm.factImpl(first, last, skipBy);
		}
	}

	public static BigInteger fac(final BigInteger facme) {
		if (null == facme || BigInteger.ZERO.compareTo(facme) > 0) {
			throw new IllegalArgumentException(
					"no factorials for negative or undefined numbers");
		}

		BigInteger reply = BigInteger.ONE;
		if (BigInteger.ONE.equals(facme) || BigInteger.ZERO.equals(facme)) {
			// leave at one
		} else if (TWO.equals(facme)) {
			reply = TWO;
		} else {
			// This will be the number of tasks being run in parallel.
			// You can play around with this to see what works better
			// or worse on your hardware. Right now, I have it set to
			// the number of available processors.
			final BigInteger taskCount = PISM;

			// The default ForkJoinPool constructor creates a ForkJoinPool
			// with parallelism equal to the number of available processors.
			final ForkJoinPool pool = new ForkJoinPool();

			final LinkedList<ForkJoinTask<BigInteger>> tasks = new LinkedList<ForkJoinTask<BigInteger>>();

			BigInteger startingPoint = BigInteger.ONE;
			while (startingPoint.compareTo(taskCount) <= 0) {
				final SubTask lower = new SubTask(startingPoint, facme,
						taskCount);
				tasks.addLast(pool.submit(lower));
				startingPoint = BigInteger.ONE.add(startingPoint);
			}

			try {
				while (2 <= tasks.size()) {
					final BigInteger left = tasks.removeFirst().get();
					final BigInteger right = tasks.removeFirst().get();
					final Multiply m = new Multiply(left, right);
					tasks.addLast(pool.submit(m));
				}

				// should be only one item left at this point
				// it contains the calculation results
				reply = tasks.pop().get();
			} catch (InterruptedException | ExecutionException
					| CancellationException ex) {
				ex.printStackTrace(System.err);
			}

			// If this is not here, we tend to get out of memory errors.
			pool.shutdownNow();
		}

		return reply;
	}

	public static void main(final String[] argv) {
		final BigInteger facme = new BigInteger(argv[0]);
		System.out.println(FactorialSkipAlgorithm.fac(facme));
	}

}