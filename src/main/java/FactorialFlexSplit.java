import java.math.BigInteger;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.RecursiveTask;

public class FactorialFlexSplit {
	private static final BigInteger TWO = BigInteger.valueOf(2L);

	// Adjust the value of MULT_LIMIT to adjust the parallelism --
	// results tend to be surprising. The smaller MULT_LIMIT * PISM,
	// the greater the parallelism. (Alternatively, directly adjust
	// the value of limitCalc, in fac() below.)

	// private static final BigInteger MULT_LIMIT = BigInteger.valueOf(300L);
	private static final BigInteger MULT_LIMIT = BigInteger.valueOf(1L);
	private static final BigInteger PISM = BigInteger.valueOf((long) (Runtime
			.getRuntime().availableProcessors()));

	private static final BigInteger factImpl(final BigInteger first,
			final BigInteger onePastLast) {
		// try to force reference onto the local stack
		// (If hotspot is any good, this won't help, but it's worth a try.)
		final BigInteger ONE = BigInteger.ONE;

		BigInteger reply = first;
		for (BigInteger current = ONE.add(first); current
				.compareTo(onePastLast) < 0; current = BigInteger.ONE
				.add(current)) {
			reply = reply.multiply(current);
		}

		return reply;
	}

	static class SubTask extends RecursiveTask<BigInteger> {
		static final long serialVersionUID = -1828579196804632167L;

		private BigInteger onePastLast;
		private BigInteger current;
		private BigInteger limit;

		public SubTask(final BigInteger first, final BigInteger onePastLast,
				final BigInteger limit) {
			this.onePastLast = onePastLast;
			this.current = first;
			this.limit = limit;
		}

		@Override
		public BigInteger compute() {
			BigInteger reply = BigInteger.ONE;

			final BigInteger size = onePastLast.subtract(current);
			if (limit.compareTo(size) < 0) {
				final BigInteger[] calc = size.divideAndRemainder(TWO);
				final BigInteger fOnePastLast = (BigInteger.ZERO
						.equals(calc[1]) ? calc[0] : BigInteger.ONE
						.add(calc[0])).add(current);
				// final BigInteger gFirst = fOnePastLast;
				final SubTask f = new SubTask(current, fOnePastLast, limit);
				final SubTask g = new SubTask(fOnePastLast, onePastLast, limit);
				f.fork();
				reply = g.compute().multiply(f.join());
				// .TODO. study the performance implications of instead using:
				// reply = f.join().multiply(g.compute());
			} else {
				reply = factImpl(current, onePastLast);
			}

			return reply;
		}

		@Override
		public String toString() {
			final StringBuilder bob = new StringBuilder("{");
			bob.append("onePastLast: ").append(onePastLast).append(", ");
			bob.append("current: ").append(current).append(", ");
			bob.append("limit: ").append(limit).append("}");
			return bob.toString();
		}
	}

	public static BigInteger fac(final BigInteger facme,
			final BigInteger limitMultiplier) {
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
			final BigInteger limitCalc = limitMultiplier.multiply(PISM);
			// The limit needs to be at least 2.
			final BigInteger limit = TWO.compareTo(limitCalc) >= 0 ? TWO
					: limitCalc;

			final ForkJoinPool pool = new ForkJoinPool();
			final RecursiveTask<BigInteger> subby = new SubTask(TWO,
					BigInteger.ONE.add(facme), limit);

			pool.invoke(subby);
			try {
				reply = subby.get();
			} catch (InterruptedException | ExecutionException
					| CancellationException ex) {
				ex.printStackTrace(System.err);
			}

			// If this is not here, we tend to get out of memory errors.
			pool.shutdownNow();
		}

		return reply;
	}

	public static BigInteger fac(final BigInteger facMe) {
		return fac(facMe, MULT_LIMIT);
	}

	public static void main(final String[] argv) {
		final BigInteger facme = new BigInteger(argv[0]);
		System.out.println(FactorialFlexSplit.fac(facme));
	}

}