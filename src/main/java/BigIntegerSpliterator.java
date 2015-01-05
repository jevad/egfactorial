// default package

import java.math.BigInteger;
import java.util.Comparator;
import java.util.Spliterator;
import java.util.function.Consumer;

/**
 * Unfortunately, the new stream functionality in Java 8 doesn't provide
 * BigInteger ranges. This Spliterator is an attempt to solve that problem.
 * 
 * The characteristics of this Spliterator are <code>IMMUTABLE</code>,
 * <code>DISTINCT</code>, <code>SORTED</code> and <code>NONNULL</code>.
 */
public class BigIntegerSpliterator implements Spliterator<BigInteger> {
	private static final BigInteger MAX_LONG = BigInteger
			.valueOf(Long.MAX_VALUE);
	private static final BigInteger TWO = BigInteger.valueOf(2L);

	private final int characteristics = IMMUTABLE & DISTINCT & SORTED & NONNULL;

	private BigInteger lastPlusOne = BigInteger.ZERO;
	private BigInteger current = BigInteger.ZERO;
	private BigInteger skipBy = BigInteger.ONE;

	/**
	 * Constructs a Spliterator represnting a range of BigInteger, where
	 * <code>first</code> is the first element of the range and
	 * <code>last</code> is the last element of the range (inclusive): [first,
	 * last].
	 * 
	 * You can space the range elements with the skipBy variable. For example,
	 * for even numbers, start with an even number and set skipBy to 2.
	 * 
	 * @param first
	 *            the first element of the range
	 * @param last
	 *            the last element of the range
	 * @param skipBy
	 *            the range elements will be this far apart
	 * @throws IllegalArgumentException
	 *             if any arguments are null or if <code>first</code> is greater
	 *             than <code>last</code>
	 */
	public BigIntegerSpliterator(final BigInteger first, final BigInteger last,
			final BigInteger skipBy) {
		if (null == first || null == last || null == skipBy) {
			throw new IllegalArgumentException(
					"range elements may not be null: " + String.valueOf(first)
							+ ", " + String.valueOf(last) + ", "
							+ String.valueOf(skipBy));
		}

		if (first.compareTo(last) > 0) {
			throw new IllegalArgumentException(first.toString()
					+ " is larger than " + last);
		}

		if (BigInteger.ZERO.equals(skipBy)) {
			throw new IllegalArgumentException("skip by zero not permitted");
		}

		this.skipBy = skipBy;
		this.current = first;
		this.lastPlusOne = BigInteger.ONE.add(last);
	}

	/**
	 * Constructs a Spliterator representing a range of BigInteger, where
	 * <code>first</code> is the first element of the range and
	 * <code>last</code> is the last element of the range (inclusive): [first,
	 * last].
	 * 
	 * @param first
	 *            the first element of the range
	 * @param last
	 *            the last element of the range
	 * @throws IllegalArgumentException
	 *             if either element is null or if <code>first</code> is greater
	 *             than <code>last</code>
	 */
	public BigIntegerSpliterator(final BigInteger first, final BigInteger last) {
		this(first, last, BigInteger.ONE);
	}

	/**
	 * The characteristics of this Spliterator are <code>IMMUTABLE</code>,
	 * <code>DISTINCT</code>, <code>SORTED</code> and <code>NONNULL</code>.
	 * 
	 * @see java.util.Spliterator#characteristics()
	 */
	@Override
	public int characteristics() {
		return this.characteristics;
	}

	/**
	 * @see java.util.Spliterator#hasCharacteristics(int)
	 */
	@Override
	public boolean hasCharacteristics(final int characteristics) {
		return 0 != (characteristics & this.characteristics);
	}

	private BigInteger currentSize() {
		final BigInteger[] result = lastPlusOne.subtract(current).divideAndRemainder(
				skipBy);
		return BigInteger.ZERO.equals(result[1]) ? result[0] : BigInteger.ONE
				.add(result[0]);
	}

	private long estimateSizeImpl(final long defaultValue) {
		long size = defaultValue;

		final BigInteger sizeCalc = currentSize();
		final int maxComparison = MAX_LONG.compareTo(sizeCalc);
		if (maxComparison > 0) {
			size = sizeCalc.longValue();
		} else if (maxComparison == 0){
			size = Long.MAX_VALUE;
		}

		return size;
	}

	/**
	 * @see java.util.Spliterator#estimateSize()
	 */
	@Override
	public long estimateSize() {
		return estimateSizeImpl(Long.MAX_VALUE);
	}

	/**
	 * Unlike the "default" implementation, this implementation returns the
	 * "exact size" when the "exact size" can be determined, even though this is
	 * not a <code>SIZED</code> Spliterator.
	 * 
	 * @see java.util.Spliterator#getExactSizeIfKnown()
	 */
	@Override
	public long getExactSizeIfKnown() {
		return estimateSizeImpl(-1L);
	}

	/**
	 * Allows callers to step through a range of <code>BigInteger</code>,
	 * incrementing by the "skip by" value.
	 * 
	 * @see java.util.Spliterator#tryAdvance(Consumer)
	 */
	@Override
	public boolean tryAdvance(final Consumer<? super BigInteger> action) {
		// .NOTE. if you change this method, be sure to change
		// forEachRemaining();
		boolean reply = false;

		if (current.compareTo(lastPlusOne) < 0) {
			action.accept(current);
			current = skipBy.add(current);
			reply = true;
		}

		return reply;
	}

	/**
	 * Steps through the remaining range of <code>BigInteger</code>,
	 * incrementing by one.
	 * 
	 * @see java.util.Spliterator#forEachRemaining(Consumer)
	 */
	@Override
	public void forEachRemaining(final Consumer<? super BigInteger> action) {
		while (current.compareTo(lastPlusOne) < 0) {
			action.accept(current);
			current = skipBy.add(current);
		}
	}

	/**
	 * <code>BigInteger</code> has what Java calls a "natural ordering", so this
	 * implementation always returns <code>null</code>.
	 * 
	 * @see java.util.Spliterator#getComparator()
	 */
	@Override
	public Comparator<? super BigInteger> getComparator() {
		// .NOTE. BigIntegers have a 'natural' order
		return null;
	}

	/**
	 * If the instance has more than two elements remaining in the range, then
	 * this method splits off roughly half the remaining elements. Otherwise, it
	 * returns null, per the spec.
	 * 
	 * @see java.util.Spliterator#trySplit()
	 */
	@Override
	public BigIntegerSpliterator trySplit() {
		// .TODO. need to review what happens when skipBy is not 1.
		BigIntegerSpliterator reply = null;

		BigInteger sizeCalc = currentSize();
		int twoComparison = TWO.compareTo(sizeCalc);
		if (twoComparison < 0) {
			final BigInteger halfSize = sizeCalc.divide(TWO).multiply(skipBy);
			final BigInteger replyLast = halfSize.add(current);
			reply = new BigIntegerSpliterator(current, replyLast, skipBy);
			this.current = skipBy.add(replyLast);
		} else if (twoComparison == 0) {
			// might not be efficient
			reply = new BigIntegerSpliterator(current, current, skipBy);
			this.current = skipBy.add(current);
		}

		return reply;
	}
}