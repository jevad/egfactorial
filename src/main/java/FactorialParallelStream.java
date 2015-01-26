// default package

import java.math.BigInteger;
import java.util.function.BinaryOperator;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public class FactorialParallelStream {
	private static final BigInteger TWO = BigInteger.valueOf(2L);

	private static UnaryOperator<BigInteger> addOne = new UnaryOperator<BigInteger>() {
		@Override
		public BigInteger apply(BigInteger b) {
			return BigInteger.ONE.add(b);
		}
	};
	
	// instead of the above, we could just do something like this ...
	private static UnaryOperator<BigInteger> addOneWithLessCode = BigInteger.ONE::add;
	
	// or you could use a lambda expression ...
	private static UnaryOperator<BigInteger> addOneLambder = n -> BigInteger.ONE.add(n);
	// or like this ...
	private static UnaryOperator<BigInteger> addOneTheOtherWay = n -> n.add(BigInteger.ONE);
	// note that Java figured out that n is a BigInteger (because, what else could it be?)
	// Sometimes, you may have to tell Java what the type is:
	private static UnaryOperator<BigInteger> addOneYetAgain = (BigInteger n) -> n.add(BigInteger.ONE);

	private static Stream<BigInteger> upToMaxLong(final BigInteger facme) {
		// This option will only work when you're looking for a factorial
		// of a number less than the largest possible long.
		return Stream.iterate(BigInteger.ONE, BigInteger.ONE::add).limit(facme.longValue())
				.parallel();
	}

	private static Stream<BigInteger> takesForever(final BigInteger facme) {
		// So you might think that this would be a way to get a stream of
		// BigInteger that's limited but is longer than the largest possible
		// long. Technically, you would be correct. However, anything that
		// consumes this string will run "forever" because the filter will test
		// every element of the stream, which is, of course, infinite. You
		// could work around this, by, say, using exceptions for flow control,
		// but it would not be ideal.
		return Stream.iterate(BigInteger.ONE, addOne)
				.filter(new Predicate<BigInteger>() {
					@Override
					public boolean test(final BigInteger b) {
						return b.compareTo(facme) <= 0;
					}
				}).parallel();
	}

	private static Stream<BigInteger> takesForeverAgain(final BigInteger facme) {
		// Same as the above, but using a lambda expression
		return Stream.iterate(BigInteger.ONE, addOne)
				.filter(n -> n.compareTo(facme) <= 0).parallel();
	}

	private static Stream<BigInteger> streamOfBigIntegerUpTo(
			final BigInteger facme) {
		// So, if you want to have a proper BigInteger range, you'll need
		// to create your own Spliterator implementation, which I did
		// (BigIntegerSpliterator).
		return StreamSupport.stream(new BigIntegerSpliterator(BigInteger.ONE,
				facme), true);
	}

	private static BinaryOperator<BigInteger> mult() {
		// This is how you would create your own operator for the reduce step.
		// However, as it turns out, we don't have to do that because the Java
		// system is "smart" enough to know that the BigInteger.multiply method
		// can be used as a binary operator. So this will be unused -- it's
		// just here as an example.
		return new BinaryOperator<BigInteger>() {
			@Override
			public BigInteger apply(final BigInteger left,
					final BigInteger right) {
				return left.multiply(right);
			}
		};
	}

	public static BigInteger fac(final BigInteger facme) {
		if (null == facme || BigInteger.ZERO.compareTo(facme) > 0) {
			throw new IllegalArgumentException(
					"no factorials for negative or undefined numbers");
		}

		BigInteger reply = BigInteger.ONE;
		if (BigInteger.ONE.equals(facme) || BigInteger.ZERO.equals(facme)) {
			// leave as one
		} else if (TWO.equals(facme)) {
			reply = TWO;
		} else {
			// System.err.println("starting calculation");

			// First we need a range of numbers up to the number the factorial
			// of which we are calculating.

			// Grab the range as a Java Stream:
			final Stream<BigInteger> numbers = streamOfBigIntegerUpTo(facme);

			// OK. So now we have a range of numbers that need to be
			// multiplied together to arrive at the factorial. The next thing
			// we need is a binary operator to actually do the multiplication
			// as the stream is reduced.

			// You could write your own binary operator (see the mult() function
			// above). However, as it turns out, the Java system is "smart"
			// enough to know that BigInteger.multiply() can be used as a binary
			// operator, so we'll just pass in a reference to that.

			// Reduce the stream using BigInteger.multiply():
			reply = numbers.reduce(BigInteger::multiply).get();
			// System.out.println(factorial);
		}

		return reply;
	}

	public static void main(final String[] argv) {
		final BigInteger facme = new BigInteger(argv[0]);
		System.out.println(FactorialParallelStream.fac(facme));
	}

}