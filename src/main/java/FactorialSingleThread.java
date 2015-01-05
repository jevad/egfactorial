import java.math.BigInteger;

public class FactorialSingleThread {

    public static BigInteger fac(final BigInteger facme) {
		if (null == facme || BigInteger.ZERO.compareTo(facme) > 0) {
			throw new IllegalArgumentException(
					"no factorials for negative or undefined numbers");
		}
		
        BigInteger reply = BigInteger.ONE;

        if (BigInteger.ONE.equals(facme) || BigInteger.ZERO.equals(facme)) {
            // leave as one
        } else {
            // try to force ref onto local stack
            final BigInteger ONE = BigInteger.ONE;

            BigInteger rslt = facme;
            BigInteger f = rslt.subtract(ONE);
            while (ONE.compareTo(f) < 0) {
                rslt = rslt.multiply(f);
                f = f.subtract(ONE);
            }

            // System.out.println(rslt);
            reply = rslt;
        }

        return reply;
    }

    public static void main(final String [] argv) {
        final BigInteger facme = new BigInteger(argv[0]);
        System.out.println(FactorialSingleThread.fac(facme));
    }

}