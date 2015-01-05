# Using the new Java parallel streaming classes

On Christmas and New Year's Day, in between visiting with family, I created some code to try out the 
new Java 8 parallel streaming functionality.  I decided to try to calculate factorials as fast as I
could in Java on my laptop.  You'll find that code in FactorialParallelStream.java.

It may not be obvious exactly what that code is doing because, well, in Java, not much ever seems to 
be obvious.  So, first, let's look at some Python code because, in Python, everything is obvious.

In the the `supporting` directory, you'll find `factorial.py`, and in that file, 
you'll find a function, `range_factorial`.  That's what we want to do with Java --
operate on a range of numbers to calculate the factorial.  But, with Java, we'll also 
be getting the advantage of parallel processing, as we'll see.

(If you're wondering why my Python code uses `islice(count(1, 1), some_number)` instead of 
`range(1, some_number)` or 'xrange(1, some_number', the reasons are as follows:  In Python 2.7.x,
`range` actually instantiates a list, which, if we're seeking the factorial of a large number,
would not be ideal.  And the documentation for `xrange` indicates that some implementations might
only go up to the size of a C long.  So, as recommended in the `xrange`, I chose to use
islice and count to get the range I needed.)

Now, if you look at FactorialParallelStream.java again, it should be more obvious what is going on.

Unfortunately, Java doesn't provide a range of `BigInteger` in the standard library.  To make one,
I used a utility function in the standard library, `StreamSupport.stream`:

```java
	private static Stream<BigInteger> streamOfBigIntegerUpTo(
			final BigInteger facme) {
		return StreamSupport.stream(new BigIntegerSpliterator(BigInteger.ONE,
				facme), true);
	}
```

The second argument, when set to `true`, automatically adds parallelization to the stream.  Cool, huh?
To use this method, you need to provide a Spliterator.  So the bulk of the work in this project was 
creating a Spliterator that returns a sequence of `BigInteger`, which you can find in the file 
BigIntegerSpliterator.java.  As you can see, it's a bit of work, but, at 220 lines of code, with some
documentation, it's not that bad.  

The standard library does provide some other ways of generating ranges that require less work, but none
actually worked in this case.  You'll see two uses of the `Stream.iterate` method, which returns a stream,
in FactorialParallelStream.java, in the methods `upToMaxLong` and `takesForever`.  The stream produced
by `upToMaxLong` works fine; however, if you wanted to calculate the factorial of a number larger than
Long.MAX_LONG, it wouldn't work.  The stream produced by `takesForever` actually works perfectly; however,
it results in code that runs forever because the stream doesn't cut off when the filter fails - it keeps
running forever, testing every value in the infinite stream (and this is good because that is actually
the desired behavior of filter).

As a sanity check, I also wrote a straightforward implementation of factorial (in FactorialSingleThread.java),
so I could be sure I was calculating the correct values.  I then ran a time test, and I felt the performance
was good.  Calculating the factorial of 100000, 100 times, took about 20 seconds on my laptop (a 2x2 core
machien).  By way of comparison, a straighforward implementation in C, when timed, took about 80 seconds
on the same machine (see factorial.c in the supporting directory for my implementation).

So, now, my goal was to see if I could beat this number using only pre-Java 8 code.  You can find my two 
attempts in FactorialSkipAlgorithm.java and FactorialFlexSplit.java.  Both take advantage of the ForkJoinPool
added to the Java standard library in Java 7.  

Long story short, FactorialSkipAlgorithm couldn't compete.  However, for larger calculations (say, 
for calculating factorials of numbers above 20000), FactorialFlexSplit was able to beat the Java 8
parallel streaming implementation.  Calculating the factorial of 100000, 100 times, took about 
12 seconds for FactorialFlexSplit and about 45 seconds for FactorialSkipAlgorithm.  

Surprisingly, FactorialFlexSplit does this by forking off a huge number of threads.  It will generate
thousands to calculate the factorial of 100000.  Likewise, as far as I can tell, Java's parallel streaming
generates many threads.  It appears that the JVM now has an Erlang-like ability to fork, juggle and joing
thousands of threads efficiently.  

Next up:

* re-implement that Python implementation using the Disco framework to see how it peforms 
( [http://discoproject.org](http://discoproject.org) ).

* Up the number of threads on FactorialSkipAlgorithm to see what happens.


