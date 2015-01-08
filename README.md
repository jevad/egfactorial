**update:**  I will be giving a talk on Java 8 parallel streaming and the parallel performance of Oracle's JVM at the January Software Developer's Cartel.  So, over the next few week, I'll be adding some stuff here, including relative performance data and more commentary/annotations to get ready for the presenation.  I'll also put my slides up here or in another github project.

For those of you who are local, the January Software Developer's Cartel will be January 20th at the Grand Stafford Theater in downtown Bryan.  I'd enjoy seeing you there.  Admission is free, snacks are usually provided (free), and the bar will be open for drinks (*not* free -- and don't forget to tip your bartender).  Doors will open 4:30-ish and the talks will start at 6:30-ish (there are usually two or three talks, with each talk lasting about a half hour).  I'll modify this information when more details are available.  As always, Grand Stafford Bar Manager and Chief Mixologist, Cody Schilling, will have some special Tuesday evening craft cocktails planned -- I'm sure my presentation will sound much better if you get there early and consume a few delicious cocktails prior to the talk.  :)

# Using the new Java parallel streaming classes

On Christmas Day and on New Year's Day, in between visiting with family and friends, I created some code to try out the new Java 8 parallel streaming functionality.  I decided to try to calculate factorials as fast as I could in Java on my laptop.  You'll find that code in `FactorialParallelStream.java`.

It may not be obvious exactly what that code is doing because, well, in Java, it seems that not much ever seems to be obvious.  So, first, let's look at some Python code because, for some reason, it seems that, in Python, everything is obvious.

In the the `supporting` directory, you'll find `factorial.py`, and in that file, you'll find a function, `range_factorial`.  That's what we want to do with Java -- operate on a range of numbers to calculate the factorial.  But, with Java, we'll also be getting the advantage of parallel processing, as we'll see.

(If you're wondering why my Python code uses `islice(count(1, 1), some_number)` instead of `range(1, some_number)` or `xrange(1, some_number)`, the reasons are as follows:  In Python 2.7.x, `range` actually instantiates a list, which, if we're seeking the factorial of a large number, would not be ideal.  And the documentation for `xrange` indicates that some implementations might only allow ranges up to the size of a C long.  So, as recommended in the `xrange` docs, I chose to use `islice` and `count` to get the range I needed.  (I **think**, in Python 3.x, `range` returns a stream, so this might not be a problem in Python 3.x, but I'm not an expert on Python by any means.))

Now, if you look at `FactorialParallelStream.java` again, it should be more obvious what is going on.

Unfortunately, Java doesn't provide a range of `BigInteger` in the standard library.  To make one, I used a utility function in the standard library, `StreamSupport.stream`:

```java
	private static Stream<BigInteger> streamOfBigIntegerUpTo(
			final BigInteger facme) {
		return StreamSupport.stream(new BigIntegerSpliterator(BigInteger.ONE,
				facme), true);
	}
```

The second argument, when set to `true`, automatically adds parallelization to the stream.  Cool, huh?

To use this method, you need to provide an implementation of `Spliterator`, which is what actually serves up the items being streamed.  So the bulk of the work in this project was creating a `Spliterator` that returns a sequence of `BigInteger`, which you can find in the file, `BigIntegerSpliterator.java`.  As you can see, it's a bit of work, but, at 220 lines of code, including some documentation, it's not that bad (although, note that I didn't write any unit tests for my `Spliterator` -- writing unit tests for a `Spliterator` potentially could be a pain).  

The standard library does provide some other ways of generating ranges that require less work, but none actually worked in this case.  You'll see two uses of the `Stream.iterate` method, which returns a `Stream`, in `FactorialParallelStream.java`, in the methods `upToMaxLong` and `takesForever`.  The `Stream` produced by `upToMaxLong` works fine; however, if you wanted to calculate the factorial of a number larger than `Long.MAX_LONG`, it wouldn't work.  The `Stream` produced by `takesForever` actually works perfectly; however, it results in code that runs forever because the stream doesn't cut off when the filter fails - it keeps running forever, testing every value in the infinite stream (and this is good because that is actually the desired behavior of a filter).

As a sanity check, I also wrote a straightforward implementation of factorial (in `FactorialSingleThread.java`), so I could be sure I was calculating the correct values.  I then ran a time test, and I felt the performance was good.  Calculating the factorial of 100000, 100 times, took about 20 seconds on my laptop (a 2x2 core machine).  By way of comparison, a straighforward implementation in C, when timed, took about 80 seconds on the same machine (See factorial.c in the supporting directory for my implementation, which uses GMP, [https://gmplib.org](https://gmplib.org) for the "big integer" functionality.  Sorry about the quality of my C code -- this is the first C or C++ program I've written in more than two years and only the fifth I've written since 2002.).

So, now, my goal was to see if I could beat this performance using only pre-Java 8 code.  You can find my two attempts in `FactorialSkipAlgorithm.java` and `FactorialFlexSplit.java`.  Both take advantage of the `ForkJoinPool` added to the Java standard library in Java 7.  

Long story short, `FactorialSkipAlgorithm`, which I **thought** would be the fastest couldn't compete at all.  However, for larger calculations (say, for calculating factorials of numbers above 20000), `FactorialFlexSplit` was able to beat the Java 8 parallel streaming implementation.  Calculating the factorial of 100000, 100 times, took about 12 seconds for `FactorialFlexSplit` and about 45 seconds for `FactorialSkipAlgorithm`.  

Surprisingly, `FactorialFlexSplit` does this by forking off a huge number of threads.  It will generate thousands of forks to calculate the factorial of 100000.  Likewise, as far as I can tell, Java's parallel streaming generates many threads.  It appears that the JVM now has an Erlang-like ability to fork, juggle and join thousands of threads efficiently.  

Next up:

* Re-implement the Python implementation using the Disco framework to see how it peforms ( [http://discoproject.org](http://discoproject.org) ).  (note:  Although the Disco framework is written for Python, it is written *in* Erlang and runs on the Erlang VM.) 

* Up the number of threads used in `FactorialSkipAlgorithm` to see what happens. 

* Learn enough Erlang or Elixir to create an implementation on the Erlang VM and see how the JVM and Erlang VM compete with one another.

* Julia, [http://julialang.org/](http://julialang.org/), claims to be a high performance, dynamic language with great parallel capabilities.  Try out Julia to see how it compares.
