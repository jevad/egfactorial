const ZERO = BigInt(0)
const ONE = BigInt(1)
const TWO = BigInt(2)
const THREE = BigInt(3)

# the built in factorial function
# used for sanity checks and so forth
function house_fac(some_number::BigInt)
  return factorial(some_number)
end

# a simple looping factorial calculator
# used for comparison
function simple_fac(some_number::BigInt)
  fac = ONE
  if 0 > some_number
    throw(DomainError("Not in my house! $some_number"))
  elseif 2 <= some_number
    i = BigInt(1)
    while i <= some_number
      fac = fac*i
      i = i + 1
    end
  end
  return fac
end

# So, in principle, one should be able to calculate
# factorial like this, using parallel streaming,
# but, unfortunately, this fails due to limitations
# of the underlying libraries (as of Julia 0.3.5).
function pfor_fac(some_number::BigInt)
  fac = @parallel (*) for n = ONE:some_number
    temp = n
  end
  return fac
end

# If you want to limit yourself to calculating factorials for numbers
# up to the size of the system-defined Int, you can do something
# like this.  I tried it out, and it was slower by an order of magnitude
# than the split-and-spawn algorithm in the next function.  I suspect that
# the underlying system is splitting the work suboptimally.
function pfor_fac_ltd(some_number)
  fac = @parallel (*) for n = 1:some_number
    temp = BigInt(n)
  end
  return fac
end

# This divides the work and spawns off said work onto different
# threads, until the amount of work gets small enough.
function calc_fac_impl(first::BigInt, last::BigInt, chk_sz::BigInt)
  diff = last - first
  if ZERO > diff
    throw(ArgumentError("First greater than Last! $first, $last"))
  elseif ZERO == diff
    return first
  elseif ONE == diff
    return first * last
  elseif TWO == diff
    return first * (ONE + first) * last
  elseif THREE == diff
    return first * (ONE + first) * (TWO + first) * last
  elseif diff <= chk_sz
    prod = first
    bgn = ONE + first
    for n = bgn:last
      prod = prod*n
    end
    return prod
  else
    (q,r) = divrem(diff, TWO)
    mid = first + q
    if ZERO != r
      mid = mid + ONE
    end
    f = @spawn calc_fac_impl(first, mid, chk_sz)
    g = @spawn calc_fac_impl((ONE + mid), last, chk_sz)
    return fetch(f)*fetch(g)
  end
end

function calc_fac(some_number::BigInt)
  fac = ONE # default result, for 0 and 1, by definition
  if BigInt(0) > some_number
    throw(DomainError("Must be non-negative! $some_number"))
  elseif TWO <= some_number
    # I played around with adjusting the chunk size.  On my system,
    # this value produced best or near-best results.
    chk_sz = BigInt(1625)
    fac = calc_fac_impl(ONE, some_number, chk_sz)
  end
  return fac
end

function spawn_fac(iterations, fac_me::BigInt, calc_rslt)
  if (calc_fac(fac_me) == calc_rslt && (0 < iterations))
    print("timing spawn implementation: ")
    @time(for n = 1:iterations calc_fac(fac_me) end)
  else
    throw(ArgumentError("calculation problem! $iterations, $fac_me"))
  end
end

iterations = parseint(ARGS[1])
fac_me = BigInt(ARGS[2])
hf = house_fac(fac_me)
spawn_fac(iterations, fac_me, hf)
