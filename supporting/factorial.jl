# the built in factorial function
# used for sanity checks and so forth
function house_fac(some_number::BigInt)
  return factorial(some_number)
end

# a simple looping factorial calculator
# used for comparison
function simple_fac(some_number::BigInt)
  fac = BigInt(1)
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
  fac = @parallel (*) for n = BigInt(1):some_number
    temp = n
  end
  return fac
end

# This divides the work and spawns off said work onto different
# threads, until the amount of work gets small enough.
function calc_fac_impl(first::BigInt, last::BigInt, chk_sz::BigInt)
  ZERO = BigInt(0)
  ONE = BigInt(1)
  TWO = BigInt(2)
  THREE = BigInt(3)

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
    if first <= mid && (ONE + mid) <= last
      f = @spawn calc_fac_impl(first, mid, chk_sz)
      g = @spawn calc_fac_impl((ONE + mid), last, chk_sz)
      return fetch(f)*fetch(g)
    else
      throw(ArgumentError("something amiss, $first, $inc, $last"))
    end
  end
end

function calc_fac(some_number::BigInt)
  fac = BigInt(1) # default result, for 0 and 1, by definition
  if BigInt(0) > some_number
    throw(DomainError("Must be non-negative! $some_number"))
  elseif BigInt(2) <= some_number
    chk_sz = BigInt(1625)
    fac = calc_fac_impl(BigInt(1), some_number, chk_sz)
  end
  return fac
end

function spawn_fac_impl(iterations, fac_me::BigInt)
  for n = 1:iterations
    calc_fac(fac_me)
  end
end


function spawn_fac(iterations, fac_me::BigInt)
  if (calc_fac(fac_me) == house_fac(fac_me)) && (0 < iterations)
    @time(spawn_fac_impl(iterations, fac_me))
  else
    throw(ArgumentError("calculation problem! $iterations, $fac_me"))
  end
end

iterations = parseint(ARGS[1])
fac_me = BigInt(ARGS[2])
spawn_fac(iterations, fac_me)
