-module(fac_impl).
-export([calc_and_die/0, fac/1, time_fac/2, time_fac/1]).
% -export([split_the_work_strategy/2, calc_and_die/0, fac_a/1, fac/1, time_fac/2, time_fac/1]).

product_impl(First, Last) ->
    lists:foldl(fun(X, Prod) -> X * Prod end, 1, lists:seq(First, Last)).

split_the_work_strategy("A", {First, Mid, Last, Limit, Recur_f}) ->
    Pid_a = spawn(fac_impl, calc_and_die, []),
    Pid_a ! {self(), {product, First, Mid, Limit, "A"}},
    Half = Recur_f({Mid + 1, Last, Limit, "A"}),
    receive
        Response_a ->
            Response_a * Half
    end;
split_the_work_strategy("B", {First, Mid, Last, Limit, Recur_f}) ->
    % An example of receiving more than one response.
    % Receives two responses.  We don't need to be able to distinguish between
    % the responses (in fact, it works to our advantage if we don't); however,
    % if you did, you would have to distinguish the responses by some value
    % passed back.  
    Pid_a = spawn(fac_impl, calc_and_die, []),
    Pid_b = spawn(fac_impl, calc_and_die, []),
    Pid_a ! {self(), {product, First, Mid, Limit, "B"}},
    Pid_b ! {self(), {product, Mid + 1, Last, Limit, "B"}},
    Half = receive
        Response_a ->
            Response_a
    end,
    receive
        Response_b ->
            Response_b * Half
    end;   
split_the_work_strategy(Other, {}) ->
    {error, Other}.

fac_a({First, Last, Limit, SplitStrategy}) ->
    Prod_sz = Last - First,
    if 
        0 > Prod_sz ->
            {error, "foobar(a)"};
        0 == Prod_sz ->
            Last;
        1 == Prod_sz ->
            First * Last;
        2 == Prod_sz ->
            First * (1 + First) * Last;
        Limit >= Prod_sz ->
            product_impl(First, Last);
        true ->
            Mid = First + trunc(Prod_sz/2),
            split_the_work_strategy(SplitStrategy, {First, Mid, Last, Limit, fun fac_a/1})
    end.

calc_and_die() ->
    receive
        {From, {product, First, OnePastLast, Limit, SplitStrategy}} ->
            From ! fac_a({First, OnePastLast, Limit, SplitStrategy});
        {From, Other} ->
            From ! {error, Other}
    end.

fac(SomeNumber, SplitStrategy) ->
    if 
        0 > SomeNumber ->
            {error, "Dude, taking the factorial of a negative number just ain't right!"};
        0 == SomeNumber ->
            1;  % by defnition of factorial
        true ->
            fac_a({1, SomeNumber, 4, SplitStrategy})
    end.

% entry point for testing correctness from the shell.
% strategy A is the default
fac(SomeNumber) ->
    fac(SomeNumber, "A").

% This pulls the number of milliseconds from an Erlang time struct.
millisecs(Ts) ->
    {MegaSecs, Secs, MicroSecs} = Ts,
    (MicroSecs/1000) + (Secs*1000) + (MegaSecs*(1000*1000)).

% When calling from the command line, atoms or strings will be 
% passed in.  We need integers.
w_as_integer(HopefullyInteger) ->
    if
        is_integer(HopefullyInteger) ->
            HopefullyInteger;
        is_atom(HopefullyInteger) ->
            list_to_integer(atom_to_list(HopefullyInteger));
        true -> % assuming it's a string at this point
            list_to_integer(HopefullyInteger)
    end.

% calculate the factorial of SomeNumber, HowManyTimes,
% using paralle processing with splitting strategy, SomeStrategy.
% Print out some data, including the amount of time it took.
time_fac_impl(SomeNumber, HowManyTimes, SomeStrategy) ->
    % This is probably not the best way to calculate time 
    % for Erlang runs, but it does allow comparisons with 
    % other systems.
    % You could also run with timer.tc(), but I wanted to embed
    % the timing.
    Start = os:timestamp(),
    % emits warning because X is unused
    lists:foreach(fun(X) -> fac(SomeNumber) end, lists:seq(1, HowManyTimes)),
    End = os:timestamp(),

    % io:format("~w ~w~n", [Start, End]),
    Elapsed = millisecs(End) - millisecs(Start),
    io:format("time for Erlang, using strategy ~w, to calculate ~w factorial ~w times: ~w.~n", [SomeStrategy, SomeNumber, HowManyTimes, Elapsed]).  

% entry point if you call this from the Erlang shell
time_fac(SomeNumberX, HowManyTimesX) ->
    SomeNumber = w_as_integer(SomeNumberX),
    HowManyTimes = w_as_integer(HowManyTimesX),

    Test_a = fac(SomeNumber, "A"),
    Test_b = fac(SomeNumber, "B"),

    if  
        % run a sanity check to make sure both algorithms return the same result
        (Test_a == Test_b) ->
            time_fac_impl(SomeNumber, HowManyTimes, "A"),
            time_fac_impl(SomeNumber, HowManyTimes, "B");
        true ->
            io:format("sanity check failed! ~w ~w~n", [Test_a, Test_b])
    end.

% entry point if you call this from a command line
time_fac([SomeNumberX, HowManyTimesX]) ->
    time_fac(SomeNumberX, HowManyTimesX).  
