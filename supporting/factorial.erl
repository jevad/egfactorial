% This is just here as a "sanity check".
% This program is so simple, that I *know* 
% the algorithm is correct "by inspection",
% so I can check other algorithms to make 
% sure that the output is correct.

% .TODO. Write a factorial in Erlang or 
% Elixir that calculates in parallel, to
% compare to the Java versions.

-module(factorial).
-export([fac/1]).

fac(0) ->
	1;
fac(N) ->
	N*fac(N-1).
