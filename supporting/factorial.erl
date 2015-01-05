% This is just here as a "sanity check".
% This program is so simple, that *know* 
% the algorithm is correct "by inspection",
% so I can check other algorithms to make 
% sure that the output is correct.

% copyright/rights: none.  The author is
% assuming that this is so obvious
% that it is 'public domain'.

-module(factorial).
-export([fac/1]).

fac(0) ->
	1;
fac(N) ->
	N*fac(N-1).
