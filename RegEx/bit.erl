-module(bit).
-export([bitmask/2, bitand/2, bitor/2, bitnot/1]).


-spec bitmask(zero | one | [number()], number()) -> bitstring().

bitmask(zero, N) ->
	<<0:N>>;
bitmask(one, N) ->
	<<bnot(0):N>>;
bitmask(Positions, N) ->
	bitmask(Positions, N, <<>>).

bitmask(_, N, Acc) when bit_size(Acc) == N ->
	Acc;
bitmask([], N, Acc) ->
	bitmask([], N, <<Acc/bitstring, 0:1>>);
bitmask([K | Rest], N, Acc) when bit_size(Acc) /= K ->
	bitmask([K | Rest], N, <<Acc/bitstring, 0:1>>);
bitmask([_ | Rest], N, Acc) ->
	bitmask(Rest, N, <<Acc/bitstring, 1:1>>).


-spec bitand(bitstring(), bitstring()) -> bitstring().

bitand(<<>>, <<>>) ->
	<<>>;
bitand(<<X1:1, R1/bitstring>>, <<X2:1, R2/bitstring>>) ->
	<<(X1 band X2):1, (bitand(R1, R2))/bitstring>>.


-spec bitor(bitstring(), bitstring()) -> bitstring().

bitor(<<>>, <<>>) ->
	<<>>;
bitor(<<X1:1, R1/bitstring>>, <<X2:1, R2/bitstring>>) ->
	<<(X1 bor X2):1, (bitor(R1, R2))/bitstring>>.


-spec bitnot(bitstring()) -> bitstring().

bitnot(Bitstring) ->
	N = bit_size(Bitstring),
	<<X:N>> = Bitstring,
	<<bnot(X):N>>.




