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

bitand(Bin1, Bin2) ->
	N = bit_size(Bin1),
	<<X1:N>> = Bin1,
	<<X2:N>> = Bin2,
	<<(X1 band X2):N>>. 


-spec bitor(bitstring(), bitstring()) -> bitstring().

bitor(Bin1, Bin2) ->
	N = bit_size(Bin1),
	<<X1:N>> = Bin1,
	<<X2:N>> = Bin2,
	<<(X1 bor X2):N>>. 


-spec bitnot(bitstring()) -> bitstring().

bitnot(Bitstring) ->
	N = bit_size(Bitstring),
	<<X:N>> = Bitstring,
	<<bnot(X):N>>.




