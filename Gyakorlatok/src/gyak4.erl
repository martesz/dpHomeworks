-module(gyak4).
-compile(export_all).

seq(Begin, End) ->
	lists:reverse(seq(Begin, End, [])).
seq(Begin, End, Result) ->
	if
		Begin =< End -> seq(Begin + 1, End, [Begin | Result]);
		true -> Result
	end.

zip([],[]) -> [];
zip([Head1 | Tail1], [Head2 | Tail2]) ->
	[{Head1, Head2} | zip(Tail1, Tail2)].

unzip(L) ->
	unzip(L, [], []).
unzip([], L1, L2) -> {lists:reverse(L1), lists:reverse(L2)};
unzip([{A , B} | Tail], L1, L2) ->
	unzip(Tail, [A | L1], [B | L2]).
	
flatten([]) -> [];
flatten([Head | Tail]) ->
	if
		is_list(Head) -> flatten(Head) ++ flatten(Tail);
		true -> [Head | flatten(Tail)]
	end.

kozepe(Matrix) ->
	[lists:sublist(Row, length(Row) div 4 + 1, length(Row) div 2) ||
		 Row <- lists:sublist(Matrix, length(Matrix) div 4 +1, length(Matrix) div 2)].

all_different([_]) -> true;
all_different([]) -> true;
all_different([H | T]) ->
	not lists:member(H, T) andalso all_different(T).

duplak([]) -> [];
duplak([H, H| T]) ->
	[H | duplak([H|T])];
duplak([_H | T]) ->
	duplak(T).

duplak2([]) -> [];
duplak2(L) -> 
	[ E || {E, E} <- zip(lists:sublist(L, length(L) - 1), tl(L))].

