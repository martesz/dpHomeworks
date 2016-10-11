%% @author martin
%% @doc @todo Add description to khf2.


-module(khf2).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-compile(export_all).




%% ====================================================================
%% Internal functions
%% ====================================================================

% lista N-edik eleme
nth(_, []) -> error;
nth(1, [H|_]) -> H;
nth(N, [_|T]) -> nth(N-1, T).

szelet(L, I, J) ->
    szelet(L, I, J, 1).
    
% Lista szeletének (i. és j. sorszámú elemek közötti részének) előállítása. 
szelet([Fej|Farok], I, J, Index) ->
    case Index of
        X when X < I -> szelet(Farok, I, J, Index + 1);
        X when X >= I, X =< J -> [Fej | szelet(Farok, I, J, Index + 1)];
        _ -> []
    end;
szelet([], _I, _J, _Index) -> [].

% isMember(X, Ys) igaz, ha az X elem benne van az Ys halmazban.
isMember(_, []) ->
	false;
isMember(X, [Head | Tail]) ->
	X =:= Head orelse isMember(X, Tail).

% @spec newMember(X::any(), Xs::set()) -> Xs2::set().
% Xs2 halmaz az Xs halmaz �s az [X] halmaz uni�ja.
newMember(X, Set) ->
	case isMember(X, Set) of
		true -> Set;
		false -> [X | Set]
	end.

% sorok listájaként tárolt mátrix sorfolytonos változata
listaToSF([H | []]) -> H;
listaToSF([H | T]) -> 
    lists:append(H, listaToSF(T)).

%% @type matrix() = [row()].
%% @type row() = [any()].
%% @type parameter() = {subRows(), subCols()}.
%% @type subRows() = integer().
%% @type subCols() = integer().
%% @spec khf1:feldarabolasa(Mx::matrix(), P::parameter()) -> LL::[[any()]].
%%   Az LL lista az Mx mátrix P paraméterű feldarabolása.
feldarabolasa(M, RC) ->
    {R, C} = RC,
    W = length(nth(1, M)),
    H = length(M),
    feldarabolasa(M, R, C, 1, 1, W, H).

feldarabolasa(M, R, C, ActColumn, ActLine, Width, Height) -> 
    case ActColumn of
        Col when Col > Width -> []; % végére értünk, nincs több oszlop
        _ ->
            case ActLine of
                Line when Line > Height -> feldarabolasa(M, R, C, ActColumn + C, 1, Width, Height);
                Line when Line =< Height -> 
                    SubMatrix = listaToSF(getSubMatrix(M, R, ActColumn, ActColumn + (C - 1), 1, ActLine)),
                    [SubMatrix | feldarabolasa(M, R, C, ActColumn, ActLine + R, Width, Height)]
            end
    end.    
     
% M mátrixból kivág egy szeletet, RAct a kezdősor, egy sorban
% az I-J-ig vág, R db sorból
% az eredmény egy R X (I-J) mátrix    
getSubMatrix(M, R, I, J, RIndex, RAct) ->
    Line = nth(RAct, M),
    case Line of
        error -> [];
        _ ->
            case RIndex of
                X when X < R -> [szelet(Line, I, J) | getSubMatrix(M, R, I, J, RIndex + 1, RAct + 1)];
                X when X =:= R -> [szelet(Line, I, J)]
            end
    end.
