-module(khf2).
-author('kovacsmartin24@gmail.com').
-vsn('2016-10-11').
-export([ertekek/2]).
-compile(export_all).

% Sorok listÃÂ¡jakÃÂ©nt tÃÂ¡rolt mÃÂ¡trix i. sorÃÂ¡nak elÃÂallÃÂ­tÃÂ¡sa.
matrixSoraSL(M, I) ->
        nth(I, M).

% Sorok listÃÂ¡jakÃÂ©nt tÃÂ¡rolt mÃÂ¡trix j. oszlopÃÂ¡nak elÃÂÃÂ¡llÃÂ­tÃÂ¡sa.        
matrixOszlopaSL(M, J) ->
        [nth(J, X) || X <- M].

% lista N-edik eleme
nth(_, []) -> error;
nth(1, [H|_]) -> H;
nth(N, [_|T]) -> nth(N-1, T).

szelet(L, I, J) ->
    szelet(L, I, J, 1).
    
% Lista szeletÃÂ©nek (i. ÃÂ©s j. sorszÃÂ¡mÃÂº elemek kÃÂ¶zÃÂ¶tti rÃÂ©szÃÂ©nek) elÃÂÃÂ¡llÃÂ­tÃÂ¡sa. 
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
% Xs2 halmaz az Xs halmaz Ã©s az [X] halmaz uniÃ³ja.
newMember(X, Set) ->
	case isMember(X, Set) of
		true -> Set;
		false -> [X | Set]
	end.

% @spec union(Xs::set(), Ys::set()) -> Zs::set(). % Zs az Xs Ã©s Ys halmazok uniÃ³ja. 
union2(Xs, Ys) ->
	lists:foldr(fun newMember/2, Ys, Xs).

% sorok listÃÂ¡jakÃÂ©nt tÃÂ¡rolt mÃÂ¡trix sorfolytonos vÃÂ¡ltozata
listaToSF([H | []]) -> H;
listaToSF([H | T]) -> 
    lists:append(H, listaToSF(T)).

%% @type matrix() = [row()].
%% @type row() = [any()].
%% @type parameter() = {subRows(), subCols()}.
%% @type subRows() = integer().
%% @type subCols() = integer().
%% @spec khf1:feldarabolasa(Mx::matrix(), P::parameter()) -> LL::[[any()]].
%%   Az LL lista az Mx mÃÂ¡trix P paramÃÂ©terÃÂ± feldarabolÃÂ¡sa.
feldarabolasa(M, RC) ->
    {R, C} = RC,
    W = length(nth(1, M)),
    H = length(M),
    feldarabolasa(M, R, C, 1, 1, W, H).

feldarabolasa(M, R, C, ActColumn, ActLine, Width, Height) -> 
    case ActColumn of
        Col when Col > Width -> []; % vÃÂ©gÃÂ©re ÃÂ©rtÃÂ¼nk, nincs tÃÂ¶bb oszlop
        _ ->
            case ActLine of
                Line when Line > Height -> feldarabolasa(M, R, C, ActColumn + C, 1, Width, Height);
                Line when Line =< Height -> 
                    SubMatrix = listaToSF(getSubMatrix(M, R, ActColumn, ActColumn + (C - 1), 1, ActLine)),
                    [SubMatrix | feldarabolasa(M, R, C, ActColumn, ActLine + R, Width, Height)]
            end
    end.    
     
% M mÃÂ¡trixbÃÂ³l kivÃÂ¡g egy szeletet, RAct a kezdÃÂsor, egy sorban
% az I-J-ig vÃÂ¡g, R db sorbÃÂ³l
% az eredmÃÂ©ny egy R X (I-J) mÃÂ¡trix    
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

%Egy sorok listájaként tárolt mátrix adott mezőjét befoglaló
% k-méretű cella elemeit tartalmazó lista előállítása.
getCellAccordingToIndex(Matrix, CellSize, FieldR, FieldC) ->
	CellI = ((FieldC - 1) div CellSize) * CellSize + 1,
	CellJ = ((FieldR - 1) div CellSize) * CellSize + 1,
	listaToSF(getSubMatrix(Matrix, CellSize, CellI, CellI + CellSize - 1, 1, CellJ)).	

%% @type col() = integer().
%% @type row() = integer().
%% @type coords() = {row(),col()}.
%% @spec khf2:ertekek(SSpec::sspec(), R_C::coords()) -> Vals::[integer()]
%%   Egy érték pontosan akkor szerepel a Vals listában, ha teljesíti a
%%   fenti Prolog specifikációban felsorolt (a), (b) és (c) feltételeket, ahol
%%   Vals az SSpec specifikációval megadott Sudoku-feladvány R_C
%%   koordinátájú mezőjében megengedett értékek listája.
ertekek({CellSize, Matrix}, {FieldR, FieldC}) ->
	FieldInfos = getField(Matrix, FieldR, FieldC),
	TempRow = matrixSoraSL(Matrix, FieldR),
	Row = lists:delete(FieldInfos, TempRow), % A kérdéses mezőt nem akarjuk beletenni
	TempColumn = matrixOszlopaSL(Matrix, FieldC),
	Column = lists:delete(FieldInfos, TempColumn),
	TempCell = getCellAccordingToIndex(Matrix, CellSize, FieldR, FieldC),
	Cell = lists:delete(FieldInfos, TempCell),
	UsedValues = getUsedValues(Row, Column, Cell),
	PossibleValues = lists:seq(1, CellSize * CellSize),
	FilteredValues = filterValues(PossibleValues, FieldInfos),
	FilteredValues -- UsedValues.
	
getUsedValues(Row, Column, Cell) ->
	Temp = union2(Row, Column),
	Values = union2(Temp, Cell),
	lists:map(fun getIntegerFromField/1, Values).
	
getIntegerFromField([]) ->
	[];
getIntegerFromField([Head | Tail]) ->
	if
		erlang:is_integer(Head) -> Head;
		true -> getIntegerFromField(Tail)
	end.

getField(Matrix, FieldR, FieldC) ->
	Row = nth(FieldR, Matrix),
	nth(FieldC, Row).

filterByInfo([], _Value) ->
	true;
filterByInfo([H | T], Value) ->
	if 
		H == e -> (Value rem 2 =:= 0) andalso filterByInfo(T, Value);
		H == o -> (Value rem 2 =/= 0) andalso filterByInfo(T, Value);
		erlang:is_integer(H) -> H =:= Value andalso filterByInfo(T, Value);
		true -> true andalso filterByInfo(T, Value)
	end.

filterValues(Values, Infos) ->
	[X || X <- Values, filterByInfo(Infos, X)].
