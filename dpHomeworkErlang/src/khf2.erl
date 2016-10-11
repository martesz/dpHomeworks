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

% Sorok listÃ¡jakÃ©nt tÃ¡rolt mÃ¡trix i. sorÃ¡nak elÅallÃ­tÃ¡sa.
matrixSoraSL(M, I) ->
        nth(I, M).

% Sorok listÃ¡jakÃ©nt tÃ¡rolt mÃ¡trix j. oszlopÃ¡nak elÅÃ¡llÃ­tÃ¡sa.        
matrixOszlopaSL(M, J) ->
        [nth(J, X) || X <- M].

% lista N-edik eleme
nth(_, []) -> error;
nth(1, [H|_]) -> H;
nth(N, [_|T]) -> nth(N-1, T).

szelet(L, I, J) ->
    szelet(L, I, J, 1).
    
% Lista szeletÃ©nek (i. Ã©s j. sorszÃ¡mÃº elemek kÃ¶zÃ¶tti rÃ©szÃ©nek) elÅÃ¡llÃ­tÃ¡sa. 
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
% Xs2 halmaz az Xs halmaz és az [X] halmaz uniója.
newMember(X, Set) ->
	case isMember(X, Set) of
		true -> Set;
		false -> [X | Set]
	end.

% @spec union(Xs::set(), Ys::set()) -> Zs::set(). % Zs az Xs és Ys halmazok uniója. 
union2(Xs, Ys) ->
	lists:foldr(fun newMember/2, Ys, Xs).

% sorok listÃ¡jakÃ©nt tÃ¡rolt mÃ¡trix sorfolytonos vÃ¡ltozata
listaToSF([H | []]) -> H;
listaToSF([H | T]) -> 
    lists:append(H, listaToSF(T)).

%% @type matrix() = [row()].
%% @type row() = [any()].
%% @type parameter() = {subRows(), subCols()}.
%% @type subRows() = integer().
%% @type subCols() = integer().
%% @spec khf1:feldarabolasa(Mx::matrix(), P::parameter()) -> LL::[[any()]].
%%   Az LL lista az Mx mÃ¡trix P paramÃ©terÅ± feldarabolÃ¡sa.
feldarabolasa(M, RC) ->
    {R, C} = RC,
    W = length(nth(1, M)),
    H = length(M),
    feldarabolasa(M, R, C, 1, 1, W, H).

feldarabolasa(M, R, C, ActColumn, ActLine, Width, Height) -> 
    case ActColumn of
        Col when Col > Width -> []; % vÃ©gÃ©re Ã©rtÃ¼nk, nincs tÃ¶bb oszlop
        _ ->
            case ActLine of
                Line when Line > Height -> feldarabolasa(M, R, C, ActColumn + C, 1, Width, Height);
                Line when Line =< Height -> 
                    SubMatrix = listaToSF(getSubMatrix(M, R, ActColumn, ActColumn + (C - 1), 1, ActLine)),
                    [SubMatrix | feldarabolasa(M, R, C, ActColumn, ActLine + R, Width, Height)]
            end
    end.    
     
% M mÃ¡trixbÃ³l kivÃ¡g egy szeletet, RAct a kezdÅsor, egy sorban
% az I-J-ig vÃ¡g, R db sorbÃ³l
% az eredmÃ©ny egy R X (I-J) mÃ¡trix    
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

% Egy sorok listájaként tárolt mátrix adott mezőjét befoglaló
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
	Row = matrixSoraSL(Matrix, FieldR),
	Column = matrixOszlopaSL(Matrix, FieldC),
	Cell = getCellAccordingToIndex(Matrix, CellSize, FieldR, FieldC),
	UsedValues = getUsedValues(Row, Column, Cell),
	PossibleValues = lists:seq(1, CellSize * CellSize),
	Field = getField(Matrix, FieldR, FieldC)
	
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

