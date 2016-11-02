-module(khf3).
-author('kovacsmartin24@gmail.com').
-vsn('2016-10-11').
%-compile(export_all).
-export([megoldase/2]).


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

% sorok listÃÂ¡jakÃÂ©nt tÃÂ¡rolt mÃÂ¡trix sorfolytonos vÃÂ¡ltozata
listaToSF([H | []]) -> H;
listaToSF([H | T]) -> 
    lists:append(H, listaToSF(T)).  
     
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

% Mátrix adott mezője
getField(Matrix, FieldR, FieldC) ->
	Row = nth(FieldR, Matrix),
	nth(FieldC, Row).

% Igaz, ha a lista összes eleme különböző
allUnique(List) ->
	SortedList = lists:sort(List),
	[H | T] = SortedList,
	allUniqueHelper(T, H).

allUniqueHelper([H|T], Prev) ->
	if 
		 H == Prev -> false;
		 true -> true andalso allUniqueHelper(T, H)
	end;
allUniqueHelper([], _) ->
	true.

% Igaz, ha a lista összes elemére igaz a predikátum
allOk([H|T], Pred) ->
	Pred(H) andalso allOk(T, Pred);
allOk([], _) ->
	true.

filterField(Matrix, Solution, R, C) ->
	Infos = getField(Matrix, R, C),
	Value = getField(Solution, R, C),
	filterByInfo(Infos, R, C, Solution, Value).

% A mező infok alapján megmondja, hogy a megoldásban szereplő érték megfelelő-e
filterByInfo([], _R, _C, _Solution, _Value) -> true;
filterByInfo([H | T], R, C, Solution, Value) ->
	if
		H == e -> (Value rem 2 =:= 0) andalso filterByInfo(T, R, C, Solution, Value);
		H == o -> (Value rem 2 =/= 0) andalso filterByInfo(T, R, C, Solution, Value);
		erlang:is_integer(H) -> H =:= Value andalso filterByInfo(T, R, C, Solution, Value);
		H == w ->
			West = getField(Solution, R, C - 1),
			((Value + West) rem 2 =/= 0) andalso filterByInfo(T, R, C, Solution, Value);
		H == s ->
			South = getField(Solution, R + 1, C),
			((Value + South) rem 2 =/= 0) andalso filterByInfo(T, R, C, Solution, Value);
		true -> true andalso filterByInfo(T, R, C, Solution, Value)
	end.

% Mátrix oszlopainak listája
getColumns(Matrix, ColumnCount) ->
	getColumns(Matrix, ColumnCount, 1).
getColumns(Matrix, ColumnCount, Iterator) ->
	if 
		Iterator =< ColumnCount ->
			Column = matrixOszlopaSL(Matrix, Iterator),
			[Column | getColumns(Matrix, ColumnCount, Iterator + 1)];
		true -> []
	end.

% Sudoku mátrix összes cellája
getCells(Matrix, CellSize) ->
	getCells(Matrix, CellSize, 1, CellSize, 1).
getCells(Matrix, CellSize, I, J, RowNum) ->
	if
		RowNum > (CellSize * CellSize) ->
			[];
		I > (CellSize * CellSize) ->
			getCells(Matrix, CellSize, 1, CellSize, RowNum + CellSize);
		true ->
			[listaToSF(getSubMatrix(Matrix, CellSize, I, J, 1, RowNum)) | getCells(Matrix, CellSize, I + CellSize, J + CellSize, RowNum)]
	end.

%% @spec khf3:megoldase(SSpec::sspec(), SSol::ssol()) -> B::bool().
%% B igaz, ha SSol megoldása az SSpec feladványnak.
megoldase({CellSize, Matrix}, Solution) ->
	checkSolutionUnique(Solution, CellSize) andalso
		checkAllFields(Matrix, Solution, CellSize).

% A megoldásban ellenőriz minden sort, oszlopot és cellát,
% hogy nincs-e bennük ismétlődő érték
checkSolutionUnique(Solution, CellSize) ->
	Rows = Solution,
	Columns = getColumns(Solution, CellSize * CellSize),
	Cells = getCells(Solution, CellSize),
	allOk(Rows, fun allUnique/1) andalso
		allOk(Columns, fun allUnique/1) andalso
		allOk(Cells, fun allUnique/1).

% A megoldás minden mezőjét ellenőrzi, hogy megfelelnek-e
% a feladványban megadott mező információknak
checkAllFields(Matrix, Solution, CellSize) ->
	checkAllFields(Matrix, Solution, CellSize, 1, 1).
checkAllFields(Matrix, Solution, CellSize, R, C) ->
	if
		R > (CellSize * CellSize) -> true;
		C > (CellSize * CellSize) -> 
			checkAllFields(Matrix, Solution, CellSize, R + 1, 1);
		true -> 
			filterField(Matrix, Solution, R, C) andalso
				checkAllFields(Matrix, Solution, CellSize, R, C + 1)
	end.



getAllPossibleValues(Matrix, CellSize) -> true.
			