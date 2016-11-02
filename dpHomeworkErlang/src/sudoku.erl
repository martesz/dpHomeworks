-module(sudoku).
-author('kovacsmartin24@gmail.com').
-vsn('2016-10-11').
-compile(export_all).
-export([megoldase/2]).

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

% parameters: Matrix ami az infókat tartalmazza, Cellaméret
% return: Mátrix, sorok listájaként,
% amiben minden mező a lehetséges értékek listája
getAllPossibleValues(Matrix, CellSize) ->
	Max = CellSize * CellSize,
	getAllPossibleValues(Matrix, CellSize, Max, 1, Max, []).

% Soronként bejárva a mátrixot összegyűjti a lehetséges értékeket
getAllPossibleValues(Matrix, CellSize,Max, R, C, RowAcc) ->
	if
		% Nincs több sor
		R > Max -> [];
		% Sor elejére értünk, fordítva járom be a sorokat,
		% hogy az akkumulátorban ne kelljen fordítgatni
		C < 1 -> 
			[RowAcc | getAllPossibleValues(Matrix, CellSize, Max, R + 1, Max, [])];
		true ->
			FieldValues = ertekek({CellSize, Matrix}, {R, C}),
			getAllPossibleValues(Matrix, CellSize,Max, R, C - 1, [FieldValues | RowAcc])
	end.

getShortestValuesList(Matrix, Max) ->
	getShortestValuesList(Matrix, Max, 1, 1, {999999, 0, 0, []}).

getShortestValuesList(Matrix, Max, R, C, Min) ->
	if
		R > Max ->
			Min;
		C > Max ->
			getShortestValuesList(Matrix, Max, R + 1, 1, Min);
		true ->
			Field = getField(Matrix, R, C),
			Length = length(Field),
			{PLength, _R, _C, _Val} = Min,
			if
				Length < PLength, Length > 1 ->
					getShortestValuesList(Matrix, Max, R, C + 1, {Length, R, C, Field});
				true -> 
					getShortestValuesList(Matrix, Max, R, C + 1, Min)
			end
	end.

changeElementInList([], N, Value) ->
	[];
changeElementInList([H|T], N, Value) ->
	if
		N =:= 1 ->
			[Value | changeElementInList(T, N - 1, Value)];
		true -> 
			[H | changeElementInList(T, N - 1, Value)]
	end.

changeFieldInMatrix([], _R, _C, _Value) ->
	[];
changeFieldInMatrix([Row | Rows], R, C, Value) ->
		Suffix = changeFieldInMatrix(Rows, R - 1, C, Value),
	if
		R =:= 1 ->
			[changeElementInList(Row, C, Value) | Suffix];
		true -> 
			[Row | Suffix]
	end.
	
	
bound(ValueMatrix, Matrix, R, C, [Value | Values]) ->
	BoundValueMatrix = changeFieldInMatrix(ValueMatrix, R, C, [Value]),
	
true.

filterValueFromList([H | T], Value) ->
	Filtered = lists:delete(H, Value),
	NewLength = length(filtered),
true.
	
% Mátrix sorából kigyűjti a kötött értékeket
getBoundValues([]) -> [];
getBoundValues([H|T]) ->
	Values1 = getBoundValues(T),
	Length = length(H),
	if
		Length =:= 1 ->
			[X] = H,
			[X | Values1];
		true ->
			Values1
	end.
		

filterRow(Row) ->
	BoundValues = getBoundValues(Row),
	filterRow(Row, BoundValues).
	
filterRow([], _) -> [];
filterRow([H|T], BoundValues) ->
	Filtered = filterRow(T, BoundValues),
	
			
	
