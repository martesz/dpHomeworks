-module(sudoku).
-author('kovacsmartin24@gmail.com').
-vsn('2016-11-03').
%-compile(export_all).
-export([sudoku/1]).

% Sorok listájaként tárolt M mátrix I-edik sora
matrixSoraSL(M, I) ->
        nth(I, M).

% Sorok listájaként tárolt M mátrix J-edik sora        
matrixOszlopaSL(M, J) ->
        [nth(J, X) || X <- M].

% sorok listájaként tárolt mátrix, oszlopok listájaként
% transzponálásnak is nevezhető
listaToOF(L) ->
    listaToOF(L, 1, length(nth(1,L))).
    
listaToOF(L, J, Length) ->
    case J of
    Oszlop when Oszlop =< Length ->
    [matrixOszlopaSL(L, Oszlop)| listaToOF(L, Oszlop + 1, Length)];
    _ -> []
    end.

% lista N-edik eleme
nth(_, []) -> error;
nth(1, [H|_]) -> H;
nth(N, [_|T]) -> nth(N-1, T).

szelet(L, I, J) ->
    szelet(L, I, J, 1).
    
% Lista szelete I-től J-edik sorszámú elemig 
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

% sorok listájaként tárolt mátrix sorfolytonos változata
listaToSF([H | []]) -> H;
listaToSF([H | T]) -> 
    lists:append(H, listaToSF(T)).
     
% M mátrix egy szelete, ami R db sorból áll,
% Minden sorban az I-től J-edik indexű elemek kerülnek kivágásra
% RIndex (int) iterátor, RAct a kezdősor indexe  
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
% CellSize méretű cella elemeit tartalmazó lista előállítása.
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
    
% Row(List of lists), Column(List of lists), Cell(List of lists)
% A bemenet listák, amik mezőinformációkat tartalmaznak
% A kimenet számok listája, a bemenetek uniója, azokból a számokat megtartva
getUsedValues(Row, Column, Cell) ->
    Temp = union2(Row, Column),
    Values = union2(Temp, Cell),
    lists:map(fun getIntegerFromField/1, Values).
    
% Mezőinformációk listájából a szám érték kivétele
getIntegerFromField([]) ->
    [];
getIntegerFromField([Head | Tail]) ->
    if
        erlang:is_integer(Head) -> Head;
        true -> getIntegerFromField(Tail)
    end.

% Matrix FieldR sorából a FieldC-edik mező
getField(Matrix, FieldR, FieldC) ->
    Row = nth(FieldR, Matrix),
    nth(FieldC, Row).

% Predikátum, ami azt mondja meg, hogy a megadott Value megfelel-e
% a megadott mezőinfók listájának
filterByInfo([], _Value) ->
    true;
filterByInfo([H | T], Value) ->
    if 
        H == e -> (Value rem 2 =:= 0) andalso filterByInfo(T, Value);
        H == o -> (Value rem 2 =/= 0) andalso filterByInfo(T, Value);
        erlang:is_integer(H) -> H =:= Value andalso filterByInfo(T, Value);
        true -> true andalso filterByInfo(T, Value)
    end.

% Azokat az értékeket tartja meg a Values-ból, amik megfelelnek
% a mezőinfókat tartalmazó Infos listának
filterValues(Values, Infos) ->
    [X || X <- Values, filterByInfo(Infos, X)].

% Matrix(listák listája, mezőinformációkat tartlmaz)
% Solution(listák listája, egy megoldás jelöltet tartalmaz)
% A mezőinfók alapján leszűri a Solution R-C mezőjében lévő értékeket
filterField(Matrix, Solution, R, C) ->
    Infos = getField(Matrix, R, C),
    [Value] = getField(Solution, R, C),
    filterByInfo(Infos, R, C, Solution, Value).

% A mező infok alapján megmondja, hogy a megoldásban szereplő érték megfelelő-e
filterByInfo([], _R, _C, _Solution, _Value) -> true;
filterByInfo([H | T], R, C, Solution, Value) ->
    if
        H == w ->
            [West] = getField(Solution, R, C - 1),
            ((Value + West) rem 2 =/= 0) andalso filterByInfo(T, R, C, Solution, Value);
        H == s ->
            [South] = getField(Solution, R + 1, C),
            ((Value + South) rem 2 =/= 0) andalso filterByInfo(T, R, C, Solution, Value);
        true -> true andalso filterByInfo(T, R, C, Solution, Value)
    end.

% Sudoku mátrix összes cellája
% Matrix(sorok listája)
% CellSize(Int)
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

% Segédfüggvény, egy új mátrixot állít elő, aminek minden mezőjébe a lehetséges
% értékek listája kerül
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

% Visszaadja a mátrix legkevesebb elemből álló mezőjét
% Matrix, Max(sor/oszlop szám)
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

% Listában megcseréli az N-edik elemet, a megadott Value-ra
changeElementInList([], _N, _Value) ->
    [];
changeElementInList([H|T], N, Value) ->
    if
        N =:= 1 ->
            [Value | changeElementInList(T, N - 1, Value)];
        true -> 
            [H | changeElementInList(T, N - 1, Value)]
    end.

% Mátrixban kicseréli a megadott mezőt, a megadott Value-ra
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

% Megvizsgálja, ha a mezőt lekötjük az adott értékre, az sérti-e a w szabályt
% Return: 2-es, {(false, ha nincs megoldás, true egyébként), A szomszédos mező új értéke}
checkW(ValueMatrix, _Matrix, R, C, Value) ->
    West = getField(ValueMatrix, R, C - 1),
            Parity = Value rem 2,
            if
                Parity =:= 0 ->
                    WestFiltered = filterValues(West, [o]),
                    WestLength = length(WestFiltered),
                    if
                        WestLength =:= 0 ->
                            % ha lekötjük az értéket nem lesz megoldás, jelezzük a hibát
                            {false, WestFiltered};
                        true ->
                            {true, WestFiltered}
                    end;
                true ->
                    WestFiltered = filterValues(West, [e]),
                    WestLength = length(WestFiltered),
                    if
                        WestLength =:= 0 ->
                            % ha lekötjük az értéket nem lesz megoldás, jelezzük a hibát
                            {false, WestFiltered};
                        true ->
                            {true, WestFiltered}
                    end
            end.

% Megvizsgálja, ha a mezőt lekötjük az adott értékre, az sérti-e az s szabályt
% Return: 2-es, {(false, ha nincs megoldás, true egyébként), A szomszédos mező új értéke}
checkS(ValueMatrix, _Matrix, R, C, Value) ->
    South = getField(ValueMatrix, R + 1, C),
            Parity = Value rem 2,
            if
                Parity =:= 0 ->
                    SouthFiltered = filterValues(South, [o]),
                    SouthLength = length(SouthFiltered),
                    if
                        SouthLength =:= 0 ->
                            % ha lekötjük az értéket nem lesz megoldás, jelezzük a hibát
                            {false, SouthFiltered};
                        true ->
                            {true, SouthFiltered}
                    end;
                true ->
                    SouthFiltered = filterValues(South, [e]),
                    SouthLength = length(SouthFiltered),
                    if
                        SouthLength =:= 0 ->
                            % ha lekötjük az értéket nem lesz megoldás, jelezzük a hibát
                            {false, SouthFiltered};
                        true ->
                            {true, SouthFiltered}
                    end
            end.

% Kicserél egy mezőt a mátrixban és frissíti a környező mezőket is a w-s-nek megfelelően,
% ha ez nem sérti a w vagy s szabályt
% return 2-es: {(notOk ha nincs megoldás, egyébként ok), Mátrix amiben ki vannak cserélve az értékek}
changeFieldInMatrixWS(ValueMatrix, Matrix, R, C, [Value]) ->
    RulesField = getField(Matrix, R, C),
    IsW = lists:member(w, RulesField),
    IsS = lists:member(s, RulesField),
    if
        IsW =:= true ->
           {W ,WestFiltered} = checkW(ValueMatrix, Matrix, R, C, Value),
            if
                W =:= true ->
                    if IsS =:= true ->
                        {S, SouthFiltered} = checkS(ValueMatrix, Matrix, R, C, Value),
                        if
                            S =:= true ->
                                SouthChanged = changeFieldInMatrix(ValueMatrix, R + 1, C, SouthFiltered),
                                WestChanged = changeFieldInMatrix(SouthChanged, R, C, WestFiltered),
                                {ok, changeFieldInMatrix(WestChanged, R, C, [Value])};
                            true ->
                                {notOk, []}
                        end;
                    true ->
                        WestChanged = changeFieldInMatrix(ValueMatrix, R, C, WestFiltered),
                        {ok, changeFieldInMatrix(WestChanged, R, C, [Value])}
                    end;
                true ->
                    {notOk, []}
            end;
          true ->
             if
                 IsS =:= true ->
                        {S, SouthFiltered} = checkS(ValueMatrix, Matrix, R, C, Value),
                        if
                            S =:= true ->
                                SouthChanged = changeFieldInMatrix(ValueMatrix, R + 1, C, SouthFiltered),
                                {ok, changeFieldInMatrix(SouthChanged, R, C, [Value])};
                            true ->
                                {notOk, []}
                        end;
                true ->                    
                    {ok, changeFieldInMatrix(ValueMatrix, R, C, [Value])}
            end
    end.
                  
    
% Sudokuban egy mezőben lévő lehetséges értékek közül kipróbálja mindegyiket, és megnézi
% hogy azzal az értékkel megoldható-e
% Ha megoldható hozzáfűzi a megoldásokhoz
bound(_, _, _, _, _, []) -> 
    [];
bound(ValueMatrix, Matrix, R, C, CellSize, [Value | Values]) ->
    Results1 = bound(ValueMatrix, Matrix, R, C, CellSize, Values),
   % BoundValueMatrix = changeFieldInMatrix(ValueMatrix, R, C, [Value]),
    {Ok,BoundValueMatrix} = changeFieldInMatrixWS(ValueMatrix, Matrix, R, C, [Value]),
    if
        Ok =:= ok ->
            {FilteredMatrix, Return, NotBound} = filterWhileChanges(BoundValueMatrix, Matrix, CellSize),
            if
                % Nem megoldható ezzel a lekötéssel
                Return < 0 ->
                    Results1;
            true ->
                Results2 = solve(FilteredMatrix, Matrix, CellSize, NotBound),
                Results1 ++ Results2
            end;
        true ->
                Results1
    end.

% Return: sudoku feladvány összes megoldása
% ValueMatrix(a lehetséges értékeket tartlmazó mátrix)
% Matrix(a feladvány)
% CellSize(int)
% NotBound(azon mezők száma, ahol még több lehetséges érték szerepel)
solve(ValueMatrix, Matrix, CellSize, NotBound) ->
    if
        % kesz vagyunk
        NotBound =:= 0 ->
           SolutionOk = checkAllFields(Matrix, ValueMatrix, CellSize),
           if
               SolutionOk =:= true ->
                   [ValueMatrix];
               true ->
                   []
           end;
        true ->
            {Length, R, C, Field} = getShortestValuesList(ValueMatrix, CellSize * CellSize),
            if
                % Ha a függvény meghívásakor már meg van oldva
                Length =:= 999999 ->
                    solve(ValueMatrix, Matrix, CellSize, 0);
                true ->
                    bound(ValueMatrix, Matrix, R, C, CellSize, Field)
            end
    end.          

%% @type sspec() = {size(), board()}.
%% @type size()  = integer().
%% @type field() = [info()].
%% @type info()  = e | o | s | w | integer().
%% @type board() = [[field()]].
%% @type ssol() = [[integer()]].
%% @spec sudoku:sudoku(SSpec::sspec()) -> SSols::[ssol()].
%% @doc  SSols az SSpec feladványt kielégítő megoldások listája.
sudoku({CellSize,Matrix}) ->
    ValueMatrix = getAllPossibleValues(Matrix, CellSize),
	{ResultMatrix, Return, _NotBound} = filterWhileChanges(ValueMatrix, Matrix, CellSize), 
    if
		% szűrés elhasalt
		Return < 0 ->
			[];
		true ->
			Solutions = solve(ResultMatrix, Matrix, CellSize, 9999),
    	[formatOutput(X) || X <- Solutions]
	end.

% Egy listából kiszíri a megadott értékeket, 
% visszatérési értékben jelzi ha
% - üres lett a lista -> fail
% - 1 elemű lett -> bound
% változott, nem változott -> bound, not bound
filterValuesFromList(List, Values) ->
    Length = length(List),
    Filtered = lists:subtract(List, Values),
    NewLength = length(Filtered),
    if
        Length =:= NewLength ->
            if 
               NewLength =:= 1 ->
                   {notChanged, bound, Filtered};
               true ->
                   {notChanged, notBound, Filtered}
            end;
        NewLength =:= 0 ->
            {failed, notBound, Filtered};
        true -> 
            if 
               NewLength =:= 1 ->
                   {changed, bound, Filtered};
               true ->
                   {changed, notBound, Filtered}
            end
    end.

% Mátrix minden sorát megszűri a w szabály szerint
% bemenetek: Solution(lehetséges értékek mátrixa), InfoMatrix(feladvány)
% Return: 2-es, {(a megszűrt mátrix), Int ami jelzi, ha nem megoldható a sudoku, ilyenkor < 0}
filterWAllRows([], []) ->
	{[], 0};
filterWAllRows([Sol | Sols], [Info | Infos]) ->
	{FilteredSol, _, _, Return} = filterW(Sol, Info),
	{FilteredTail, Return1} = filterWAllRows(Sols, Infos),
	{[FilteredSol | FilteredTail], Return + Return1}.

% Mátrix egy sorát megszűri a w szabály szerint
filterW(SolutionRow, MatrixRow) ->
	filterW(SolutionRow, MatrixRow, [2,3]).

% Sor szűrése w szabály szerint, 
% Return 4-es {(Megszűrt sor, megszűrt előző mező, előző mezőben volt-e w, (int) hibajelzésre)}
filterW([],[], _) ->
	{[], [], false, 0};
filterW([Sol|Sols], [Row| Rows], Prev) ->
	{FilteredTail1, FilteredPrev1, PrevIsW1, Return} = filterW(Sols, Rows, Sol),
    ActIsW = lists:member(w, Row),
	if
		ActIsW =:= true ->
			AllEven = allEven(Sol),
			AllOdd = allOdd(Sol),
			if 
				AllEven =:= true ->
				   FilteredPrev = filterValues(Prev, [o]),
				   Length = length(FilteredPrev),
				   if
					   Length =:= 0 ->
						   {[],[], false, -9999};
					   PrevIsW1 =:= true ->
						   {[FilteredPrev1 | FilteredTail1], FilteredPrev, true, Return};
					   true ->
						   {[Sol | FilteredTail1], FilteredPrev, true, Return}
				   end;
			    AllOdd =:= true ->
					FilteredPrev = filterValues(Prev, [e]),
					Length = length(FilteredPrev),
				     if
					   Length =:= 0 ->
						   {[],[], false, -9999};
					   PrevIsW1 =:= true ->
						   {[FilteredPrev1 | FilteredTail1], FilteredPrev, true, Return};
					   true ->
						   {[Sol | FilteredTail1], FilteredPrev, true, Return}
				   end;
				true ->
					{[Sol | FilteredTail1], Prev, true, Return}
			end;
		true ->
			if 
				PrevIsW1 =:= true ->
					   {[FilteredPrev1 | FilteredTail1], Prev, false, Return};
			 	true ->
					   {[Sol | FilteredTail1], Prev, false, Return}
				end
	end.

% Mátrix minden oszlopát megszűri az s szabály szerint
% bemenetek: Solution(lehetséges értékek mátrixa), InfoMatrix(feladvány)
% Return: 2-es, {(a megszűrt mátrix), Int ami jelzi, ha nem megoldható a sudoku, ilyenkor < 0}
filterSAllColumns([], []) ->
	{[], 0};
filterSAllColumns([Sol | Sols], [Info | Infos]) ->
	{FilteredSol, Return} = filterS(Sol, Info,[], []),
	{FilteredTail, Return1} = filterSAllColumns(Sols, Infos),
	{[FilteredSol | FilteredTail], Return + Return1}.

% Input: Solution(lehetséges értékek mátrixa)
% Matrix(feladvány), Prev(előző mező), PrevIsS(előző mezőben volt s)
% Return: 2-es {megszűrt oszlop, hibajelzés (int)}
filterS([],[],_, _) ->
	{[], 0};
filterS([Sol|Sols], [Row| Rows], Prev, PrevIsS1) ->
    ActIsS = lists:member(s, Row),
	{FilteredTail1,  Return} = filterS(Sols, Rows, Sol, ActIsS),
	if
		PrevIsS1 =:= true ->
			AllEven = allEven(Prev),
			AllOdd = allOdd(Prev),
			if
				AllEven =:= true ->
					Filtered = filterValues(Sol, [o]),
					Length = length(Filtered),
					if
						Length =:= 0 ->
							{[Filtered| FilteredTail1], -9999};
						true ->
							{[Filtered | FilteredTail1], Return}
					end;
				AllOdd =:= true ->
					Filtered = filterValues(Sol, [e]),
					Length = length(Filtered),
					if
						Length =:= 0 ->
							{[Filtered| FilteredTail1], -9999};
						true ->
							{[Filtered | FilteredTail1], Return}
					end;
				true ->
					{[Sol | FilteredTail1], Return}
			end;
		true ->
			{[Sol | FilteredTail1], Return}
	end.
			
% Lista minden eleme páros-e
allEven([]) ->
	true;
allEven([H|T]) ->
	H rem 2 =:= 0 andalso
		allEven(T).

% Lista minden eleme páratlan-e
allOdd([]) ->
	true;
allOdd([H|T]) ->
	H rem 2 =/= 0 andalso
		allOdd(T).
					   
	
% Mindegyik listaelemre meghívja a szűrést
filterRows([]) -> {0, [], 0};
filterRows([H|T]) ->
    {Return1, FilteredRows, NotBound1} = filterRows(T),
    {Return, FilteredRow, NotBound} = filterRow(H),
    {Return + Return1, [FilteredRow | FilteredRows], NotBound + NotBound1}.

filterRow(Row) ->
    {Return, FilteredRow, _BoundValues, NotBound} = filterRowHelper(Row, []),    
    {Return, FilteredRow, NotBound}.            
            
% Kiszűrjük a sorból a kötött elemeket, ha újabb kötött elemek jelennek meg,
% akkor ezeket is kiszűrjük az eddig bejárt részből
% Bemenet: Sor, kezdeti kötött értékek listája
% Kimenet: 4-es {Int(történt-e a változás, >0 történt, <0 nincs megoldás, =:=0 nem történt változás)
% List(a szűrt sor), List(kötött értékek), Int( nem lekötött mezők száma)}
filterRowHelper([], InitBound) -> {0, [], InitBound, 0};
filterRowHelper([H|T], InitBound) ->
    {Return1, FilteredTail, BoundValues1, NotBound1} = filterRowHelper(T, InitBound),
    {IsChanged, IsBound, FilteredField} = filterValuesFromList(H, BoundValues1),
    if 
        IsBound =:= bound ->
            {Return, FilteredTail1, _BoundValues, NotBound2} = filterRowHelper(FilteredTail, FilteredField),
            if
                Return < 0 ->
                    {Return, [], [], 0};
                true ->
                    BoundValues = ordsets:union(FilteredField, BoundValues1),
                    if
                        IsChanged =:= changed ->
                            {Return1 + Return + 1, [FilteredField | FilteredTail1], BoundValues, NotBound2};
                        IsChanged =:= failed ->
                            {-9999, [], [], 0};
                        true ->
                            {Return1 + Return, [FilteredField | FilteredTail1], BoundValues, NotBound2}
                    end
            end;
        true ->
           if
                IsChanged =:= changed ->
                    {Return1 + 1, [FilteredField | FilteredTail], BoundValues1, NotBound1 + 1};
                IsChanged =:= failed ->
                    {-9999, [], [], 0};
                true ->
                    {Return1, [FilteredField | FilteredTail], BoundValues1, NotBound1 + 1}
            end
    end.

% Mátrix sorain, oszlopain és celláin is elvégezzük a szűrést
filterMatrix(Matrix, InfoMatrix, CellSize) ->
    % Sorok szűrése
	{WFiltered, ReturnW} = filterWAllRows(Matrix, InfoMatrix),
   
	if
		 ReturnW < 0  ->
			 {[], ReturnW, 0};
		true ->
			 {Return, FilteredRows, _NotBound} = filterRows(WFiltered),
    	if 
        % Ha bármelyik szűrés elhasal, visszatérünk, nincs megoldás
        Return < 0 ->
            {[], Return, 0};
        true ->
            %oszlopok szűrése
            Columns = listaToOF(FilteredRows),
			InfoColumns = listaToOF(InfoMatrix),
			{FilteredColumns, ReturnS} = filterSAllColumns(Columns, InfoColumns),
			if
				ReturnS < 0 ->
					{[], ReturnS, 0};
				true ->
            {Return1, FilteredRows1, _NotBound1} = filterRows(FilteredColumns),
            if
                Return1 < 0 ->
                    {[], Return1, 0};
                true ->
                    % cellák szűrése
                    Rows = listaToOF(FilteredRows1),
                    Cells = getCells(Rows, CellSize),
                    {Return2, FilteredRows2, NotBound2} = filterRows(Cells),
                    if 
                        Return2 < 0 ->
                          {[], Return2, 0};
                        true ->
                            Result = cellsToRowConsecutive(FilteredRows2, CellSize),
                            {Result, Return2, NotBound2}
                    end
            end
			end
    	end
	end.

% Addig szűrjük a mátrixot, amíg változik
filterWhileChanges(Matrix, InfoMatrix, CellSize) ->
    {ResultMatrix, Return, NotBound} = filterMatrix(Matrix, InfoMatrix, CellSize),
    if
        Return < 0 ->
            {[],Return,0};
        true ->
            if
                % változott, újra szűrünk
                Return > 0 ->
                    filterWhileChanges(ResultMatrix, InfoMatrix, CellSize);
                true ->
                    {ResultMatrix, Return, NotBound}
            end
    end.
          

% Cellákból mátrix visszaállítása
cellsToRowConsecutive(Cells, CellSize) ->
    cellsToRowConsecutive(Cells, CellSize, 1).

cellsToRowConsecutive(Cells, CellSize, Iterator) ->
    if 
        Iterator > (CellSize * CellSize) ->
            [];
        true ->
            Tail = cellsToRowConsecutive(Cells, CellSize, Iterator + CellSize),
            SubList = lists:sublist(Cells, Iterator, CellSize),
            Rows = cellsToRows(SubList, CellSize, 1),
            Rows ++ Tail
    end.
        
cellsToRows(Cells, CellSize, Iterator) ->
    if
        Iterator > CellSize * CellSize ->
            [];
        true ->
            Tail = cellsToRows(Cells, CellSize, Iterator + CellSize),
            SubLists = sublists(Cells, Iterator, CellSize),
            [SubLists | Tail]
    end.
    
% Lista minden eleméből veszi kivesz egy részt és ezeket összefűzi egy újabb listává
% Input: List of lists, Int, Int
sublists([], _, _) ->
    [];
sublists([H|T], From, Count) ->
    Tail = sublists(T, From, Count),
    Sublist = lists:sublist(H, From, Count),
    Sublist ++ Tail.

% listák listáját összefűzi egy listává
concatLists([])->
    [];
concatLists([H|T]) ->
    H ++ concatLists(T).  

% Bemenet sudoku megoldás, sorok listájaként, amiben minden elem egy lista
% Return: sudoku megoldás, sorok listájaként, amiben minden elem egy Int
formatOutput([]) ->
    [];
formatOutput([H|T]) ->
    Tail = formatOutput(T),
    [concatLists(H)| Tail].

