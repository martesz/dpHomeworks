-module(khf1).
-author('kovacsmartin24@gmail.com').
-vsn('2016-09-28').
-export([feldarabolasa/2]).
%-compile(export_all).

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

% Sorok listájaként tárolt mátrix i. sorának előallítása.
matrixSoraSL(M, I) ->
        nth(I, M).
        
% Oszlopok listájaként tárolt mátrix i. sorának előallítása.
matrixSoraOL(M, I) ->
        [nth(I, X) || X <- M].

% Sorok listájaként tárolt mátrix j. oszlopának előállítása.        
matrixOszlopaSL(M, J) ->
        [nth(J, X) || X <- M].
        
% Oszlopok listájaként tárolt mátrix j. oszlopának előállítása.
matrixOszlopaOL(M, J) ->
        nth(J, M).
        
% Sorfolytonosan tárolt mátrix i. oszlopának előállítása
matrixOszlopaSF(M, I, W, H) ->
        matrixOszlopaSF(M, I, W, H, I).
        
% segéd fv.     
matrixOszlopaSF(M, I, W, H, Index) ->
    X = nth(Index, M),
    case X of
    error -> [];
    _ -> [X | matrixOszlopaSF(M, I, W, H, Index + W)]
    end.
    
% Oszlopfolytonosan tárolt mátrix i. oszlopának előállítása
matrixOszlopaOF(M, I, W, H) ->
        matrixOszlopaOF(M, I, W, H, 1 + (I-1) * H).
        
% segéd fv.     
matrixOszlopaOF(M, I, W, H, Index) ->
    X = nth(Index, M),
    case Index of
    Y when Y =< I * H -> [X | matrixOszlopaOF(M, I, W, H, Index + 1)];
    _ -> []
    end.
    
% Sorfolytonosan tárolt mátrix i. sorának előállítása
matrixSoraSF(M, I, W, H) ->
        matrixSoraSF(M, I, W, H, 1 + (I-1) * W).
        
% segéd fv.     
matrixSoraSF(M, I, W, H, Index) ->
    X = nth(Index, M),
    case Index of
    Y when Y =< I * W -> [X | matrixSoraSF(M, I, W, H, Index + 1)];
    _ -> []
    end.

% Oszlopfolytonosan tárolt mátrix i. sorának előállítása
matrixSoraOF(M, I, W, H) ->
        matrixSoraOF(M, I, W, H, I).
        
% segéd fv.     
matrixSoraOF(M, I, W, H, Index) ->
    X = nth(Index, M),
    case X of
    error -> [];
    _ -> [X | matrixSoraOF(M, I, W, H, Index + H)]
    end.

% sorok listájaként tárolt mátrix sorfolytonos változata
listaToSF([H | []]) -> H;
listaToSF([H | T]) -> 
    lists:append(H, listaToSF(T)).

% sorok listájaként tárolt mátrix oszlopfolytonos változatának előállítása
listaToOF(L) ->     % oszlopszám
    listaToOF(L, 1, length(nth(1,L))).
    
listaToOF(L, J, Length) ->
    case J of
    Oszlop when Oszlop =< Length ->
    lists:append(matrixOszlopaSL(L, Oszlop), listaToOF(L, Oszlop + 1, Length));
    _ -> []
    end.
    
% Sorfolytonosan tárolt mátrix sorok listájaként tárolt változatának előállítása.
sFToList(M, W) -> 
    sFToList(M, W, 1, length(M)).
        
sFToList(M, W, I, L) ->
    case I of
        X when X < L -> [szelet(M, I, I + (W - 1)) | sFToList(M, W, I + W, L)];
        _ -> []
    end.

oFToList(M, H, W) ->
    sFToList(oFToSF(M, H, W, 1, 1, length(M)), W).

% oszlopfolytonos mátrix sorfolytonossá
% LI = line index, CI = column index, L = length of M, H = height -> line number
oFToSF(M, H, W, LI, CI, L) ->
    Index = LI + ((CI - 1) * H),
    case LI of
        Y when Y =< H ->
            case CI of
                X when X < W -> [nth(Index, M) | oFToSF(M, H, W, LI, CI + 1, L)];
                X when X =:= W -> [nth(Index, M) | oFToSF(M, H, W, LI + 1, 1, L)]
            end;
        _ -> []
    end.

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