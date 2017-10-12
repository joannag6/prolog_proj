:- ensure_loaded(library(clpfd)).

% puzzle_solution(Puzzle).

puzzle_solution(Rows, RowElems, EqualsList, RowHeaders) :-
  Rows = [FirstRow | OtherRows], % OtherRows is a list of rows, incl headers
  transpose(Rows, Columns),
  Columns = [FirstCol | OtherCols], % OtherCols is a list of cols, incl headers
  maplist(separate_list, OtherRows, RowHeaders, RowElems), % RowElems are lists of row elem lists
  maplist(separate_list, OtherCols, ColHeaders, ColElems), % ColElems are lists of col elem lists

  % append(RowElems, Num), Num ins 1..9, % ensures all elems are between 1 to 9
  maplist(all_distinct, RowElems), % ensure RowElems have distinct values in each row
  maplist(all_distinct, ColElems). % ensure ColElems have distinct values in each col

  % (maplist(sum, RowElems, EqualsList, RowHeaders); % ensure the sum of each row add up to the header
  % maplist(product, RowElems, RowHeaders)),
  % (maplist(sum, ColElems, EqualsList, ColHeaders); % ensure the sum of each col add up to the header
  % maplist(product, ColElems, ColHeaders)).

testSum([[0,4,6],[3,1,_],[7,_,4]]).



% testPuzzle([[0,14,10,35],[14,_,_,_],[15,_,_,_],[28,_,1,_]]).
testPuzzle([[0,14,10,35],[14,7,2,1],[15,3,7,5],[28,4,1,7]]).

separate_list(Lst, Head, Tail) :-
  Lst = [Head | Tail].

% set_eq_list(Length, EqualsList), % makes a list of [#=] to be used in sum()
% set_eq_list(Length, EqualsList) :-
%   length(EqualsList, Length),
%   maplist(=(#=), EqualsList).

% Recursive function to find product of a list.
product(List, Product) :-
  List = [Head | Tail],
  product(Tail, NextProduct),
  Product is Head * NextProduct.
product([], Product) :-
  Product = 1.

% checks a row/col if they're sum or product.
is_sum_or_prod(Elem, Header) :- product(Elem, Header),!. % for first check, if true, will check the second one too
is_sum_or_prod(Elem, Header) :- sum(Elem, #=, Header).

% Rows = [FirstRow | ElemRows], % ElemRows is a list of rows
% FirstRow = [_ | ColHeaders], % ColHeaders is a list of column headers
% transpose(Rows, Columns),
% Columns = [FirstCol | ElemCols], % ElemCols is a list of columns
% FirstCol = [_ | RowHeaders], % RowHeaders is a list of row headers

  % Rows = [RowHeaders | RowElem],
  % maplist(same_length(RowElem), RowElem), % not sure what this does, makes sure it's square?
  % append(RowElem, Vs), Vs ins 1..9, % only digits 1 to 9
  % maplist(all_distinct, RowElem), % for non-repeated digits
  % transpose(Rows, Columns),
  % Columns = [ColumnHeaders | ColumnElem],
  % maplist(all_distinct, ColumnElem), % for non-repeated digits
  % maplist(sum(RowElem, #=, RowHeaders)),
  % sum(ColumnElem, #=, ColumnHeader).
  % Rows = [A,B,C,D,E,F,G,H,I],
  % blocks(A,B,C), blocks(D,E,F), blocks(G,H,I).

% Puzzle=[[0,14,10,35],[14,_,_,_],[15,_,_,_],[28,_,1,_]].


% should check for: (using SPCLP)
% diagonal all same number - only do once
% heading = sum or product of all digits in row
%   not part of row / column, but first of the list
%   need to extract
%   can use sumlist(TAIL, HEAD).
%   use transpose for columns
%   can ignore first elem of first list because always 0
% no need divide into blocks,
%  because the whole thing can have repeated numbers

% can assume that the inputs are valid

% strategy to make it faster
%   -- choose the columns/rows w least possibilities
%         use the bagof(_,_,_) predicate

% need to GROUND (term), means that the term only has one possible value.
%   can check with ground(X).


blocks([],[],[]).
blocks([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3]) :-
  all_distinct([A,B,C,D,E,F,G,H,I]),
  blocks(Bs1, Bs2, Bs3).

puzzle([[5,3,_, _,7,_, _,_,_],
       [6,_,_, 1,9,5, _,_,_],
       [_,9,8, _,_,_, _,6,_],

       [8,_,_, _,6,_, _,_,3],
       [4,_,_, 8,_,3, _,_,1],
       [7,_,_, _,2,_, _,_,6],

       [_,6,_, _,_,_, 2,8,_],
       [_,_,_, 4,1,9, _,_,5],
       [_,_,_, _,8,_, _,7,9]]).

    % [[5,3,4, 6,7,8, 9,1,2],
    %  [6,7,2, 1,9,5, 3,4,8],
    %  [1,9,8, 3,4,2, 5,6,7],
    %
    %  [8,5,9, 7,6,1, 4,2,3],
    %  [4,2,6, 8,5,3, 7,9,1],
    %  [7,1,3, 9,2,4, 8,5,6],
    %
    %  [9,6,1, 5,3,7, 2,8,4],
    %  [2,8,7, 4,1,9, 6,3,5],
    %  [3,4,5, 2,8,6, 1,7,9]]
