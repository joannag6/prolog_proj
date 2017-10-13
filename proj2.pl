:- ensure_loaded(library(clpfd)).

puzzle_solution(Rows) :-
  Rows = [_ | OtherRows], % OtherRows is a list of rows, incl headers
  transpose(Rows, Columns),
  Columns = [_ | OtherCols], % OtherCols is a list of cols, incl headers
  maplist(separate_list, OtherRows, RowHeaders, RowElems), % RowElems are lists of row elem lists
  maplist(separate_list, OtherCols, ColHeaders, ColElems), % ColElems are lists of col elem lists

  % ensures all elems are between 1 to 9
  append(RowElems, Num), Num ins 1..9,

  % Ensure Elems have distinct values in each row and column
  maplist(all_distinct, RowElems),
  maplist(all_distinct, ColElems),

  % ensure header is either sum or product of elems
  maplist(is_sum_or_prod, RowElems, RowHeaders),
  maplist(is_sum_or_prod, ColElems, ColHeaders),

  length(RowElems, Length),
  LastIndex is Length - 1,
  numlist(0, LastIndex, Indices),
  maplist(nth0, Indices, RowElems, DiagonalList),
  same_values(DiagonalList),

  append(RowElems, X),
  label(X).

testSum([[0,4,6],[3,1,_],[7,_,4]]).

% testPuzzle([[0,13,8],[6,_,_],[13,_,_]]).
% testPuzzle([[0,19,960,20,168],[384,_,_,_,2],[25,_,_,_,_],[60,_,_,_,_],[480,_,_,_,_]]).

% testPuzzle([[0,210,10,192],[15,_,_,_],[15,_,_,_],[126,_,_,_]]).
% testPuzzle([[0,90,56,12],[15,_,_,_],[18,_,_,_],[11,_,_,_]]).
% testPuzzle([[0,144,96,336],[144,_,_,_],[18,_,_,_],[18,_,_,_]]).
% testPuzzle([[0,336,11,36,280],[16,_,_,_,7],[180,_,_,_,_],[280,_,_,_,_],[24,_,_,_,_]]).
% testPuzzle([[0,18,19,22,2520],[216,_,_,_,_],[23,_,_,_,_],[160,1,_,_,_],[1296,_,_,_,_]]).

separate_list(Lst, Head, Tail) :-
  Lst = [Head | Tail].

same_values(List) :-
  List = [First | Tail],
  Tail = [Second | _],
  First #= Second,
  same_values(Tail),!.
same_values([_]) :- true.

% Recursive function to find product of a list.
product(List, Product) :-
  List = [Head | Tail],
  product(Tail, NextProduct),
  Product #= Head * NextProduct.
product([Head], Product) :-
  Product #= Head.

% checks a row/col if they're sum or product.
is_sum_or_prod(Elem, Header) :-
  product(Elem, Header); % added cut to counter backtracking, might run into errors.
  sum(Elem, #=, Header).

% Rows = [FirstRow | ElemRows], % ElemRows is a list of rows
% FirstRow = [_ | ColHeaders], % ColHeaders is a list of column headers
% transpose(Rows, Columns),
% Columns = [FirstCol | ElemCols], % ElemCols is a list of columns
% FirstCol = [_ | RowHeaders], % RowHeaders is a list of row headers

% set_eq_list(Length, EqualsList), % makes a list of [#=] to be used in sum()
% set_eq_list(Length, EqualsList) :-
%   length(EqualsList, Length),
%   maplist(=(#=), EqualsList).

% get_diagonal([[a,b,c],[c,a,b],[b,c,a]])

% get_diagonal(ListOfRows, Index) :-
%   ListOfRows = [Head | Tail].

% maplist(nth0, 0..Length, ListOfRows, DiagonalList),
% DiagonalList should be all the same numbers.
  % nth0(Index, List, Elem)

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
