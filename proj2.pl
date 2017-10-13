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
