%  Author     : Joanna Grace Cho Ern Lee
%  Student ID : 710094
%  Origin     : Friday, 13 Oct 2017
%  Purpose    : COMP30020 Declarative Progamming, Project 2
%
% The predicate puzzle_solution(Puzzle) aims to provide a valid solution for the
% inputted puzzle, given the rules in the specification. This is done using the
% clpfd to define constraints.

:- ensure_loaded(library(clpfd)).

% Gives a valid solution to the puzzle.
puzzle_solution(Puzzle) :-
  % separate out the input into other variables
  Puzzle = [_ | OtherRows],
  transpose(Puzzle, Columns),
  Columns = [_ | OtherCols],
  maplist(separate_list, OtherRows, RowHeaders, RowElems),
  maplist(separate_list, OtherCols, ColHeaders, ColElems),

  % ensures all elems are between 1 to 9 and are ground
  append(RowElems, Num), Num ins 1..9, label(Num),

  % ensure Elems have distinct values in each row and column
  maplist(all_distinct, RowElems),
  maplist(all_distinct, ColElems),

  % ensure header is either sum or product of elems
  maplist(is_sum_or_prod, RowElems, RowHeaders),
  maplist(is_sum_or_prod, ColElems, ColHeaders),

  % ensure all elem on the diagonal are the same value
  length(RowElems, Length),
  LastIndex is Length - 1,
  numlist(0, LastIndex, Indices),
  maplist(nth0, Indices, RowElems, DiagonalList),
  same_values(DiagonalList).

% Separates out List into variables Head and Tail
separate_list(List, Head, Tail) :-
  List = [Head | Tail].

% Checks if all the elements in the List have the same value
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

% Checks if the header is sum or product of all the elements.
is_sum_or_prod(Elem, Header) :-
  product(Elem, Header);
  sum(Elem, #=, Header).
