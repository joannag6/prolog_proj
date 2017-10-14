%  Author     : Joanna Grace Cho Ern Lee
%  Login ID   : joannal1
%  Student ID : 710094
%  Purpose    : COMP30020 Declarative Progamming (Semester 2, 2017) Project 2
%               The University of Melbourne
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The predicate puzzle_solution(Puzzle) aims to provide a valid solution for
%% an inputted puzzle. The puzzle is a square grid of squares, each with a
%% single digit, 1 to 9, and the headers for each row and column. A valid
%% solution holds the following constraints:
%%     - no repeated digits in a row or column.
%%     - all squares on the diagonal (top left to bottom right) contain the
%%       same value.
%%     - heading of each row and column contain either sum or product of its
%%       elements.
%% This was done using the clpfd library to define constraints.

:- ensure_loaded(library(clpfd)).

%% Provides a valid solution (as defined above) for Puzzle.
puzzle_solution(Puzzle) :-
  Puzzle = [_ | Rows],
  transpose(Puzzle, TransposedPuzzle),
  TransposedPuzzle = [_ | Cols],
  maplist(separate_list, Rows, RowHeaders, RowSquares),
  maplist(separate_list, Cols, ColHeaders, ColSquares),

  % ensures all values in squares are between 1 to 9
  append(RowSquares, Num), Num ins 1..9,

  % ensure no repeated values in each row and column
  maplist(all_distinct, RowSquares),
  maplist(all_distinct, ColSquares),

  % ensure header is either sum or product of elems
  maplist(is_sum_or_prod, RowSquares, RowHeaders),
  maplist(is_sum_or_prod, ColSquares, ColHeaders),

  % ensure all squares on the diagonal are the same value
  length(RowSquares, Length),
  LastIndex is Length - 1,
  numlist(0, LastIndex, Indices),
  maplist(nth0, Indices, RowSquares, DiagonalList),
  same_values(DiagonalList),

  % ensures all terms are ground
  append(RowSquares, Term),
  label(Term).

%% Predicate that separates out List into variables Head and Tail
separate_list(List, Head, Tail) :-
  List = [Head | Tail].

%% Predicate that defines a List to have all of its elements share the
%% same value
same_values(List) :-
  List = [First | Tail],
  Tail = [Second | _],
  First #= Second,
  same_values(Tail),!.
same_values([_]) :- true.

%% Predicate that assigns the product of all the elements in List to Product
product(List, Product) :-
  List = [Head | Tail],
  product(Tail, NextProduct),
  Product #= Head * NextProduct.
product([Head], Product) :-
  Product #= Head.

%% Predicate that defines Header to be the sum or the product of the elements
%% in the list List
is_sum_or_prod(List, Header) :-
  product(List, Header);
  sum(List, #=, Header).
