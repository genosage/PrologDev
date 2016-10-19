% COMP30020 Declarative Programming - Project 2
% Name: Zeyu Ye (290159)
% This file is the main file for Project2-Puzzle Filling of COMP30020 
% Delcarative Programming, it will read and parse a PuzzleFile and 
% a WordlistFile, then it will generate an output SolutionFile (If all words,
% in the solution file can be all filled in the blank spot of the puzzle file).
% eg.
% PuzzleFile 
% #_#
% ___
% #_#
% WordlistFile
% HEL
% EEL
% SolutionFile
% #E#
% HEL
% #L#

:- ensure_loaded(library(clpfd)).

% main function that parses the puzzlefile, wordlistfile and produces output to
% solutionfile
main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

% function that reads a file and convert to a list of strings in Content
read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

% function that reads input stream line by line
read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).

% function that reads the first line of input stream
read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).

% function that prints the puzzle to the solution file
print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).
% functions that prints a line to a Stream output
print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

% function that prints a character to a Stream output
put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

% checks if the puzzle is a valid puzzle
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(same_length(Row), Rows).



% solve_puzzle takes a unfilled Puzzle in Matrix forms, WordList in List forms 
% and output a filled solution in Solved 
solve_puzzle(Puzzle, WordList, Solved):-
	transform_puzzle(Puzzle, Solved),
	transform_puzzle_to_list(Solved, List),
	build_possible_words(List, WordList, WordLists),
	sort_list_by_possible_list_size(List, WordLists, List1, WordLists1),
	solve_puzzle_list(List1, WordLists1).


% calls transform_array recursively to transform a puzzle matrix to 
% a matrix with Variables
transform_puzzle([], []).
transform_puzzle([X|Xs], [Y|Ys]):-
	transform_array(X, Y),
	transform_puzzle(Xs, Ys).


% transform an puzzle row into a row with Variables
transform_array([], []).
transform_array([X|Xs], [Y|Ys]):-
	(X = '_' -> Y = _; Y = X),
	transform_array(Xs, Ys).


% construct all possible variable list given a puzzle
transform_puzzle_to_list(Matrix, List):-
	transform_puzzle_to_list_row(Matrix, List1),
	transpose(Matrix, Matrix2),
	transform_puzzle_to_list_row(Matrix2, List2),
	append(List1, List2, List).


% consruct all possible variable list given by only considering puzzle rows
transform_puzzle_to_list_row([], []).
transform_puzzle_to_list_row([X|Xs], Matrix):-
	transform_puzzle_array(X, Y),
	transform_puzzle_to_list_row(Xs, Matrix1),
	append(Y, Matrix1, Matrix).


% construct all possible variable list given a puzzle row
transform_puzzle_array(Array, ArrayF):-
	split_puzzle_array(Array, [], [], ArrayF).


% construct a variable list given an array of puzzle row
split_puzzle_array([], Array, X, ArrayF):-
	((length(X, N), N > 1) ->
		append(Array, [X], ArrayF)
	; ArrayF = Array).

% construct a variable list given a matrix of puzzle
split_puzzle_array([Y|Ys], Array, X, ArrayF):-
	(\+ var(Y), Y = # ->
		((length(X, N), N > 1) -> 
			append(Array, [X], ArrayT),
			split_puzzle_array(Ys, ArrayT, [], ArrayF)
		;	split_puzzle_array(Ys, Array, [], ArrayF))
	;	append(X, [Y], X1),
		split_puzzle_array(Ys, Array, X1, ArrayF)).


% initial calculation that works out the possible word that could be 
% filled in each slots
build_possible_words([], _, []).
build_possible_words([L|Ls], WordList, [T|Ts]):-
	length(L, N),
	filter_eq_length(WordList, T, N),
	build_possible_words(Ls, WordList, Ts).


filter_eq_length([], [], _).
filter_eq_length([X|Xs], List, N):-
	((length(X, N1), N1 =:= N) ->
		List = [X|Ys],
		filter_eq_length(Xs, Ys, N)
	;	filter_eq_length(Xs, List, N)
	).


% sorts the list by size of possible word that could be filled in it.
sort_list_by_possible_list_size([], [], [], []).
sort_list_by_possible_list_size([L|Ls], [T|Ts], List, WordLists):-
	length(T, N),
	filter_list_by_le_length(Ls, Ts, Lse, Tse, N),
	filter_list_by_me_length(Ls, Ts, Lsm, Tsm, N),
	sort_list_by_possible_list_size(Lse, Tse, LseList, TseList),
	sort_list_by_possible_list_size(Lsm, Tsm, LsmList, TsmList),
	append(LseList, [L|LsmList], List),
	append(TseList, [T|TsmList], WordLists).


filter_list_by_le_length([], [], [], [], _).
filter_list_by_le_length([L|Ls], [T|Ts], List, WordLists, N):-
	((length(T, N1), N1 < N) -> 
		List = [L|Ys],
		WordLists = [T|Zs],
		filter_list_by_le_length(Ls, Ts, Ys, Zs, N)
	;	filter_list_by_le_length(Ls, Ts, List, WordLists, N)
	).

filter_list_by_me_length([], [], [], [], _).
filter_list_by_me_length([L|Ls], [T|Ts], List, WordLists, N):-
	((length(T, N1), N1 >= N) ->
		List = [L|Ys],
		WordLists = [T|Zs],
		filter_list_by_me_length(Ls, Ts, Ys, Zs, N)
	;	filter_list_by_me_length(Ls, Ts, List, WordLists, N)
	).


% solve_puzzle_list takes the puzzle in Variable List Form, and list of 
% remaining wordlist, and solves the Puzzle
solve_puzzle_list([], _).
solve_puzzle_list(List, WordLists):-
	rebuild_possible_words(List, WordLists, WordLists1),
	sort_list_by_possible_list_size(List, WordLists, [L|Ls], [T|Ts]),
	(length(T, N), N > 0 -> 
		fill_slot(L, T, W),
		filter_remain_list(Ts, W, Ys),
		solve_puzzle_list(Ls, Ys)
	).


% recalulates the current words that could be filled in for each slot
rebuild_possible_words([], [], []).
rebuild_possible_words([L|Ls], [T|Ts], [Z|Zs]):-
	filter_possible_list(T, L, Z),
	rebuild_possible_words(Ls, Ts, Zs).


% recalculates the current words that could be filled for the specified slot
filter_possible_list([], _, []).
filter_possible_list([Y|Ys], Slot, D):-
	((\+ \+ Slot = Y) ->
		D = [Y|Ds],
		filter_possible_list(Ys, Slot, Ds)
	;	filter_possible_list(Ys, Slot, D)
	).


% fill slot tries a slot with all possible word that could fill in it, and 
% output the current filled word for the slot
fill_slot(Slot, [Slot|_Slots], Slot).
fill_slot(Slot, [_|Slots], RH):-
	fill_slot(Slot, Slots, RH).


% filter_remain_list takes a word to be filtered out of wordlist of each slot
filter_remain_list([], _, []).
filter_remain_list([T|Ts], W, [Y|Ys]):-
	filter_first_instance(T, W, Y),
	filter_remain_list(Ts, W, Ys).


% filter_first_instance takes a word to be filtered out of all wordlist for a 
% slot, and filters out the first occurance of the word in this wordlist
filter_first_instance([], _, []).
filter_first_instance([X|Ws], W, Y):-
	(X == W ->
		Y = Ws
	; 	Y = [X|Ys],
		filter_first_instance(Ws, W, Ys)
	).