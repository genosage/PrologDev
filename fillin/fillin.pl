% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% Title:   COMP90048 Declarative Programming - Project 2                        %
%                                                                               %
% Author:  Tian Zhang (735882)                                                  %
%                                                                               %
% Purpose: This file is to solve the fillin crossword puzzles. It can           %
%          read a puzzle file whose name is PuzzleFile and a word list          %
%          file whose name is WordListFile, generate a solution of the          %
%          puzzle, and print out the result to the file SolutionFile.           %
%                                                                               %
%          The puzzle file is a rectangle which contains some lines each        %
%          with the same numbe of characters. The characters in this file       %
%          should be either an underline ('_'), a hash ('#'), or a letter       %
%          or digit. The SolutionFile has the same format, and the underlines   %
%          are filled. The WordListFile is a text file with one word per line.  %
%                                                                               %
% Eg.:     PuzzleFile                                                           %
%          ____                                                                 %
%          a__#                                                                 %
%          ____                                                                 %
%                                                                               %
%          WordListFile                                                         %
%          boat                                                                 %
%          art                                                                  %
%          need                                                                 %
%          ban                                                                  %
%          ore                                                                  %
%          ate                                                                  %
%                                                                               %
%          SolutionFile                                                         %
%          boat                                                                 %
%          art#                                                                 %
%          need                                                                 %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Main Predicate                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% This line ensures that the correct transpose predicate is loaded.
:- ensure_loaded(library(clpfd)).


% This predicate reads the PuzzleFile and WordListFile, solve the puzzle,
% and print the solution to the SolutionFile.
main(PuzzleFile, WordListFile, SolutionFile) :-
    read_file(PuzzleFile, Puzzle),
    read_file(WordListFile, WordList),
    valid_puzzle(Puzzle),
    solve_puzzle(Puzzle, WordList, Solved),
    print_puzzle(SolutionFile, Solved).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                I/O Predicates                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% This predicate reads a file, and convert the content to a list.
read_file(Filename, Content) :-
    open(Filename, read, Stream),
    read_lines(Stream, Content),
    close(Stream).


% This predicate reads a stream line by line, and convert the content
% to a list.
read_lines(Stream, Content) :-
    read_line(Stream, Line, Last),
    (   Last = true
    ->  (   Line = []
        ->  Content = []
        ;   Content = [Line]
        )
    ;   Content = [Line|Content1],
        read_lines(Stream, Content1)
    ).


% This predicate reads a line from the stream, and convert the content
% to a list.
read_line(Stream, Line, Last) :-
    get_char(Stream, Char),
    (   Char = end_of_file
    ->  Line = [],
        Last = true
    ;   Char = '\n'
    ->  Line = [],
        Last = false
    ;   Line = [Char|Line1],
        read_line(Stream, Line1, Last)
    ).


% This predicate prints out the solution to the SolutionFile.
print_puzzle(SolutionFile, Puzzle) :-
    open(SolutionFile, write, Stream),
    maplist(print_row(Stream), Puzzle),
    close(Stream).

% This predicate prints a line to the stream.
print_row(Stream, Row) :-
    maplist(put_puzzle_char(Stream), Row),
    nl(Stream).


% This predicate prints a character to the stream.
put_puzzle_char(Stream, Char) :-
    (   var(Char)
    ->  put_char(Stream, '_')
    ;   put_char(Stream, Char)
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                          Puzzle Slover Predicate                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% This predicate takes an unfilled puzzle, replaces the underlines to unbound
% variables, and convert the replaced puzzle to a list of slots. Each time, a
% best slot is selected from the slots list, and the best slot will be filled
% with the word from the word list, this process goes on until both the slots 
% list and the word list are empty.
solve_puzzle(Puzzle, WordList, Solved) :-
    puzzle_to_puzzlelist(Puzzle, Solved),
    puzzlelist_to_slotslist(Solved, SlotsList),
    fillin(SlotsList, WordList).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                          Conversion Predicates                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% This predicate takes a puzzle, replaces the underlines to unbound variables,
% and save it in a list.
puzzle_to_puzzlelist([], []).
puzzle_to_puzzlelist([Row|Rows], [PuzzleLine|PuzzleList]) :-
    row_to_puzzlelist(Row, PuzzleLine),
    puzzle_to_puzzlelist(Rows, PuzzleList).


% This predicate takes a line of the puzzle, replaces the underlines to unbound
% variables, and save it in a list.
row_to_puzzlelist([], []).
row_to_puzzlelist([Char|Chars], [Var|Vars]) :-
    char_to_var(Char, Var),
    row_to_puzzlelist(Chars, Vars).


% This predicate can convert an underline character ('_') to an unbound variable,
% the hash character ('#') and letters or digits are unchanged.
char_to_var(Char, Var) :-
    (   Char == '_'
    ->  Var = _
    ;   Var = Char
    ).


% This predicate can construct a list of slots from a puzzle which has been
% processed.
puzzlelist_to_slotslist(Puzzle, SlotsList) :-
    rows_to_slots(Puzzle, SlotsList1),
    transpose(Puzzle, Puzzle2),
    rows_to_slots(Puzzle2, SlotsList2),
    append(SlotsList1, SlotsList2, SlotsList).


% This predicate can construct a list of slots from all rows of the puzzle.
rows_to_slots([], []).
rows_to_slots([Row|Rows], Slots) :-
    row_to_slots(Row, [], Slots1),
    rows_to_slots(Rows, Slots2),
    append(Slots1, Slots2, Slots).


% This predicate can construct a list of slots from a single row of the puzzle.
% Only the slot whose length is more than 2 will be considered.
row_to_slots([], CurrentSlot, Slots) :-
    length(CurrentSlot, N),
    (   N > 1
    ->  Slots = [CurrentSlot]
    ;   Slots = []
    ).
row_to_slots([Var|Vars], CurrentSlot, Slots) :-
    (   Var \== '#'
    ->  append(CurrentSlot, [Var], CurrentSlot1),
        row_to_slots(Vars, CurrentSlot1, Slots)
    ;   length(CurrentSlot, N),
        (   N > 1
        ->  Slots = [CurrentSlot|Slots1]
        ;   Slots = Slots1
        ),
        row_to_slots(Vars, [], Slots1)
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            Optimization Predicates                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% This predicate takes a SlotsList and a WordList, finds the best slot and fills
% it. The filled slot then is removed from SlotsList and the word is removed from
% the WordList. This process continues until both the SlotsList and the WordList
% are empty.
fillin([], []).
fillin(SlotsList, WordList) :-
    find_best(SlotsList, WordList, Best),
    member(Best, WordList),
    delete_elem(SlotsList, Best, SlotsList1),
    delete_elem(WordList, Best, WordList1),
    fillin(SlotsList1, WordList1).


% This predicate takes a list of slots and a word list, and can find the best slot
% which has the lowest matching number.
find_best([Slot|Slots], WordList, Best) :-
    slot_matching_number(Slot, WordList, 0, Result),
    best_slot(Slots, WordList, Result, Slot, FinalBest),
    Best = FinalBest.


% This predicate takes a slot and a word list, and can return the matching number 
% of the slot.
slot_matching_number(_, [], CurrentNum, Result) :-
    Result = CurrentNum.
slot_matching_number(Slot, [Word|Words], CurrentNum, Result) :-
    (   Slot \= Word
    ->  CurrentNum1 = CurrentNum
    ;   CurrentNum1 = CurrentNum + 1
    ),
    slot_matching_number(Slot, Words, CurrentNum1, Result).


% This predicate can find the best slot which has the lowest matching number
% from a list of slots.
best_slot([], _, _, CurrentBest, FinalBest) :-
    FinalBest = CurrentBest.
best_slot([Slot|Slots], WordList, LowestNum, CurrentBest, FinalBest) :-
    slot_matching_number(Slot, WordList, 0, Result),
    (   Result < LowestNum
    ->  LowestNum1 = Result,
        CurrentBest1 = Slot
    ;   LowestNum1 = LowestNum,
        CurrentBest1 = CurrentBest
    ),
    best_slot(Slots, WordList, LowestNum1, CurrentBest1, FinalBest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              Helper Predicates                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% This predicate can ensure that the rows from a puzzle have the same length.
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
    maplist(same_length(Row), Rows).


% This predicate can delete an element from a list.
delete_elem([], _, ResultList) :-
    ResultList = [].
delete_elem([X|Xs], Elem, ResultList) :-
    (   X == Elem
    ->  ResultList = Xs
    ;   delete_elem(Xs, Elem, List),
        ResultList = [X|List]
    ).