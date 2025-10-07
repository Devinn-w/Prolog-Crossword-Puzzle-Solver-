% Author: BINZHEN WEI (Student ID: 1575618)

% Purpose: This Prolog program solves a crossword puzzle by inserting words of 
% a given word list.
% It retrieves slots, filters by length, and employs a heuristic method of 
% matching words while keeping variable consistency.
%
% Language and Libraries Used:
%   - clpfd: for variable handling and logical constraints
%   - lists: for list operations like transpose/2 and maplist/3

% Import finite domain constraint logic support.
:- use_module(library(clpfd)).

% Import list utility predicates (e.g., transpose, maplist).
:- use_module(library(lists)).


% Solves the crossword puzzle by filling all horizontal and vertical slots
% (length ≥ 2) using words from WordList. Only one solution is returned.
%
% @param Puzzle    2D grid of variables representing the puzzle.
% @param WordList  List of words to fill into the puzzle.
puzzle_solution(Puzzle, WordList) :-
    once(solve(Puzzle, WordList)).

% solve(+Puzzle, +WordList) is det
%
% Solves the puzzle by filling all valid slots (length ≥ 2) 
% using words from the list with recursive unification.
%
% @param Puzzle    The crossword puzzle grid.
% @param WordList  Words available to be placed in the puzzle.
solve(Puzzle, WordList) :-
    get_slots(Puzzle, RowSlots),
    get_col_slots(Puzzle, ColSlots),
    append(RowSlots, ColSlots, AllSlots),
    include(is_longer_than_one, AllSlots, FilteredSlots),
    solve_slots(FilteredSlots, WordList).

% get_slots(+PuzzleRows:list(list), -AllSlots:list(list))
%
% Extracts all horizontal slots from a list of puzzle rows.
%
% @param PuzzleRows   List of rows in the puzzle grid.
% @param AllSlots     List of valid horizontal slots 
%                     (each a list of cells) extracted from all rows.
get_slots([], []).
get_slots([Row|Rest], AllSlots) :-
    get_row_slots(Row, RowSlots),
    get_slots(Rest, RestSlots),
    append(RowSlots, RestSlots, AllSlots).

% get_col_slots(+Puzzle:list(list), -ColSlots:list(list))
%
% Extracts all vertical slots from the puzzle grid 
% by transposing it and applying get_slots/2.
%
% @param Puzzle     2D puzzle grid as a list of rows.
% @param ColSlots   List of valid vertical slots.
get_col_slots(Puzzle, ColSlots) :-
    transpose(Puzzle, TPuzzle),
    get_slots(TPuzzle, ColSlots).

% get_row_slots(+Row:list, -Slots:list(list))
%
% Extracts all valid slots (length ≥ 2) from a single row.
%
% @param Row     A single row of the puzzle grid.
% @param Slots   List of valid slots found in the row.
get_row_slots(Row, Slots) :-
    extract_slots(Row, [], [], AllSlots),
    include(is_longer_than_one, AllSlots, Slots).

% extract_slots(+Row:list, +Curr:list, +Acc:list, -Slots:list) is det
%
% Extracts valid slots (non-'#' sequences of length >= 2) from a row.
%
% @param Row   The input row (list of characters/variables) to be scanned.
% @param Curr  The current sequence being accumulated.
% @param Acc   The accumulator collecting completed valid slots.
% @param Slots The final list of valid slots extracted from the row.
extract_slots([], [], Acc, Slots) :- reverse(Acc, Slots).
extract_slots([], Curr, Acc, Slots) :-
    ( length(Curr, L), L >= 2 -> reverse([Curr|Acc], Slots)
    ; reverse(Acc, Slots)
    ).
extract_slots([C|Rest], [], Acc, Slots) :-
    nonvar(C), C == '#',
    extract_slots(Rest, [], Acc, Slots).
extract_slots([C|Rest], Curr, Acc, Slots) :-
    nonvar(C), C == '#',
    ( length(Curr, L), L >= 2 -> extract_slots(Rest, [], [Curr|Acc], Slots)
    ; extract_slots(Rest, [], Acc, Slots)
    ).
extract_slots([C|Rest], Curr, Acc, Slots) :-
    append(Curr, [C], NewCurr),
    extract_slots(Rest, NewCurr, Acc, Slots).

% is_longer_than_one(+Slot:list) is semidet
%
% Succeeds if the slot is of length 2 or more.
%
% @param Slot  A potential slot to be checked for validity.
is_longer_than_one(Slot) :-
    length(Slot, L),
    L >= 2.

% solve_slots(+Slots:list(list), +Words:list(list)) is semidet
%
% Solve all slots recursively using a heuristic to choose the next best slot.
% Stops after finding the first valid solution (via once/1).
%
% @param Slots     A list of all valid slots in the puzzle.
% @param Words     A list of words still available to be placed in the puzzle.
solve_slots([], []).
solve_slots(Slots, Words) :-
    select_best_slot(Slots, Words, BestSlot),
    findall(W, (
        member(W, Words),
        same_length(W, BestSlot),
        matches(BestSlot, W)
    ), Candidates),
    once(member(Word, Candidates)),
    unify_slots(BestSlot, Word),
    select(BestSlot, Slots, RestSlots),
    select(Word, Words, RestWords),
    solve_slots(RestSlots, RestWords).

% select_best_slot(+Slots:list(list), +Words:list(list), -BestSlot:list) is det
%
% Selects the slot with the fewest matching candidate words.
% Used to improve efficiency by reducing branching in backtracking.
%
% @param Slots     List of available slots to choose from.
% @param Words     List of available words to match against slots.
% @param BestSlot  The slot with the minimal number of matching candidate words.
select_best_slot(Slots, Words, BestSlot) :-
    map_list_to_pairs(count_slot(Words), Slots, Pairs),
    keysort(Pairs, [_-BestSlot | _]).

% count_slot(+Words:list(list), +Slot:list, -Count:int) is det
%
% Count how many words from the word list are compatible with a given slot.
%
% @param Words  List of candidate words.
% @param Slot   A single slot (list of cells, possibly containing variables).
% @param Count  Number of words from Words that match this slot.
count_slot(Words, Slot, Count) :-
    findall(W, (
        member(W, Words),
        same_length(W, Slot),
        matches(Slot, W)
    ), Candidates),
    length(Candidates, Count).

% matches(+Slot:list, +Word:list) is semidet
%
% True if the Word can be placed into the Slot without violating fixed letters.
% Does not bind any variables in the Slot.
%
% @param Slot   A slot containing variables and/or fixed letters.
% @param Word   A candidate word to check for compatibility.
matches([], []).
matches([S|Ss], [W|Ws]) :-
    ( var(S) -> true ; S == W ),
    matches(Ss, Ws).

% unify_slots(+Slot:list, +Word:list) is det
%
% Unify each variable in the Slot with the corresponding letter in the Word.
% Used to place a word into the puzzle.
%
% @param Slot   A list of variables or fixed letters in the puzzle.
% @param Word   A word to bind into the Slot.
unify_slots([], []).
unify_slots([S|Ss], [W|Ws]) :-
    ( var(S) -> S = W ; S == W ),
    unify_slots(Ss, Ws).