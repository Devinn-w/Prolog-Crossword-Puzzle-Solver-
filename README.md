#  Prolog Crossword Puzzle Solver

This project is a **crossword puzzle solver** implemented in **Prolog**, designed to fill a puzzle grid with words from a given word list.  
It uses **slot extraction**, **length filtering**, and a **heuristic-based search strategy** to efficiently find a valid solution while preserving variable consistency.

---

##  Features

-  Extracts **horizontal and vertical slots** (length ≥ 2) from a puzzle grid  
-  Filters valid slots automatically  
-  Uses a **heuristic** (fewest candidate words first) to reduce backtracking  
-  Ensures **letter consistency at intersections** between words  
-  Returns **only one valid solution** using `once/1` to prevent multiple outputs

---

##  How It Works

1. **Slot Extraction**  
   - The program scans each row and column of the puzzle grid.  
   - Any sequence of non-`#` cells of length ≥ 2 is considered a valid slot.

2. **Filtering & Preparation**  
   - All horizontal and vertical slots are combined and filtered for length.

3. **Heuristic Word Placement**  
   - For each step, the solver chooses the slot with **the fewest matching words**.  
   - This reduces branching and improves search efficiency.

4. **Recursive Solving**  
   - A candidate word is selected, unified with the slot, removed from the word list, and the solver proceeds with the remaining slots.

---

##  Example Usage

### Puzzle Grid (Prolog Format)
```prolog
Puzzle = [
    [#, h, #],
    [_, _, _],
    [#, _, #]
].
WordList = [[h, a, t], [b, a, g]].

Query
?- puzzle_solution(Puzzle, WordList).

Output
Puzzle = [
    [#, h, #],
    [b, a, g],
    [#, t, #]
].
