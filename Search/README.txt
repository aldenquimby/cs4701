Alden Quimby
adq2101
CS4701 Assignment 2

----------------------
TO RUN MY CODE
----------------------
	- For all 6 test cases, double click "15-puzzle.exe" on any Windows machine
	- To run a specific case, from a command line type "15-puzzle.exe <init_state> <goal_state>"
	- After completion, you will be prompted to enter more searches if you wish 
	
	By default, this runs all searches and prints output. DFS is run with different depth
	limits 6, 12, and 18 as specified by the assignment. Note that each algorithm runs on 
	it's own thread, so the order of algorithm output may change with each trial.
	
	I've provided example output from my program below.
	
	I followed the format specified by the assignment addendum.


----------------------
NOTES ON MY CODE
----------------------
	I did this assignment in C#. It is very similar to Java (but better!), so all syntax
	should be easy to understand.

	Generic search code is in the "Search" folder, and this is compiled into "Search.dll".
	
	"Searcher.cs" contains the meat of my search algorithms. It has a generic search 
	algorithm that performs all of my searches. Each specific search passes in a different
	"open list comparator" which is used to keep the open list (implemented as a heap) sorted.
	I cache all heuristic calculations and keep a hash table of previously visited states to 
	speed things up.

	In order to use the generic search algorithms, "StateBase" and "IHeuristic" must be implmented.
	My code in the "FifteenPuzzle" folder implements both of these for the 15-puzzle. This code
	is compiled into "15-puzzle.exe" and includes the test driver.

	For "MyHeuristic", I altered the Manhattan Distance heuristic to account for linear
	conflicts - both horizontal and vertical. For puzzles where there are no linear
	conflicts, my heuristic is equivalent to the Manhattan Distance heuristic. You will see
	this when you run my example test cases for goal state 2, as they have no linear conflicts.
	
	My comments in code should answer any additional questions. 


----------------------
OUTPUT FOR 6 TESTS
----------------------

	**********************
	RUNNING ALL TEST CASES
	**********************

	Goal State 1: ((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15) (0 0))
	EASY Init State: ((1 2 3 0) (4 5 6 7) (8 9 10 11) (12 13 14 15) (0 3))

	BFS:
	((("W" "W" "W") 3) 54 15 23 17)

	DFS (limit 6):
	((("W" "W" "W") 3) 284 91 0 194)

	UCS:
	((("W" "W" "W") 3) 54 15 23 17)

	IDDFS:
	((("W" "W" "W") 3) 22 6 0 17)

	A* (MyHeuristic):
	((("W" "W" "W") 3) 8 2 3 4)

	Greedy (MyHeuristic):
	((("W" "W" "W") 3) 8 2 3 4)

	Greedy (Manhattan):
	((("W" "W" "W") 3) 8 2 3 4)

	A* (Manhattan):
	((("W" "W" "W") 3) 8 2 3 4)

	DFS (limit 12):
	((("W" "W" "W") 3) 23502 8806 0 14697)

	DFS (limit 18):
	((("S" "W" "N" "W" "W" "S" "E" "N" "E" "S" "E" "N" "W" "W" "S" "W" "N") 17) 488938 206457 11 282471)


	Goal State 1: ((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15) (0 0))
	MED Init State: ((1 2 6 3) (4 5 10 7) (0 9 14 11) (8 12 13 15) (2 0))

	DFS (limit 6):
	(((FAIL) 0) 378 123 0 256)

	A* (MyHeuristic):
	((("S" "E" "E" "N" "N" "N" "W" "W") 8) 25 7 10 9)

	Greedy (MyHeuristic):
	((("S" "E" "E" "N" "N" "N" "W" "W") 8) 25 7 10 9)

	A* (Manhattan):
	((("S" "E" "E" "N" "N" "N" "W" "W") 8) 25 7 10 9)

	Greedy (Manhattan):
	((("S" "E" "E" "N" "N" "N" "W" "W") 8) 25 7 10 9)

	IDDFS:
	((("S" "E" "E" "N" "N" "N" "W" "W") 8) 757 247 6 505)

	BFS:
	((("S" "E" "E" "N" "N" "N" "W" "W") 8) 2481 830 861 791)

	UCS:
	((("S" "E" "E" "N" "N" "N" "W" "W") 8) 2481 830 861 791)

	DFS (limit 12):
	((("S" "E" "E" "N" "N" "N" "W" "W") 8) 14597 5281 6 9311)

	DFS (limit 18):
	((("S" "E" "N" "E" "N" "N" "W" "S" "E" "S" "W" "S" "E" "N" "N" "W" "N" "W") 18) 862623 375136 17 487471)


	Goal State 1: ((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15) (0 0))
	HARD Init State: ((1 2 3 7) (4 5 6 15) (8 9 11 0) (12 13 14 10) (2 3))

	DFS (limit 6):
	(((FAIL) 0) 378 123 0 256)

	Greedy (MyHeuristic):
	((("N" "N" "W" "W" "S" "S" "S" "E" "E" "N" "W" "S" "W" "N" "N" "N" "W") 17) 408 128 151 130)

	A* (MyHeuristic):
	((("S" "W" "N" "E" "N" "N" "W" "S" "S" "S" "E" "N" "W" "N" "N" "W" "W") 17) 1023 341 353 330)

	Greedy (Manhattan):
	((("N" "N" "W" "W" "S" "S" "S" "E" "E" "N" "W" "S" "W" "N" "N" "N" "W") 17) 1907 732 599 577)

	A* (Manhattan):
	((("S" "W" "N" "E" "N" "N" "W" "S" "S" "S" "E" "N" "W" "N" "N" "W" "W") 17) 1231 416 417 399)

	DFS (limit 12):
	(((FAIL) 0) 31741 12138 0 19604)

	DFS (limit 18):
	((("N" "N" "W" "W" "S" "S" "S" "E" "E" "N" "W" "S" "W" "N" "N" "N" "W") 17) 143070 58168 15 84888)

	IDDFS:
	((("N" "N" "W" "W" "S" "S" "S" "E" "E" "N" "W" "S" "W" "N" "N" "N" "W") 17) 70481 27770 15 42697)

	BFS:
	((("N" "N" "W" "W" "S" "S" "S" "E" "E" "N" "W" "S" "W" "N" "N" "N" "W") 17) 996787 371139 305048 320601)

	UCS:
	((("N" "N" "W" "W" "S" "S" "S" "E" "E" "N" "W" "S" "W" "N" "N" "N" "W") 17) 996787 371139 305048 320601)


	Goal State 2: ((1 2 3 4) (8 7 6 5) (9 10 11 12) (0 15 14 13) (3 0))
	EASY Init State: ((0 2 3 4) (1 7 6 5) (8 10 11 12) (9 15 14 13) (0 0))

	DFS (limit 6):
	((("S" "S" "S") 3) 8 2 3 4)

	DFS (limit 12):
	((("S" "S" "S") 3) 8 2 3 4)

	DFS (limit 18):
	((("S" "S" "S") 3) 8 2 3 4)

	IDDFS:
	((("S" "S" "S") 3) 8 2 3 4)

	BFS:
	((("S" "S" "S") 3) 22 6 9 8)

	UCS:
	((("S" "S" "S") 3) 22 6 9 8)

	A* (MyHeuristic):
	((("S" "S" "S") 3) 8 2 3 4)

	Greedy (MyHeuristic):
	((("S" "S" "S") 3) 8 2 3 4)

	Greedy (Manhattan):
	((("S" "S" "S") 3) 8 2 3 4)

	A* (Manhattan):
	((("S" "S" "S") 3) 8 2 3 4)


	Goal State 2: ((1 2 3 4) (8 7 6 5) (9 10 11 12) (0 15 14 13) (3 0))
	MED Init State: ((2 3 4 0) (1 8 6 5) (10 7 11 12) (9 15 14 13) (0 3))

	DFS (limit 6):
	(((FAIL) 0) 296 95 0 202)

	IDDFS:
	((("W" "W" "W" "S" "E" "S" "W" "S") 8) 1275 426 1 849)

	BFS:
	((("W" "W" "W" "S" "E" "S" "W" "S") 8) 2684 893 939 853)

	UCS:
	((("W" "W" "W" "S" "E" "S" "W" "S") 8) 2684 893 939 853)

	A* (MyHeuristic):
	((("W" "W" "W" "S" "E" "S" "W" "S") 8) 24 7 9 9)

	Greedy (MyHeuristic):
	((("W" "W" "W" "S" "E" "S" "W" "S") 8) 24 7 9 9)

	Greedy (Manhattan):
	((("W" "W" "W" "S" "E" "S" "W" "S") 8) 24 7 9 9)

	A* (Manhattan):
	((("W" "W" "W" "S" "E" "S" "W" "S") 8) 24 7 9 9)

	DFS (limit 12):
	((("W" "W" "W" "S" "E" "S" "W" "S") 8) 24442 9151 1 15291)

	DFS (limit 18):
	((("S" "W" "N" "W" "W" "S" "E" "N" "E" "S" "E" "N" "W" "W" "S" "S" "W" "S") 18) 488898 206439 12 282448)


	Goal State 2: ((1 2 3 4) (8 7 6 5) (9 10 11 12) (0 15 14 13) (3 0))
	HARD Init State: ((7 1 2 3) (8 6 5 4) (0 9 10 11) (15 14 13 12) (2 0))

	DFS (limit 6):
	(((FAIL) 0) 378 123 0 256)

	A* (MyHeuristic):
	((("N" "N" "E" "E" "E" "S" "W" "W" "W" "S" "E" "E" "E" "S" "W" "W" "W") 17) 122 38 45 40)

	Greedy (MyHeuristic):
	((("E" "E" "E" "S" "W" "W" "W" "N" "N" "N" "E" "E" "E" "S" "W" "W" "W" "S" "S") 19) 61 19 22 21)

	Greedy (Manhattan):
	((("E" "E" "E" "S" "W" "W" "W" "N" "N" "N" "E" "E" "E" "S" "W" "W" "W" "S" "S") 19) 61 19 22 21)

	A* (Manhattan):
	((("N" "N" "E" "E" "E" "S" "W" "W" "W" "S" "E" "E" "E" "S" "W" "W" "W") 17) 122 38 45 40)

	DFS (limit 12):
	(((FAIL) 0) 31742 12139 0 19604)

	DFS (limit 18):
	((("N" "N" "E" "E" "E" "S" "W" "W" "W" "S" "E" "E" "E" "S" "W" "W" "W") 17) 218847 90784 3 128061)

	IDDFS:
	((("N" "N" "E" "E" "E" "S" "W" "W" "W" "S" "E" "E" "E" "S" "W" "W" "W") 17) 108268 43588 3 64678)

	UCS:
	((("N" "N" "E" "E" "E" "S" "W" "W" "W" "S" "E" "E" "E" "S" "W" "W" "W") 17) 1030969 383816 315569 331585)

	BFS:
	((("N" "N" "E" "E" "E" "S" "W" "W" "W" "S" "E" "E" "E" "S" "W" "W" "W") 17) 1030969 383816 315569 331585)
