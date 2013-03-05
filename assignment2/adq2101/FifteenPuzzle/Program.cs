using System;
using System.Collections.Generic;
using System.Threading;
using Search;

namespace FifteenPuzzle
{
    public class Program
    {
        public static void Main(string[] args)
        {
            try
            {
                // no arguments means run all test cases
                if (args.Length == 0)
                {
                    RunAllTestCases();
                }
                // 2 arguments means <init_state> <goal_state>
                else if (args.Length == 2)
                {
                    RunSingleSearch(args[0], args[1]);
                }

                AskForInput();
            }
            catch
            {
                Console.WriteLine("Oops, something went really wrong!");
            }
        }

        // runs easy/med/hard tests against both goal states
        private static void RunAllTestCases()
        {
            Console.WriteLine("**********************");
            Console.WriteLine("RUNNING ALL TEST CASES");
            Console.WriteLine("**********************");

            const string goalState1 = "((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15) (0 0))";
            const string goal1Easy = "((1 2 3 0) (4 5 6 7) (8 9 10 11) (12 13 14 15) (0 3))";
            const string goal1Med = "((1 2 6 3) (4 5 10 7) (0 9 14 11) (8 12 13 15) (2 0))";
            const string goal1Hard = "((1 2 3 7) (4 5 6 15) (8 9 11 0) (12 13 14 10) (2 3))";

            Console.WriteLine("\nGoal State 1: " + goalState1);
            Console.WriteLine("EASY Init State: " + goal1Easy + "\n");
            RunSingleSearch(goal1Easy, goalState1);

            Console.WriteLine("\nGoal State 1: " + goalState1);
            Console.WriteLine("MED Init State: " + goal1Med + "\n");
            RunSingleSearch(goal1Med, goalState1);

            Console.WriteLine("\nGoal State 1: " + goalState1);
            Console.WriteLine("HARD Init State: " + goal1Hard + "\n");
            RunSingleSearch(goal1Hard, goalState1);

            const string goalState2 = "((1 2 3 4) (8 7 6 5) (9 10 11 12) (0 15 14 13) (3 0))";
            const string goal2Easy = "((0 2 3 4) (1 7 6 5) (8 10 11 12) (9 15 14 13) (0 0))";
            const string goal2Med = "((2 3 4 0) (1 8 6 5) (10 7 11 12) (9 15 14 13) (0 3))";
            const string goal2Hard = "((7 1 2 3) (8 6 5 4) (0 9 10 11) (15 14 13 12) (2 0))";

            Console.WriteLine("\nGoal State 2: " + goalState2);
            Console.WriteLine("EASY Init State: " + goal2Easy + "\n");
            RunSingleSearch(goal2Easy, goalState2);

            Console.WriteLine("\nGoal State 2: " + goalState2);
            Console.WriteLine("MED Init State: " + goal2Med + "\n");
            RunSingleSearch(goal2Med, goalState2);

            Console.WriteLine("\nGoal State 2: " + goalState2);
            Console.WriteLine("HARD Init State: " + goal2Hard + "\n");
            RunSingleSearch(goal2Hard, goalState2);
        }

        // runs all search algorithms for start and goal state
        private static void RunSingleSearch(string initStateString, string goalStateString)
        {
            var initState = new PuzzleState(initStateString);
            var goalState = new PuzzleState(goalStateString);
            RunSingleSearch(initState, goalState);
        }

        // runs all search algorithms for start and goal state
        private static void RunSingleSearch(PuzzleState initState, PuzzleState goalState)
        {
            var searcher = new Searcher(initState, goalState);

            // for 15-puzzle, cost from one state to next is always 1
            CostFunc cost = (fromState, toState) => 1;

            // do each search on a separate thread to speed things up
            var searchThreads = new List<Thread>
            {
                new Thread(() => Console.WriteLine(searcher.DepthFirstSearch(6).ToString())),
                new Thread(() => Console.WriteLine(searcher.DepthFirstSearch(12).ToString())),
                new Thread(() => Console.WriteLine(searcher.DepthFirstSearch(18).ToString())),
                new Thread(() => Console.WriteLine(searcher.IterativeDeepeningDfs().ToString())),
                new Thread(() => Console.WriteLine(searcher.BreadthFirstSearch().ToString())),
                new Thread(() => Console.WriteLine(searcher.UniformCostSearch(cost).ToString())),
                new Thread(() => Console.WriteLine(searcher.AStarSearch(cost, new MyHeuristic()).ToString())),
                new Thread(() => Console.WriteLine(searcher.GreedySearch(cost, new MyHeuristic()).ToString())),
                new Thread(() => Console.WriteLine(searcher.GreedySearch(cost, new ManhattanDistanceHeuristic()).ToString())),
                new Thread(() => Console.WriteLine(searcher.AStarSearch(cost, new ManhattanDistanceHeuristic()).ToString())),
            };

            // start all searches
            searchThreads.ForEach(x => x.Start());

            // wait for all searches to complete
            searchThreads.ForEach(x => x.Join());
        }

        public static void AskForInput()
        {
            while (true)
            {
                try
                {
                    Console.WriteLine("\n\nEnter a 15-puzzle start state (q to quit): ");
                    var initStateString = Console.ReadLine();
                    if ("q".Equals(initStateString))
                    {
                        return;
                    }

                    Console.WriteLine("Enter a 15-puzzle goal state: ");
                    var goalStateString = Console.ReadLine();

                    RunSingleSearch(initStateString, goalStateString);
                }
                catch
                {
                    Console.WriteLine("Oops, I didn't recognize that input. Please try again.");
                }
            }
        }
    }
}