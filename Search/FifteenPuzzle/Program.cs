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
            //TODO: uncomment
            //if (args.Length != 2)
            //{
            //    Console.WriteLine("Usage: 15-puzzle <initial_state> <goal_state>");
            //    return;
            //}

            while (true)
            {
                // TODO: remove
                Console.WriteLine("Start state (0 for easy, 1 for medium, 2 for hard): ");
                var initStateString = Console.ReadLine();
                if ("0".Equals(initStateString))
                {
                    initStateString = "((1 2 3 0) (4 5 6 7) (8 9 10 11) (12 13 14 15) (0 3))";
                }
                else if ("1".Equals(initStateString))
                {
                    initStateString = "((1 2 6 3) (4 5 10 7) (0 9 14 11) (8 12 13 15) (2 0))";
                }
                else if ("2".Equals(initStateString))
                {
                    initStateString = "((1 5 3 7) (4 9 2 11) (8 13 10 14) (12 15 0 6) (3 2))";
                }
                Console.WriteLine("Goal state (0 for default): ");
                var goalStateString = Console.ReadLine();
                if ("0".Equals(goalStateString))
                {
                    goalStateString = "((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15) (0 0))";
                }
                Console.WriteLine("Enter a depth limit: ");
                var depthLimit = int.Parse(Console.ReadLine() ?? "20");

                Console.WriteLine();

                //var initialState = new PuzzleState(args[0]);
                //var goalState = new PuzzleState(args[1]);
                var initialState = new PuzzleState(initStateString);
                var goalState = new PuzzleState(goalStateString);

                CostFunc cost = (fromState, toState) => 1;

                var searcher = new GenericSearch(initialState, goalState);

                // do each search on a separate thread to speed things up
                var searchThreads = new List<Thread>
                {
                    new Thread(() => Console.WriteLine(searcher.DepthFirstSearch(depthLimit).Print())),
                    new Thread(() => Console.WriteLine(searcher.IterativeDeepeningDfs().Print())),
                    new Thread(() => Console.WriteLine(searcher.BreadthFirstSearch().Print())),
                    new Thread(() => Console.WriteLine(searcher.UniformCostSearch(cost).Print())),
                    new Thread(() => Console.WriteLine(searcher.GreedySearch(cost, new ManhattanDistanceHeuristic()).Print())),
                    new Thread(() => Console.WriteLine(searcher.AStarSearch(cost, new ManhattanDistanceHeuristic()).Print())),
                    new Thread(() => Console.WriteLine(searcher.AStarSearch(cost, new MyHeuristic()).Print())),
                    new Thread(() => Console.WriteLine(searcher.GreedySearch(cost, new MyHeuristic()).Print())),
                };

                // start all searches
                searchThreads.ForEach(x => x.Start());

                // wait for all searches to complete
                searchThreads.ForEach(x => x.Join());
            }
        }
    }
}
