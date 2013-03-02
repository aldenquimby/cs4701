using System;
using Search;

namespace FifteenPuzzle
{
    public class Program
    {
        public static void Main(string[] args)
        {
            if (args.Length != 2)
            {
                Console.WriteLine("Usage: 15-puzzle <initial_state> <goal_state>");
                return;
            }

            var initialState = new PuzzleState(args[0]);
            var goalState = new PuzzleState(args[1]);

            CostFunc cost = (fromState, toState) => 1;

            var search = new GenericSearch(initialState, goalState);

            Console.WriteLine(search.BreadthFirstSearch());
            Console.WriteLine(search.UniformCostSearch(cost));

            Console.WriteLine(search.AStarSearch(cost, new ManhattanDistanceHeuristic()));
            Console.WriteLine(search.GreedySearch(cost, new ManhattanDistanceHeuristic()));

            Console.WriteLine(search.AStarSearch(cost, new MyHeuristic()));
            Console.WriteLine(search.GreedySearch(cost, new MyHeuristic()));

            //“((1 5 3 7) (4 9 2 11) (8 13 10 14) (12 15 0 6) (3 2))” 
            //“((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15) (0 0)))”

        }
    }
}
