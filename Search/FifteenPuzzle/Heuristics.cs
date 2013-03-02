using System;
using System.Collections.Generic;
using Search;

namespace FifteenPuzzle
{


    public class ManhattanDistanceHeuristic : IHeuristic
    {
        public string Name { get { return "Manhattan Distance"; } }

        public int Evaluate(StateBase state, StateBase goalState)
        {
            var start = (PuzzleState)state;
            var goal = (PuzzleState)goalState;

            var manhattanDistanceOff = 0;

            for (byte i = 0; i < start.Board.Length; i++)
            {
                for (byte j = 0; j < start.Board.LongLength; j++)
                {
                    var val = start.Board[i, j];

                    if (val == 0)
                    {
                        continue; // skip the blank
                    }

                    var correctPlace = goal.GetSpace(val);
                    manhattanDistanceOff += MathUtil.ManhattanDistance(i, j, correctPlace.Row, correctPlace.Col);
                }
            }

            return manhattanDistanceOff;
        }
    }

    public class MyHeuristic : IHeuristic
    {
        public string Name { get { return "My Heuristic, Linear Conflicts"; } }
        
        public int Evaluate(StateBase state, StateBase goalState)
        {
            var start = (PuzzleState)state;
            var goal = (PuzzleState)goalState;

            var manhattanDistanceOff = 0;

            var possibleRowConflicts = new Dictionary<byte, Tuple<byte, byte>>();
            var possibleColConflicts = new Dictionary<byte, Tuple<byte, byte>>();

            for (byte i = 0; i < start.Board.Length; i++)
            {
                for (byte j = 0; j < start.Board.LongLength; j++)
                {
                    var val = start.Board[i, j];

                    if (val == 0)
                    {
                        continue; // skip the blank
                    }

                    var correctPlace = goal.GetSpace(val);
                    manhattanDistanceOff += MathUtil.ManhattanDistance(i, j, correctPlace.Row, correctPlace.Col);

                    // now check for linear conflicts

                    if (i == correctPlace.Row) // in correct row?
                    {
                        if (j != correctPlace.Col) // and wrong column?
                        {
                            possibleRowConflicts.Add(i, Tuple.Create(j, correctPlace.Col));
                        }
                    }
                    else if (j == correctPlace.Col) // in correct col and wrong row?
                    {
                        possibleColConflicts.Add(j, Tuple.Create(i, correctPlace.Row));
                    }
                }
            }

            foreach (var kvp in possibleRowConflicts)
            {

            }

            return manhattanDistanceOff;
        }
    }
}
