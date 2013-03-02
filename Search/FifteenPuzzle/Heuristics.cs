using System;
using System.Collections.Generic;
using System.Linq;
using Search;

namespace FifteenPuzzle
{
    public class ManhattanDistanceHeuristic : HeurisitcBase<PuzzleState>
    {
        public override string Name { get { return "Manhattan"; } }

        public override int Evaluate(PuzzleState state, PuzzleState goal)
        {
            var manhattanDistanceOff = 0;

            for (byte i = 0; i < 4; i++)
            {
                for (byte j = 0; j < 4; j++)
                {
                    var val = state.Board[i, j];

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

    public class MyHeuristic : HeurisitcBase<PuzzleState>
    {
        public override string Name { get { return "MyHeuristic"; } }

        public override int Evaluate(PuzzleState state, PuzzleState goal)
        {
            var manhattanDistanceOff = 0;

            var possibleRowConflicts = new Dictionary<byte, List<Tuple<byte, byte>>>();
            var possibleColConflicts = new Dictionary<byte, List<Tuple<byte, byte>>>();

            for (byte i = 0; i < 4; i++)
            {
                for (byte j = 0; j < 4; j++)
                {
                    var val = state.Board[i, j];

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
                            if (!possibleRowConflicts.ContainsKey(i))
                            {
                                possibleRowConflicts[i] = new List<Tuple<byte, byte>>();
                            }
                            possibleRowConflicts[i].Add(Tuple.Create(j, correctPlace.Col));
                        }
                    }
                    else if (j == correctPlace.Col) // in correct col and wrong row?
                    {
                        if (!possibleColConflicts.ContainsKey(j))
                        {
                            possibleRowConflicts[j] = new List<Tuple<byte, byte>>();
                        }
                        possibleRowConflicts[j].Add(Tuple.Create(i, correctPlace.Row));
                    }
                }
            }

            var linearConflicts = 0;

            foreach (var possibleConflicts in possibleRowConflicts.Values.Concat(possibleColConflicts.Values))
            {
                var conflict = possibleConflicts.First();
                possibleConflicts.RemoveAt(0);

                linearConflicts += possibleConflicts
                    .Where(other => (other.Item1 > conflict.Item1 && other.Item2 < conflict.Item2) || 
                                    (other.Item1 < conflict.Item1 && other.Item2 > conflict.Item2))
                    .Sum(x => 2);
            }

            return manhattanDistanceOff + linearConflicts;
        }
    }
}
