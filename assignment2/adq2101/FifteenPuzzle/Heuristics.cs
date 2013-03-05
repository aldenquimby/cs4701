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
                    manhattanDistanceOff += ManhattanDistance(i, j, correctPlace.Row, correctPlace.Col);
                }
            }

            return manhattanDistanceOff;
        }

        public static int ManhattanDistance(byte fromRow, byte fromCol, byte toRow, byte toCol)
        {
            return Math.Abs(fromRow - toRow) + Math.Abs(fromCol - toCol);
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
                    manhattanDistanceOff += ManhattanDistanceHeuristic.ManhattanDistance(i, j, correctPlace.Row, correctPlace.Col);

                    // now check for linear conflicts

                    // in correct row and wrong column?
                    if (i == correctPlace.Row && j != correctPlace.Col) 
                    {
                        if (!possibleRowConflicts.ContainsKey(i))
                        {
                            possibleRowConflicts[i] = new List<Tuple<byte, byte>>();
                        }
                        possibleRowConflicts[i].Add(Tuple.Create(j, correctPlace.Col));
                    }

                    // in correct col and wrong row?
                    if (j == correctPlace.Col && i != correctPlace.Row) 
                    {
                        if (!possibleColConflicts.ContainsKey(j))
                        {
                            possibleColConflicts[j] = new List<Tuple<byte, byte>>();
                        }
                        possibleColConflicts[j].Add(Tuple.Create(i, correctPlace.Row));
                    }
                }
            }

            var linearConflicts = 0;

            foreach (var possibleConflicts in possibleRowConflicts.Values.Concat(possibleColConflicts.Values))
            {
                var conflict = possibleConflicts.First();

                linearConflicts += possibleConflicts.Skip(1).Count(x => (x.Item1 > conflict.Item1 && x.Item2 < conflict.Item2) || 
                                                                  (x.Item1 < conflict.Item1 && x.Item2 > conflict.Item2));
            }

            return manhattanDistanceOff + 2*linearConflicts;
        }
    }
}
