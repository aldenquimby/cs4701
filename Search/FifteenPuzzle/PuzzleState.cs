using System;
using System.Collections.Generic;
using Search;

namespace FifteenPuzzle
{
    public struct PuzzleSpace
    {
        public PuzzleSpace(byte row, byte col) : this()
        {
            Row = row;
            Col = col;
        }

        public byte Row { get; private set; }
        public byte Col { get; private set; }
    }

    public class PuzzleState : StateBase
    {
        public PuzzleState(byte[,] board, Dictionary<byte, PuzzleSpace> valueLookup)
        {
            Board = board;
            _valueLookup = valueLookup;
        }

        public PuzzleState(string stateAsString)
        {
            // example stateAsString:
            // ((1 5 3 7) (4 9 2 11) (8 13 10 14) (12 15 0 6) (3 2))

            Board = new byte[4,4];

            var parts = stateAsString.Split(new []{") ("}, StringSplitOptions.None);

            for (byte i = 0; i < 4; i++)
            {
                var part = parts[i].TrimStart(new[] {'('}).TrimEnd(new[] {')'});
                var nums = part.Split(' ');
                for (byte j = 0; j < 4; j++)
                {
                    var num = byte.Parse(nums[j]);
                    Board[i, j] = num;
                    _valueLookup[num] = new PuzzleSpace(i, j);
                }
            }
        }

        private readonly Dictionary<byte, PuzzleSpace> _valueLookup = new Dictionary<byte, PuzzleSpace>();

        public byte[,] Board { get; private set; }

        #region equality

        public override bool Equals(object obj)
        {
            return Equals(obj as PuzzleState);
        }

        public override bool Equals(StateBase other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            if (other as PuzzleState == null)
            {
                return false;
            }
            return other.GetHashCode().Equals(GetHashCode());
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hash = 17;
                for (var i = 0; i < 4; i++)
                {
                    for (var j = 0; j < 4; j++)
                    {
                        hash = hash * 397 + Board[i, j] ^ 23 + (i + j) ^ 19;
                    }
                }
                return hash;
            }
        }

        #endregion

        #region clone

        public PuzzleState Clone()
        {
            var valueLookup = new Dictionary<byte, PuzzleSpace>(16);
            var newBoard = new byte[4, 4];
            for (byte i = 0; i < 4; i++)
            {
                for (byte j = 0; j < 4; j++)
                {
                    var num = Board[i, j];
                    newBoard[i, j] = num;
                    valueLookup[num] = new PuzzleSpace(i, j);
                }
            }
            return new PuzzleState(newBoard, valueLookup);
        }

        #endregion

        #region operators

        public PuzzleState North()
        {
            var blank = GetSpace(0);
            if (blank.Row == 0)
            {
                return null;
            }
            var clone = Clone();
            var newRow = (byte) (blank.Row - 1);
            var valToSwap = clone.Board[newRow, blank.Col];
            clone.Board[blank.Row, blank.Col] = valToSwap;
            clone.Board[newRow, blank.Col] = 0;
            clone._valueLookup[0] = new PuzzleSpace(newRow, blank.Col);
            clone._valueLookup[valToSwap] = new PuzzleSpace(blank.Row, blank.Col);
            return clone;
        }

        public PuzzleState South()
        {
            var blank = GetSpace(0);
            if (blank.Row == 3)
            {
                return null;
            }
            var clone = Clone();
            var newRow = (byte)(blank.Row + 1);
            var valToSwap = clone.Board[newRow, blank.Col];
            clone.Board[blank.Row, blank.Col] = valToSwap;
            clone.Board[newRow, blank.Col] = 0;
            clone._valueLookup[0] = new PuzzleSpace(newRow, blank.Col);
            clone._valueLookup[valToSwap] = new PuzzleSpace(blank.Row, blank.Col);
            return clone;
        }

        public PuzzleState East()
        {
            var blank = GetSpace(0);
            if (blank.Col == 3)
            {
                return null;
            }
            var clone = Clone();
            var newCol = (byte)(blank.Col + 1);
            var valToSwap = clone.Board[blank.Row, newCol];
            clone.Board[blank.Row, blank.Col] = valToSwap;
            clone.Board[blank.Row, newCol] = 0;
            clone._valueLookup[0] = new PuzzleSpace(blank.Row, newCol);
            clone._valueLookup[valToSwap] = new PuzzleSpace(blank.Row, blank.Col);
            return clone;
        }

        public PuzzleState West()
        {
            var blank = GetSpace(0);
            if (blank.Col == 0)
            {
                return null;
            }
            var clone = Clone();
            var newCol = (byte)(blank.Col - 1);
            var valToSwap = clone.Board[blank.Row, newCol];
            clone.Board[blank.Row, blank.Col] = valToSwap;
            clone.Board[blank.Row, newCol] = 0;
            clone._valueLookup[0] = new PuzzleSpace(blank.Row, newCol);
            clone._valueLookup[valToSwap] = new PuzzleSpace(blank.Row, blank.Col);
            return clone;
        }

        #endregion

        public PuzzleSpace GetSpace(byte value)
        {
            return _valueLookup[value];
        }

        public override IDictionary<string, StateBase> Successors()
        {
            var dict = new Dictionary<string, StateBase>();
            var n = North();
            var s = South();
            var e = East();
            var w = West();
            if (n != null)
            {
                dict["N"] = n;
            }
            if (s != null)
            {
                dict["S"] = s;
            }
            if (e != null)
            {
                dict["E"] = e;
            }
            if (w != null)
            {
                dict["W"] = w;
            }
            return dict;
        }
    }
}
