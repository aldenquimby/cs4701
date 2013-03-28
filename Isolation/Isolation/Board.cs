using System;
using System.Collections.Generic;
using System.Text;

namespace Isolation
{
    public class Board : IEquatable<Board>
    {
        public Player MyPlayer { get; private set; }

        private BoardSpaceValue[,] _board;

        public BoardSpace Xposition { get; private set; }
        public BoardSpace Oposition { get; private set; }

        public BoardSpaceValue this[byte row, byte col]
        {
            get { return _board[row, col]; }
        }

        private char GetCharFromSpace(BoardSpaceValue spaceValue)
        {
            switch (spaceValue)
            {
                case BoardSpaceValue.Empty:
                    return '-';
                case BoardSpaceValue.Filled:
                    return '*';
                case BoardSpaceValue.PlayerO:
                    return 'o';
                case BoardSpaceValue.PlayerX:
                    return 'x';
                default:
                    throw new Exception("Unhandled board space.");
            }
        }

        private BoardSpaceValue GetSpaceFromChar(char c)
        {
            switch (c)
            {
                case '*':
                    return BoardSpaceValue.Filled;
                case '-':
                    return BoardSpaceValue.Empty;
                case 'o':
                case 'O':
                    return BoardSpaceValue.PlayerO;
                case 'x':
                case 'X':
                    return BoardSpaceValue.PlayerX;
                default:
                    throw new ArgumentException("Invalid board space.");
            }
        }

        private Board(Player myPlayer)
        {
            MyPlayer = myPlayer;
        }

        public Board(IList<string> boardRows, Player myPlayer) : this(myPlayer)
        {
            _board = new BoardSpaceValue[8,8];

            if (boardRows.Count != 8)
            {
                throw new ArgumentException("Invalid board.");
            }

            for (byte i = 0; i < 8; i++)
            {
                var row = boardRows[i];

                if (row.Length != 8)
                {
                    throw new ArgumentException("Invalid board.");
                }

                for (byte j = 0; j < 8; j++)
                {
                    var space = GetSpaceFromChar(row[j]);
                    _board[i, j] = space;

                    if (space == BoardSpaceValue.PlayerX)
                    {
                        Xposition = new BoardSpace(i, j);
                    }
                    if (space == BoardSpaceValue.PlayerO)
                    {
                        Oposition = new BoardSpace(i, j);
                    }
                }
            }
        }

        public Board Copy()
        {
            var board = new Board(MyPlayer)
            {
                Xposition = Xposition,
                Oposition = Oposition,
                _board = new BoardSpaceValue[8, 8]
            };

            for (byte i = 0; i < 8; i++)
            {
                for (byte j = 0; j < 8; j++)
                {
                    board._board[i, j] = _board[i, j];
                }
            }

            return board;
        }

        #region win/lose/forfeit

        public Player? Winner { get; private set; }

        public void Forfeit()
        {
            Winner = MyPlayer == Player.X ? Player.O : Player.X;
        }

        public void ForfeitOpponent()
        {
            Winner = MyPlayer;
        }

        #endregion

        public void MoveMe(BoardSpace move)
        {
            
        }

        public void MoveOpponent(BoardSpace move)
        {
            
        }

        public bool IsValidOpponentMove(BoardSpace move)
        {
            // TODO: validate that opponent can move here
            return true;
        }

        public override string ToString()
        {
            var builder = new StringBuilder();
            builder.AppendLine("  1 2 3 4 5 6 7 8"); //TODO remove
            for (var i = 0; i < 8; i++)
            {
                builder.Append(i+1).Append(" "); //TODO remove
                for (var j = 0; j < 8; j++)
                {
                    builder.Append(GetCharFromSpace(_board[i, j])).Append(" ");
                }
                builder.AppendLine();
            }
            return builder.ToString();
        }

        #region equality and hashing

        // unique idenifier for a board state
        private int GetSum()
        {
            var sum = 0;
            for (var i = 0; i < 8; i++)
            {
                for (var j = 0; j < 8; j++)
                {
                    // positionNumber * positionValue
                    sum += (i * 8 + (j + 1)) * ((byte)_board[i, j]);
                }
            }
            return sum;
        }

        public bool Equals(Board other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return GetSum() == other.GetSum();
        }

        public override bool Equals(object obj)
        {
            return Equals(obj as Board);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return 17 ^ (23 * GetSum());
            }
        }

        #endregion
    }

    public class BoardSpace
    {
        public byte Row { get; private set; }
        public byte Col { get; private set; }

        public BoardSpace(byte row, byte col)
        {
            Row = row;
            Col = col;
        }

        public BoardSpace(string fromString)
        {
            try
            {
                var parts = fromString.TrimStart('(').TrimEnd(')').Split(' ');

                // board spaces are stored 0-7, displayed 1-8, so subtract 1
                Row = (byte)(byte.Parse(parts[0]) - 1);
                Col = (byte)(byte.Parse(parts[1]) - 1);
            }
            catch
            {
                throw new ArgumentException("Invalid board space string.");
            }
        }

        public override string ToString()
        {
            // board spaces are stored 0-7, displayed 1-8, so add 1
            return string.Format("({0} {1})", Row + 1, Col + 1);
        }
    }
}