using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Isolation
{
    public class Board : IEquatable<Board>
    {
        private readonly BoardSpaceValue[,] _board;

        private Player _playerToMove;
        public Player MyPlayer { get; private set; }
        public BoardSpace Xposition { get; private set; }
        public BoardSpace Oposition { get; private set; }

        // override of operator[]
        public BoardSpaceValue this[byte row, byte col]
        {
            get { return _board[row, col]; }
        }

        private Board(Player myPlayer)
        {
            MyPlayer = myPlayer;
            _board = new BoardSpaceValue[8, 8];
        }

        public static Board ConstructInitialBoard(Player myPlayer)
        {
            var board = new Board(myPlayer)
            {
                Xposition = new BoardSpace(0, 0),
                Oposition = new BoardSpace(7, 7),
                _playerToMove = Player.X,
            };

            for (byte i = 0; i < 8; i++)
            {
                for (byte j = 0; j < 8; j++)
                {
                    board._board[i, j] = BoardSpaceValue.Empty;
                }
            }

            board._board[0, 0] = BoardSpaceValue.PlayerX;
            board._board[7, 7] = BoardSpaceValue.PlayerO;

            return board;
        }

        public Board Copy()
        {
            var board = new Board(MyPlayer)
            {
                Xposition = Xposition,
                Oposition = Oposition,
                _playerToMove = _playerToMove,
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

        #region perform move

        public void Move(BoardSpace move)
        {
            if (_playerToMove == Player.X)
            {
                _board[Xposition.Row, Xposition.Col] = BoardSpaceValue.Filled;
                _board[move.Row, move.Col] = BoardSpaceValue.PlayerX;
                Xposition = move;
                _playerToMove = Player.O;
            }
            else
            {
                _board[Oposition.Row, Oposition.Col] = BoardSpaceValue.Filled;
                _board[move.Row, move.Col] = BoardSpaceValue.PlayerO;
                Oposition = move;
                _playerToMove = Player.X;
            }
        }

        #endregion

        #region MoveGenerator

        public List<BoardSpace> GetValidMoves()
        {
            return _playerToMove == Player.X ? GetMoves(Xposition) : GetMoves(Oposition);
        } 

        private List<BoardSpace> GetMoves(BoardSpace currentPosition)
        {
            var moves = new List<BoardSpace>();

            #region vertical moves

            // walk down from currentPosition
            for (var i = currentPosition.Row + 1; i < 8; i++)
            {
                if (_board[i, currentPosition.Col] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace((byte)i, currentPosition.Col));
                }
                else { break; }
            }

            // walk up from currentPosition
            for (var i = currentPosition.Row - 1; i >= 0; i--)
            {
                if (_board[i, currentPosition.Col] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace((byte)i, currentPosition.Col));
                }
                else { break; }
            }

            #endregion

            #region horizontal moves

            // walk right from currentPosition
            for (var j = currentPosition.Col + 1; j < 8; j++)
            {
                if (_board[currentPosition.Row, j] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace(currentPosition.Row, (byte)j));
                }
                else { break; }
            }

            // walk left from currentPosition
            for (var j = currentPosition.Col - 1; j >= 0; j--)
            {
                if (_board[currentPosition.Row, j] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace(currentPosition.Row, (byte)j));
                }
                else { break; }
            }

            #endregion

            #region diagonal moves

            // walk down-right from currentPosition
            for (int i = currentPosition.Row + 1, j = currentPosition.Col + 1; i < 8 && j < 8; i++, j++)
            {
                if (_board[i, j] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace((byte)i, (byte)j));
                }
                else { break; }
            }

            // walk down-left from currentPosition
            for (int i = currentPosition.Row + 1, j = currentPosition.Col - 1; i < 8 && j >=0; i++, j--)
            {
                if (_board[i, j] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace((byte)i, (byte)j));
                }
                else { break; }
            }

            // walk up-right from currentPosition
            for (int i = currentPosition.Row - 1, j = currentPosition.Col + 1; i >=0 && j < 8; i--, j++)
            {
                if (_board[i, j] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace((byte)i, (byte)j));
                }
                else { break; }
            }

            // walk up-left from currentPosition
            for (int i = currentPosition.Row - 1, j = currentPosition.Col - 1; i >=0 && j >=0; i--, j--)
            {
                if (_board[i, j] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace((byte)i, (byte)j));
                }
                else { break; }
            }

            #endregion

            return moves;
        } 

        private List<BoardSpace> GetMovesForX()
        {
            return GetMoves(Xposition);
        }

        private List<BoardSpace> GetMovesForO()
        {
            return GetMoves(Oposition);
        }

        public List<BoardSpace> GetMyValidMoves()
        {
            return MyPlayer == Player.X ? GetMoves(Xposition) : GetMoves(Oposition);
        }

        public List<BoardSpace> GetOpponentValidMoves()
        {
            return MyPlayer == Player.X ? GetMoves(Oposition) : GetMoves(Xposition);
        }

        public bool IsValidMove(BoardSpace move)
        {
            return GetValidMoves().Any(x => x.Equals(move));
        }

        #endregion

        #region equality and hashing

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

            for (byte i = 0; i < 8; i++)
            {
                for (byte j = 0; j < 8; j++)
                {
                    if (_board[i, j] != other._board[i, j])
                    {
                        return false;
                    }
                }
            }

            return true;
        }

        public override bool Equals(object obj)
        {
            return Equals(obj as Board);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var sum = 0;
                for (byte i = 0; i < 8; i++)
                {
                    for (byte j = 0; j < 8; j++)
                    {
                        var positionalValue = (i * 8 + (j + 1)) * ((byte)_board[i, j]);
                        sum += 17 ^ 23 * positionalValue.GetHashCode();
                    }
                }
                return sum;
            }
        }

        #endregion

        #region printing

        private static char GetCharFromSpace(BoardSpaceValue spaceValue)
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

        private static BoardSpaceValue GetSpaceFromChar(char c)
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
                    throw new Exception("Invalid board space.");
            }
        }

        //TODO: remove this constructor
        public Board(IList<string> boardRows, Player myPlayer) : this(myPlayer)
        {
            if (boardRows.Count != 8)
            {
                throw new ArgumentException("Invalid board.");
            }

            byte totalMoves = 0;

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
                    else if (space == BoardSpaceValue.PlayerO)
                    {
                        Oposition = new BoardSpace(i, j);
                    }
                    else if (space == BoardSpaceValue.Filled)
                    {
                        totalMoves++;
                    }
                }
            }

            _playerToMove = totalMoves % 2 == 0 ? Player.X : Player.O;
        }

        public override string ToString()
        {
            var builder = new StringBuilder();
            builder.AppendLine("  1 2 3 4 5 6 7 8"); //TODO remove
            for (byte i = 0; i < 8; i++)
            {
                builder.Append(i + 1).Append(" "); //TODO remove
                for (byte j = 0; j < 8; j++)
                {
                    builder.Append(GetCharFromSpace(_board[i, j])).Append(" ");
                }
                builder.AppendLine();
            }
            return builder.ToString();
        }

        #endregion
    }
}