using System;
using System.Collections.Generic;
using System.Text;

namespace Isolation
{
    public class Program
    {
        private static string GetPlayer()
        {
            Console.WriteLine("Are you player 'X' or player 'O'?");

            var input = Console.ReadLine();

            while (input == null || !input.Equals("O") || !input.Equals("X"))
            {
                Console.WriteLine("Please enter 'X' or 'O'.");
                input = Console.ReadLine();
            }

            return input;
        }

        private static void MyMove(Board board)
        {
            MoveTimer.I.StartTimer();

            var myMove = new BoardSpace(5, 5);

            board.Move(myMove);
            
            var move = string.Format("({0} {1})", myMove.Row, myMove.Col);
            
            Console.WriteLine("My move:");
            Console.WriteLine(move);
        }

        private static void OpponentMove(Board board)
        {
            Console.WriteLine("Enter opponent move (<row> <col>):");
            var input = Console.ReadLine() ?? "";

            while (true)
            {
                try
                {
                    var parts = input.TrimStart('(').TrimEnd(')').Split(' ');
                    var row = byte.Parse(parts[0]);
                    var col = byte.Parse(parts[1]);
                    
                    var move = new BoardSpace(row, col);
                    board.Move(move);

                    return;
                }
                catch
                {
                    Console.WriteLine("Please enter a valid move!");
                }
            }
        }

        public static void Main(string[] args)
        {
            var isFirst = GetPlayer().Equals("X");

            var board = new Board(InitBoard);

            if (isFirst)
            {
                while (!board.IsGameOver())
                {
                    MyMove(board);
                    OpponentMove(board);
                }
            }
            else
            {
                while (!board.IsGameOver())
                {
                    OpponentMove(board);
                    MyMove(board);
                }                
            }
        }

        private static readonly List<string> InitBoard = new List<string>(8)
        {
            "x-------",
            "--------",
            "--------",
            "--------",
            "--------",
            "--------",
            "--------",
            "-------o",
        };
    }

    public enum BoardSpaceValue : byte
    {
        Empty = 0,
        Filled = 1,
        PlayerX = 2,
        PlayerO = 3,
    }

    public struct BoardSpace
    {
        public BoardSpace(byte row, byte col) : this()
        {
            Row = row;
            Col = col;
        }

        public byte Row { get; private set; }
        public byte Col { get; private set; }
    }

    public class Board
    {
        private BoardSpaceValue[,] _board;

        public BoardSpace Xposition { get; private set; }
        public BoardSpace Oposition { get; private set; }

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

        public Board(IList<string> boardRows)
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

        public bool IsGameOver()
        {
            return false;
        }

        public void Move(BoardSpace move)
        {
            
        }

        public override string ToString()
        {
            var builder = new StringBuilder();
            for (var i = 0; i < 8; i++)
            {
                for (var j = 0; j < 8; j++)
                {
                    builder.Append(GetCharFromSpace(_board[i, j]))
                           .Append(" ");
                }
                builder.AppendLine();
            }
            return builder.ToString();
        }

        public List<BoardSpace> GetMovesForX()
        {
            var moves = new List<BoardSpace>();

            #region horizontal moves

            for (var i = (byte)(Xposition.Row + 1); i < 8; i++)
            {
                if (_board[i, Xposition.Col] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace(i, Xposition.Col));
                }
                else { break; }
            }
            
            for (var i = (byte)(Xposition.Row - 1); i < 8; i--) // byte wraps to 255
            {
                if (_board[i, Xposition.Col] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace(i, Xposition.Col));
                }
                else { break; }
            }

            #endregion

            #region vertical moves

            for (var j = (byte)(Xposition.Col + 1); j < 8; j++)
            {
                if (_board[Xposition.Row, j] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace(Xposition.Row, j));
                }
                else { break; }
            }

            for (var j = (byte)(Xposition.Col - 1); j < 8; j--) // byte wraps to 255
            {
                if (_board[Xposition.Row, j] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace(Xposition.Row, j));
                }
                else { break; }
            }

            #endregion

            #region diagonal moves

            for (byte i = (byte)(Xposition.Row + 1), j = (byte)(Xposition.Col + 1); i < 8 && j < 8; i++, j++)
            {
                if (_board[i, j] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace(i, j));
                }
                else { break; }
            }

            for (byte i = (byte)(Xposition.Row - 1), j = (byte)(Xposition.Col - 1); i < 8 && j < 8; i--, j--)
            {
                if (_board[i, j] == BoardSpaceValue.Empty)
                {
                    moves.Add(new BoardSpace(i, j));
                }
                else { break; }
            }

            #endregion

            return moves;
        } 
    }
}
