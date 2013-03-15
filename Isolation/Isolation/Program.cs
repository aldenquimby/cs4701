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

        private static void MyMove(Board b)
        {
            MoveTimer.I.StartTimer();

            byte row = 5;
            byte col = 5;

            var move = string.Format("({0},{1})", row, col);
            Console.WriteLine(move);
        }

        private static void OpponentMove()
        {
        }

        public static void Main(string[] args)
        {
            var isFirst = GetPlayer().Equals("X");

            var board = new Board(InitBoard);

            if (isFirst)
            {
                MyMove(board);
            }
            else
            {
                OpponentMove();
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

    public enum BoardSpace : byte
    {
        Empty = 0,
        Filled = 1,
        PlayerX = 2,
        PlayerO = 3,
    }

    public class Board
    {
        private BoardSpace[,] _board;

        private char GetCharFromSpace(BoardSpace space)
        {
            switch (space)
            {
                case BoardSpace.Empty:
                    return '-';
                case BoardSpace.Filled:
                    return '*';
                case BoardSpace.PlayerO:
                    return 'o';
                case BoardSpace.PlayerX:
                    return 'x';
                default:
                    throw new Exception("Unhandled board space.");
            }
        }

        private BoardSpace GetSpaceFromChar(char c)
        {
            switch (c)
            {
                case '*':
                    return BoardSpace.Filled;
                case '-':
                    return BoardSpace.Empty;
                case 'o':
                case 'O':
                    return BoardSpace.PlayerO;
                case 'x':
                case 'X':
                    return BoardSpace.PlayerX;
                default:
                    throw new ArgumentException("Invalid board space.");
            }
        }

        public Board(IList<string> boardRows)
        {
            _board = new BoardSpace[8,8];

            if (boardRows.Count != 8)
            {
                throw new ArgumentException("Invalid board.");
            }

            for (var i = 0; i < 8; i++)
            {
                var row = boardRows[i];

                if (row.Length != 8)
                {
                    throw new ArgumentException("Invalid board.");
                }

                for (var j = 0; j < 8; j++)
                {
                    _board[i, j] = GetSpaceFromChar(row[j]);
                }
            }
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
    }
}
