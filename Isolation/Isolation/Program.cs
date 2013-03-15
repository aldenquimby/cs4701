using System;
using System.Collections.Generic;
using System.Linq;
using Search;

namespace Isolation
{
    public class Program
    {
        private static void TestMoveGenerator()
        {
            var b1 = new Board(InitBoard);
            var b2 = new Board(new List<string>(8)
            {
                "--------",
                "------o-",
                "--*****-",
                "--x-----",
                "------*-",
                "------*-",
                "--*---*-",
                "------*-",
            });

            Console.WriteLine(b1.ToString());
            Console.WriteLine();
            Console.WriteLine("Moves for x:");
            Console.WriteLine(string.Join("\n", MoveGenerator.I.GetMovesForX(b1).Select(x => x.ToString())));
            Console.WriteLine();

            Console.WriteLine(b2.ToString());
            Console.WriteLine();
            Console.WriteLine("Moves for x:");
            Console.WriteLine(string.Join("\n", MoveGenerator.I.GetMovesForX(b2).Select(x => x.ToString())));
            Console.WriteLine();

            Console.ReadKey();
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

            board.MoveX(myMove);
            
            Console.WriteLine("My move:");
            Console.WriteLine(myMove.ToString());
        }

        private static void OpponentMove(Board board)
        {
            Console.WriteLine("Enter opponent move (<row> <col>):");
            var input = Console.ReadLine() ?? "";

            while (true)
            {
                try
                {
                    var move = new BoardSpace(input);
                    board.MoveX(move);
                    return;
                }
                catch
                {
                    Console.WriteLine("Please enter a valid move!");
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

}
