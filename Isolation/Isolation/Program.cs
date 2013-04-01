using System;
using System.Collections.Generic;

namespace Isolation
{
    public class Program
    {
        //private static void TestMoveGenerator()
        //{
        //    var b1 = new Board(new List<string>(8)
        //    {
        //        "x-------",
        //        "--------",
        //        "--------",
        //        "--------",
        //        "--------",
        //        "--------",
        //        "--------",
        //        "-------o",
        //    }, Player.X);

        //    var b2 = new Board(new List<string>(8)
        //    {
        //        "--------",
        //        "------o-",
        //        "--*****-",
        //        "--x-----",
        //        "------*-",
        //        "------*-",
        //        "--*---*-",
        //        "------*-",
        //    }, Player.X);

        //    Logger.Log(b1 + "\n");
        //    Logger.Log("Moves for x:");
        //    Logger.Log(string.Join("\n", b1.GetValidMoves().Select(x => x.ToString())) + "\n");

        //    Logger.Log(b2.ToString() + "\n");
        //    Logger.Log("Moves for x:");
        //    Logger.Log(string.Join("\n", b2.GetValidMoves().Select(x => x.ToString())) + "\n");

        //    Console.ReadKey();
        //}

        private static IEnumerable<BoardSpace> GetSurroundingSpaces(BoardSpace space)
        {
            if (space.Row > 0)
            {
                yield return new BoardSpace((byte)(space.Row - 1), space.Col);

                if (space.Col > 0)
                {
                    yield return new BoardSpace((byte)(space.Row - 1), (byte)(space.Col - 1));
                    yield return new BoardSpace(space.Row, (byte)(space.Col - 1));
                }
                if (space.Col < 7)
                {
                    yield return new BoardSpace((byte)(space.Row - 1), (byte)(space.Col + 1));
                    yield return new BoardSpace(space.Row, (byte)(space.Col + 1));
                }
            }
            if (space.Row < 7)
            {
                yield return new BoardSpace((byte)(space.Row + 1), space.Col);

                if (space.Col > 0)
                {
                    yield return new BoardSpace((byte)(space.Row + 1), (byte)(space.Col - 1));
                }
                if (space.Col < 7)
                {
                    yield return new BoardSpace((byte)(space.Row + 1), (byte)(space.Col + 1));
                }
            }
        }

        private static HashSet<BoardSpace> GetOpenArea(Board board, Player player)
        {
            var initialPosition = player == Player.X ? board.Xposition : board.Oposition;

            var toExamine = new Queue<BoardSpace>(new[] { initialPosition });
            var closed = new HashSet<BoardSpace> { initialPosition };
            var accessible = new HashSet<BoardSpace>();

            while (toExamine.Count > 0)
            {
                var space = toExamine.Dequeue();

                // enqueue empty spaces immediately next to this space
                foreach (var successor in GetSurroundingSpaces(space))
                {
                    // skip spaces we've seen
                    if (closed.Contains(successor))
                    {
                        continue;
                    }

                    // mark this sapce as seen
                    closed.Add(successor);

                    if (board[successor.Row, successor.Col] == BoardSpaceValue.Empty)
                    {
                        accessible.Add(successor);
                        toExamine.Enqueue(successor);
                    }
                }
            }

            return accessible;
        }

        private static void TestOpenArea()
        {
            var board1 = new Board("********-**-*--o--**-********--*----*-x------*--*****--*-------*", Player.X);
            var board2 = new Board("******-*-**----**-**-***---**x------*--***---*-----o**-*-------*", Player.X);

            TestBoard(board1);
            Console.ReadKey();
            TestBoard(board2);
            Console.ReadKey();
        }

        private static void TestBoard(Board b)
        {
            Console.WriteLine(b);
            var xOpen = GetOpenArea(b, Player.X);
            var oOpen = GetOpenArea(b, Player.O);
            foreach (var open in xOpen)
            {
                Console.WriteLine("X: " + open);
            }
            foreach (var open in oOpen)
            {
                Console.WriteLine("O: " + open);
            }
            Console.WriteLine("X total: " + xOpen.Count);
            Console.WriteLine("O total: " + oOpen.Count);
        }

        public static void Main(string[] args)
        {
            try
            {
                GameRunner.KickoffNewGame();
            }
            catch (Exception e)
            {
                Console.WriteLine("********* FIERY DEATH! *********");
                Console.WriteLine(e.ToString());
            }
        }

        //private static void Test()
        //{
        //    var a = new List<string>(8)
        //        {
        //            "********",
        //            "-**-**o*",
        //            "****-**-",
        //            "--******",
        //            "*--****-",
        //            "x****---",
        //            "-*-*--*-",
        //            "-***---*",
        //        };

        //    var board = new Board(a, Player.O);
        //    var moves3 = board.GetValidMoves();
        //    var nextMove = Searcher.I.GetMyNextMove(board);
        //    var x = 4;
        //}
    }
}
