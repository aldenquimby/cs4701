using System;
using System.Diagnostics;
using System.Linq;
using System.Threading;

namespace Isolation
{
    public class Program
    {
        static void TestEndGameTransition()
        {
            const string boardString = "* * - * - * - -" +
                                       "- - - - * - - -" +
                                       "- - * * * - * -" +
                                       "* * - - * * * -" +
                                       "- * * * * o - *" +
                                       "* * - * * * * *" +
                                       "* * - - - * * -" +
                                       "- * - - * - - x";

            var board = new Board(new string(boardString.Where(x => !char.IsWhiteSpace(x)).ToArray()), Player.X);

            var searcher = new Searcher();
            searcher.Initialize(new SearchConfig("60"){DepthLimit = 19, GameMode = GameMode.Middle});
            var searchResult = searcher.GetMyNextMove(board);
            Console.WriteLine("My move:");
            Console.WriteLine(searchResult);
        }

        static void Test()
        {
            var board = new Board("**---*******-****-**-****--*********-*x**---***-****o**-*****-**", Player.O);
            var move = new AlphaBetaWithStats().BestMove(board, new SearchConfig("60"), MoveTimer.I, new CancellationToken());

            Console.WriteLine(board.ToString());
            Console.WriteLine("");
            Console.WriteLine(move.ToString());
            Console.ReadKey();
        }

        static int TryGetBoard(int numMovesCompleteSoFar, out Board daBoard)
        {
            var rand = new Random();

            var moves = 0;

            var board = Board.ConstructInitialBoard(Player.X);

            for (var i = 0; i < numMovesCompleteSoFar; i++)
            {
                var move = board.GetValidMoves().OrderBy(x => rand.Next()).FirstOrDefault();
                if (move == null)
                {
                    break;
                }
                board.Move(move);
                moves++;
            }

            daBoard = board;
            return moves;
        }

        static Board GetBoard(int numMovesCompleteSoFar)
        {
            // try 1000 times to get a valid board
            Board bestB = null;
            var best = 0;
            for (var tries = 0; tries < 1000; tries++)
            {
                Board b;
                var temp = TryGetBoard(numMovesCompleteSoFar, out b);
                if (temp > best)
                {
                    best = temp;
                    bestB = b;
                }
                if (temp == numMovesCompleteSoFar)
                {
                    break;
                }
            }
            return bestB;
        }

        static void TestHeuristicEvaluation(HeuristicBase h, int numMovesCompleteSoFar)
        {
            var board = GetBoard(numMovesCompleteSoFar);

            var sw = new Stopwatch();
            sw.Start();
            
            var useless = 0;
            for (var i = 0; i < 1000000; i++)
            {
                useless += h.Evaluate(board);
            }

            sw.Stop();

            Console.WriteLine("Useless " + useless);

            Console.WriteLine(sw.ElapsedMilliseconds);

            Console.ReadKey();
        }

        static void TestMoveGen(int numMovesCompleteSoFar)
        {
            var board = GetBoard(numMovesCompleteSoFar);

            var sw = new Stopwatch();
            sw.Start();

            var count = 0;
            while (count < 1000000)
            {
                var children = board.GetValidMoves().Select(x => board.Copy().Move(x));
                count += children.Count();
            }

            sw.Stop();

            Console.WriteLine(sw.ElapsedMilliseconds);

            Console.ReadKey();
        }

        static void TestLongestPath(int numMovesCompleteSoFar)
        {
            var board = GetBoard(numMovesCompleteSoFar);
            Console.WriteLine(board.ToString());

            var xResult = LongestPathHeuristic.LongestPathLength(board, Player.X);
            var oResult = LongestPathHeuristic.LongestPathLength(board, Player.O);

            Console.WriteLine("X Path: " + xResult);
            Console.WriteLine("O Path: " + oResult);
            Console.WriteLine("Moves left: " + board.GetEmptySpacesRemaining());
        }

        public static void Main(string[] args)
        {
            try
            {
                // jack up CPU
                Process.GetCurrentProcess().PriorityBoostEnabled = true;
                Process.GetCurrentProcess().PriorityClass = ProcessPriorityClass.High;

                GameRunner.KickoffNewGame();
            }
            catch (Exception e)
            {
                Console.WriteLine("********* FIERY DEATH! *********");
                Console.WriteLine(e.ToString());
            }

            Console.ReadKey();
        }
    }
}
