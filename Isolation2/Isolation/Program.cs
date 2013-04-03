using System;
using System.Diagnostics;
using System.Linq;

namespace Isolation
{
    public class Program
    {
        //public static void Test()
        //{
        //    var board = new Board("**---*******-****-**-****--*********-*x**---***-****o**-*****-**", Player.O);
        //    var move = new AlphaBeta(HeuristicCache.I, MoveTimer.I).BestMove(board, new SearchConfig(""));

        //    Console.WriteLine(board.ToString());
        //    Console.WriteLine("");
        //    Console.WriteLine(move.ToString());
        //    Console.ReadKey();
        //}

        public static void TestMoveGen()
        {
            var board = Board.ConstructInitialBoard(Player.X);

            var sw = new Stopwatch();
            var count = 0;
            sw.Start();
            while (count < 1000000)
            {
                var children = board.GetValidMoves().Select(x => board.Copy().Move(x));
                count += children.Count();
            }
            sw.Stop();

            Console.WriteLine(sw.ElapsedMilliseconds);

            Console.ReadKey();
        }

        public static void Main(string[] args)
        {
            try
            {
                // jack up CPU usage
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
