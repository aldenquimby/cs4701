using System;

namespace Isolation
{
    public class Program
    {
        public static void Test()
        {
            var board = new Board("**---*******-****-**-****--*********-*x**---***-****o**-*****-**", Player.O);
            var move = new AlphaBeta(HeuristicCache.I, MoveTimer.I).BestMove(board, new SearchConfig(""));

            Console.WriteLine(board.ToString());
            Console.WriteLine("");
            Console.WriteLine(move.ToString());
            Console.ReadKey();
        }

        public static void Main(string[] args)
        {
            Test();
            return;

            try
            {
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
