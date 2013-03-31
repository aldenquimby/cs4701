using System;
using System.Collections.Generic;
using System.Linq;

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
                Console.ReadKey();
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
