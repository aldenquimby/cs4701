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
            var b1 = new Board(new List<string>(8)
            {
                "x-------",
                "--------",
                "--------",
                "--------",
                "--------",
                "--------",
                "--------",
                "-------o",
            }, Player.X);

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
            }, Player.X);

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
            try
            {
                GameRunner.KickoffNewGame();
            }
            catch (Exception e)
            {
                Console.WriteLine("********* FIERY DEATH! *********");
                Console.WriteLine(e);
                Console.ReadKey();
            }
        }
    }
}
