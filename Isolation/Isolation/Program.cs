using System;
using System.Diagnostics;

namespace Isolation
{
    public class Program
    {
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
