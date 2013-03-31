using System;
using System.Diagnostics;
using System.IO;
using Isolation;

namespace Server
{
    public class Program
    {
        private static bool AskContinue()
        {
            Console.WriteLine("\nContinue game (Q to quit)?");
            var response = Console.ReadLine();
            return !"Q".Equals(response, StringComparison.OrdinalIgnoreCase);
        }

        private static void InitializeClient(StreamReader clientOut, StreamWriter clientIn, string player)
        {
            Logger.ServerLog(string.Format("Enter player {0} config: ", player));
            var config = Console.ReadLine();
            Logger.ServerLog(string.Format("Player {0} config: {1}", player, config));

            clientOut.ReadLine();
            clientIn.WriteLine(player);
            clientOut.ReadLine();
            clientIn.WriteLine(config);
        }

        private static void PrintBoard(Board board)
        {
            Logger.Log("\n" +
                       "*****************\n" +
                       board + "\n" +
                       "*****************");
        }

        private static BoardSpace GetMoveFromClient(StreamReader clientOut, string player)
        {
            Logger.ServerLog("Getting next move from client...");
            
            var move = clientOut.ReadLine();
            while (!"My move:".Equals(move))
            {
                if (move != null && (move.StartsWith("I win") || move.StartsWith("I lose")))
                {
                    Logger.ServerLog(string.Format("Game over! Player {0} says: {1}", player, move));
                    return null;
                }

                Logger.ClientLog(player, move);
                move = clientOut.ReadLine();
            }

            move = clientOut.ReadLine();

            Logger.ServerLog(player + " moves " + move);
            
            return new BoardSpace(move);
        }

        private static void SendMoveToClient(StreamReader clientOut, StreamWriter clientIn, BoardSpace move, string player)
        {
            var wait = clientOut.ReadLine();
            while (!"Enter opponent move (row col):".Equals(wait))
            {
                Logger.ClientLog(player, wait);
                wait = clientOut.ReadLine();
            }

            clientIn.WriteLine(move.ToString());
        }

        private static void PlayGame(Process client1, Process client2)
        {
            Logger.ServerLog("Starting game.");

            var board = Board.ConstructInitialBoard(Player.X);

            using (var xIn = client1.StandardInput)
            using (var xOut = client1.StandardOutput)
            using (var oIn = client2.StandardInput)
            using (var oOut = client2.StandardOutput)
            {
                InitializeClient(xOut, xIn, "X");
                InitializeClient(oOut, oIn, "O");

                PrintBoard(board);

                while (true)
                {
                    if (!AskContinue()) { break; } // user quit

                    var xMove = GetMoveFromClient(xOut, "X");
                    if (xMove == null) { break; } // game over
                    SendMoveToClient(oOut, oIn, xMove, "X");
                    board.Move(xMove);

                    PrintBoard(board);

                    var oMove = GetMoveFromClient(oOut, "O");
                    if (oMove == null) { break; } // game over
                    SendMoveToClient(xOut, xIn, oMove, "O");
                    board.Move(oMove);

                    PrintBoard(board);
                }
            }
        }

        private static Process StartClient()
        {
            return Process.Start(new ProcessStartInfo
            {
                CreateNoWindow = true,
                FileName = @"C:\src\columbia\cs4701\Isolation\Isolation\bin\Debug\Isolation.exe",
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                UseShellExecute = false,
            });
        }

        public static void Main(string[] args)
        {
            var client1 = StartClient();
            var client2 = StartClient();

            try
            {
                PlayGame(client1, client2);
            }
            catch (Exception e)
            {
                Console.WriteLine(e);
                Console.ReadKey();
            }
            finally
            {
                client1.Kill();
                client2.Kill();
            }
        }
    }
}
