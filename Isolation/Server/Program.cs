using System;
using System.Diagnostics;
using System.IO;
using Isolation;

namespace Server
{
    public class Program
    {
        private static Process StartClient()
        {
            var p = new Process
                {
                    StartInfo = new ProcessStartInfo
                        {
                            CreateNoWindow = true,
                            FileName = @"C:\src\columbia\cs4701\Isolation\Isolation\bin\Debug\Isolation.exe",
                            RedirectStandardInput = true,
                            RedirectStandardOutput = true,
                            UseShellExecute = false,
                        },
                };
            p.Start();
            return p;
        }

        private static bool AskContinue()
        {
            Console.WriteLine("[SERVER] Continue game (Y/N)?");
            var response = Console.ReadLine();
            return !"N".Equals(response, StringComparison.OrdinalIgnoreCase);
        }

        private static void PrintBoard(Board board)
        {
            Console.WriteLine("*****************");
            Console.WriteLine(board.ToString());
            Console.WriteLine("*****************");
            Console.WriteLine();
        }

        private static BoardSpace GetMoveFromClient(StreamReader clientOut)
        {
            Console.WriteLine("[SERVER] getting next move from client...");
            
            var move = clientOut.ReadLine();
            while (!"My move:".Equals(move))
            {
                move = clientOut.ReadLine();
            }

            move = clientOut.ReadLine();
            
            return new BoardSpace(move);
        }

        //"I win :)." : "I lose :(."

        private static void SendMoveToClient(StreamReader clientOut, StreamWriter clientIn, BoardSpace move)
        {
            var wait = clientOut.ReadLine();
            while (!"Enter opponent move (row col):".Equals(wait))
            {
                wait = clientOut.ReadLine();
            }

            clientIn.WriteLine(move.ToString());
        }

        public static void Main(string[] args)
        {
            var board = Board.ConstructInitialBoard(Player.X);

            var client1 = StartClient();
            var client2 = StartClient();

            using (var xIn = client1.StandardInput)
            using (var xOut = client1.StandardOutput)
            using (var oIn = client2.StandardInput)
            using (var oOut = client2.StandardOutput)
            {
                // initialize client 1 to X
                Console.WriteLine("[SERVER] Client 1 init.");
                xOut.ReadLine();
                xIn.WriteLine("X");

                // initialize client 2 to O
                Console.WriteLine("[SERVER] Client 2 init.");
                oOut.ReadLine();
                oIn.WriteLine("O");

                while (true)
                {
                    if (!AskContinue())
                    {
                        break;
                    }

                    PrintBoard(board);

                    var xMove = GetMoveFromClient(xOut);
                    SendMoveToClient(oOut, oIn, xMove);

                    Console.WriteLine("[SERVER] X moves " + xMove);
                    board.Move(xMove);

                    if (!AskContinue())
                    {
                        break;
                    }

                    PrintBoard(board);

                    var oMove = GetMoveFromClient(oOut);
                    SendMoveToClient(xOut, xIn, oMove);

                    Console.WriteLine("[SERVER] O moves " + oMove);
                    board.Move(oMove);
                }
            }

            client1.Kill();
            client2.Kill();
        }
    }
}
