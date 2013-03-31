using System;

namespace Isolation
{
    public class GameRunner
    {
        // play a new game
        public static void KickoffNewGame()
        {
            // figure out if I'm player X or O
            var myPlayer = GetPlayer();

            // get configuration (depth limit, quiesence search, etc)
            var config = GetConfig();

            // get the initial board
            var board = Board.ConstructInitialBoard(myPlayer);

            while (true)
            {
                // player X moves first
                if (board.MyPlayer == Player.X)
                {
                    if (!MyMove(board))
                    {
                        Console.WriteLine("I lose :).");
                        break;
                    }
                    if (!OpponentMove(board))
                    {
                        Console.WriteLine("I win :).");
                        break;
                    }
                }
                else
                {
                    if (!OpponentMove(board))
                    {
                        Console.WriteLine("I win :).");
                        break;
                    }
                    if (!MyMove(board))
                    {
                        Console.WriteLine("I lose :).");
                        break;
                    }
                }
            }
        
            Console.ReadKey();
        }

        // ask user which player I am
        private static Player GetPlayer()
        {
            Console.WriteLine("Am I player 'X' or player 'O'?");

            var input = Console.ReadLine();

            while (!"O".Equals(input, StringComparison.OrdinalIgnoreCase) && !"X".Equals(input, StringComparison.OrdinalIgnoreCase))
            {
                Console.WriteLine("Please enter 'X' or 'O'.");
                input = Console.ReadLine();
            }

            return (Player)Enum.Parse(typeof(Player), input.ToUpper());
        }

        // get alpha-beta configuration
        private static ClientConfig GetConfig()
        {
            Console.WriteLine("Enter configuration: ");
            var input = Console.ReadLine();

            ClientConfig cfg = null;

            while (cfg == null)
            {
                try
                {
                    cfg = new ClientConfig(input);
                }
                catch
                {
                    Console.WriteLine("Please enter a valid configuration: ");
                    input = Console.ReadLine();
                }
            }

            return cfg;
        }

        // perform my next move on the board
        private static bool MyMove(Board board)
        {
            var myMove = Searcher.I.GetMyNextMove(board);

            if (myMove == null)
            {
                Console.WriteLine("I cannot move!");
                return false;
            }

            board.Move(myMove);
            Console.WriteLine("My move:");
            Console.WriteLine(myMove.ToString());
            return true;
        }

        // perform opponents next move on the board
        private static bool OpponentMove(Board board)
        {
            // if opponent can't move, i win!
            if (board.GetOpponentValidMoves().Count == 0)
            {
                Console.WriteLine("Opponent cannot move.");
                return false;
            }

            // ask for move
            var move = GetOpponentMove();

            // ensure it is valid
            var isValid = board.IsValidMove(move);
            while (!isValid)
            {
                Console.WriteLine("This is not a valid opponent move!");
                Console.WriteLine("Was this entered correctly? (Y/N):");

                // if opponent enters invalid move, they forfeit
                if ("Y".Equals(Console.ReadLine(), StringComparison.OrdinalIgnoreCase))
                {
                    Console.WriteLine("Opponent forfeits from invalid move.");
                    return false;
                }
                
                move = GetOpponentMove();
                isValid = board.IsValidMove(move);
            }

            // now that we know it's valid, perform move
            board.Move(move);
            return true;
        }

        // ask user for next opponent move
        private static BoardSpace GetOpponentMove()
        {
            Console.WriteLine("Enter opponent move (row col):");

            BoardSpace move = null;

            while (move == null)
            {
                try
                {
                    var input = Console.ReadLine();
                    move = new BoardSpace(input);
                }
                catch
                {
                    Console.WriteLine("Error reading move, use the format (row col):");
                }
            }

            return move;
        }
    }
}