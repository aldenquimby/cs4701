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

            // get the initial board
            var board = Board.ConstructInitialBoard(myPlayer);

            // play until there is a winner
            while (board.Winner == null)
            {
                Console.WriteLine("\n" + board); // TODO remove

                // player X moves first
                if (board.MyPlayer == Player.X)
                {
                    MyMove(board);
                }
                else
                {
                    OpponentMove(board);
                }

                Console.WriteLine("\n" + board); // TODO remove

                // if there is a winner, game over
                if (board.Winner != null)
                {
                    break;
                }

                // player O moves second
                if (board.MyPlayer == Player.X)
                {
                    OpponentMove(board);
                }
                else
                {
                    MyMove(board);
                }
            }

            // report win/loss
            Console.WriteLine(board.Winner == board.MyPlayer ? "I win :)." : "I lose :(.");
            Console.ReadKey();
        }

        // ask user which player I am
        private static Player GetPlayer()
        {
            Console.WriteLine("Are you player 'X' or player 'O'?");

            var input = Console.ReadLine();

            while (!"O".Equals(input, StringComparison.OrdinalIgnoreCase) && !"X".Equals(input, StringComparison.OrdinalIgnoreCase))
            {
                Console.WriteLine("Please enter 'X' or 'O'.");
                input = Console.ReadLine();
            }

            return (Player)Enum.Parse(typeof(Player), input);
        }

        // perform my next move on the board
        private static void MyMove(Board board)
        {
            var myMove = Searcher.I.GetMyNextMove(board);

            if (myMove == null)
            {
                Console.WriteLine("I cannot move!");
                board.Forfeit();
                return;
            }

            board.Move(myMove);
            Console.WriteLine("My move:");
            Console.WriteLine(myMove.ToString());
        }

        // perform opponents next move on the board
        private static void OpponentMove(Board board)
        {
            // ask for move
            var move = GetOpponentMove();

            // ensure it is valid
            var isValid = board.IsValidMove(move);
            while (!isValid)
            {
                Console.WriteLine("This is not a valid opponent move!");
                Console.WriteLine("Was this entered correctly? (Y/N):");

                if ("Y".Equals(Console.ReadLine(), StringComparison.OrdinalIgnoreCase))
                {
                    Console.WriteLine("Opponent forfeits from invalid move.");
                    board.ForfeitOpponent();
                    return;
                }
                
                move = GetOpponentMove();
                isValid = board.IsValidMove(move);
            }

            // now that we know it's valid, perform move
            board.Move(move);
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