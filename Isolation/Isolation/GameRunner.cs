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
                Logger.Log("\n" + board); // TODO remove

                // player X moves first
                if (board.MyPlayer == Player.X)
                {
                    MyMove(board);
                }
                else
                {
                    OpponentMove(board);
                }

                Logger.Log("\n" + board); // TODO remove

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
            Logger.Log(board.Winner == board.MyPlayer ? "I win :)." : "I lose :(.");
            Console.ReadKey();
        }

        // ask user which player I am
        private static Player GetPlayer()
        {
            Logger.Log("Am I player 'X' or player 'O'?");

            var input = Console.ReadLine();

            while (!"O".Equals(input, StringComparison.OrdinalIgnoreCase) && !"X".Equals(input, StringComparison.OrdinalIgnoreCase))
            {
                Logger.Log("Please enter 'X' or 'O'.");
                input = Console.ReadLine();
            }

            return (Player)Enum.Parse(typeof(Player), input.ToUpper());
        }

        // perform my next move on the board
        private static void MyMove(Board board)
        {
            var myMove = Searcher.I.GetMyNextMove(board);

            if (myMove == null)
            {
                Logger.Log("I cannot move!");
                board.Forfeit();
                return;
            }

            board.Move(myMove);
            Logger.Log("My move:");
            Logger.Log(myMove.ToString());
        }

        // perform opponents next move on the board
        private static void OpponentMove(Board board)
        {
            // if opponent can't move, i win!
            if (board.GetOpponentValidMoves().Count == 0)
            {
                Logger.Log("Opponent cannot move.");
                board.ForfeitOpponent();
                return;
            }

            // ask for move
            var move = GetOpponentMove();

            // ensure it is valid
            var isValid = board.IsValidMove(move);
            while (!isValid)
            {
                Logger.Log("This is not a valid opponent move!");
                Logger.Log("Was this entered correctly? (Y/N):");

                // if opponent enters invalid move, they forfeit
                if ("Y".Equals(Console.ReadLine(), StringComparison.OrdinalIgnoreCase))
                {
                    Logger.Log("Opponent forfeits from invalid move.");
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
            Logger.Log("Enter opponent move (row col):");

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
                    Logger.Log("Error reading move, use the format (row col):");
                }
            }

            return move;
        }
    }
}