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

            // set up my move searcher with configuration
            Searcher.I.Initialize(config);

            // get the initial board
            var board = Board.ConstructInitialBoard(myPlayer);

            // load up heuristic cache from db
            if (config.LoadHeuristicCacheFromDb)
            {
                var heuristics = HeuristicSql.I.GetHeuristicCache(board.MyPlayer);
                HeuristicCache.I.LoadCache(heuristics);
            }

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

            // save heuristic cache to db
            if (config.SaveHeuristicCacheToDb)
            {
                var heuristics = HeuristicCache.I.DumpCache();
                HeuristicSql.I.SaveHeuristicCache(heuristics, board.MyPlayer);
            }
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
        private static SearchConfig GetConfig()
        {
            Console.WriteLine("Enter configuration: ");
            var input = Console.ReadLine();

            SearchConfig cfg = null;

            while (cfg == null)
            {
                try
                {
                    cfg = new SearchConfig(input);
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
            Console.WriteLine("Shall I go ahead with my move? 'Undo' to ");

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

        private static BoardSpace _lastOppoenentSpace;

        private static bool OpponentMoveAllowRollback(Board board)
        {
            if (!OpponentMove(board))
            {
                return false;
            }

            Console.WriteLine("Continue? Type 'undo' to rollback the last opponent move.");
            
            if ("undo".Equals(Console.ReadLine(), StringComparison.OrdinalIgnoreCase))
            {
                board.RollbackMove(_lastOppoenentSpace);
                return OpponentMoveAllowRollback(board);
            }
            
            return true;
        }

        // perform opponents next move on the board
        private static bool OpponentMove(Board board)
        {
            // if opponent can't move, i win!
            if (board.GetOpponentValidMoves().Count == 0)
            {
                Console.WriteLine("Opponent cannot move!");
                return false;
            }

            // ask for move
            var move = GetOpponentMove();

            if (move == null) //TODO: remove this hack for the server
            {
                Console.WriteLine("Opponenet quit.");
                return false;
            }

            // ensure it is valid
            if (!board.IsValidMove(move))
            {
                Console.WriteLine("Not a valid opponent move! Was it entered correctly? (y/n):");

                // if opponent enters invalid move, they forfeit
                if ("y".Equals(Console.ReadLine(), StringComparison.OrdinalIgnoreCase))
                {
                    Console.WriteLine("Opponent forfeits from invalid move.");
                    return false;
                }

                // otherwise it was a mistake, so let's try again
                return OpponentMove(board);
            }

            // confirm move is correct, which is equivalent to a 'rollback'
            // a rollback would simply mean asking this question after performing the move instead of before
            Console.WriteLine("Should I continue? 'undo' to rollback the last move, anything else to continue.");
            if ("undo".Equals(Console.ReadLine(), StringComparison.OrdinalIgnoreCase))
            {
                return OpponentMove(board);
            }

            //// ensure it is valid
            //var isValid = board.IsValidMove(move);
            //while (!isValid)
            //{
            //    Console.WriteLine("This is not a valid opponent move!");
            //    Console.WriteLine("Was this entered correctly? (Y/N):");

            //    // if opponent enters invalid move, they forfeit
            //    if ("Y".Equals(Console.ReadLine(), StringComparison.OrdinalIgnoreCase))
            //    {
            //        Console.WriteLine("Opponent forfeits from invalid move.");
            //        return false;
            //    }
                
            //    move = GetOpponentMove();
            //    isValid = board.IsValidMove(move);
            //}

            // _lastOppoenentSpace = board.MyPlayer == Player.X ? board.Oposition : board.Xposition;

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

                    if ("kill client".Equals(input)) //TODO: remove this server hack
                    {
                        return null;
                    }

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