﻿namespace Isolation
{
    public class AlphaBeta
    {
        private readonly HeuristicCache _evaluator;
        private readonly MoveTimer _timer;

        public AlphaBeta(HeuristicCache evaluator, MoveTimer timer)
        {
            _evaluator = evaluator;
            _timer = timer;
        }

        /// <summary>
        /// INITIAL CALL NEEDS: odd depth, -inifinity alpha, infinity beta
        /// </summary>
        public BestMoveResult BestMove(Board board, int depth, int alpha, int beta, HeuristicBase heuristic)
        {
            // if we reached the bottom, return
            if (depth == 0)
            {
                return new BestMoveResult(_evaluator.Evaluate(board, heuristic), null);
            }

            // if we have less than 10% of time left, return
            if (_timer.GetPercentOfTimeRemaining() < 0.1)
            {
                System.Console.WriteLine("!!Timeout!!"); //TODO remove
                return new BestMoveResult(_evaluator.Evaluate(board, heuristic), null);
            }

            var bestScore = alpha;
            BoardSpace bestMove = null;

            foreach (var move in board.GetValidMoves())
            {
                var boardCopy = board.Copy();
                boardCopy.Move(move);

                var tryMoveResult = BestMove(boardCopy, depth - 1, -beta, -bestScore, heuristic);

                var tryScore = -tryMoveResult.Score;

                // is this the best move so far?
                if (tryScore > bestScore)
                {
                    //System.Console.WriteLine("Found better move!");
                    //System.Console.WriteLine(board.ToString());
                    //System.Console.WriteLine("SCORE: " + tryScore);

                    bestScore = tryScore;
                    bestMove = move;
                }

                // alpha-beta cuttoff here
                if (bestScore > beta)
                {
                    return new BestMoveResult(bestScore, bestMove);
                }
            }

            return new BestMoveResult(bestScore, bestMove);
        }
    }

    public class BestMoveResult
    {
        public BestMoveResult(int score, BoardSpace move)
        {
            Score = score;
            Move = move;
        }

        public int Score { get; private set; }
        public BoardSpace Move { get; private set; }
    }
}
