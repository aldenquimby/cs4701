using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Isolation
{
    public class AlphaBeta
    {
        private readonly HeuristicCache _evaluator;
        private readonly MoveGenerator _generator;
        private readonly MoveTimer _timer;

        public AlphaBeta(HeuristicCache evaluator, MoveGenerator generator, MoveTimer timer)
        {
            _evaluator = evaluator;
            _generator = generator;
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

            // if we're out of time, return
            if (_timer.GetTimeRemaining() < TimeSpan.FromSeconds(1))
            {
                return new BestMoveResult(_evaluator.Evaluate(board, heuristic), null);
            }

            var moveList = _generator.GetMovesForX(board);

            var bestScore = alpha;
            BoardSpace bestMove = null;

            foreach (var move in moveList)
            {
                var boardCopy = board.Copy();
                boardCopy.MoveMe(move);

                var tryMoveResult = BestMove(boardCopy, depth - 1, -beta, -alpha, heuristic);

                var tryScore = -tryMoveResult.Score;

                // is this the best move so far?
                if (tryScore > bestScore)
                {
                    bestScore = tryScore;
                    bestMove = tryMoveResult.Move;
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
