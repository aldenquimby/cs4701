using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Isolation
{
    public class AlphaBeta
    {        
        public const int WinGame = int.MaxValue;
        public const int LoseGame = int.MinValue;

        private readonly HeuristicCache _evaluator;
        private readonly MoveGenerator _generator;

        public AlphaBeta()
        {
            _evaluator = HeuristicCache.I;
            _generator = MoveGenerator.I;
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

        /// <summary>
        /// INITIAL CALL NEEDS: odd depth, -inifinity alpha, infinity beta
        /// </summary>
        public BestMoveResult BestMove(Board board, int depth, int alpha, int beta, HeuristicBase heuristic)
        {
            if (depth == 0)
            {
                return new BestMoveResult(_evaluator.Evaluate(board, heuristic), null);
            }

            var moveList = _generator.GetMovesForX(board);

            var bestScore = alpha;
            BoardSpace bestMove = null;

            foreach (var move in moveList)
            {
                var boardCopy = new Board(board);
                boardCopy.MoveX(move);

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
}
