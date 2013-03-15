using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Isolation
{
    public class Minimax
    {
        public const int WinGame = int.MaxValue;
        public const int LoseGame = int.MinValue;

        private readonly HeuristicCache _evaluator;
        private readonly MoveGenerator _generator;

        public Minimax()
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

        public BestMoveResult BestMove(Board board, int depth, HeuristicBase heuristic)
        {
            if (depth == 0)
            {
                return new BestMoveResult(_evaluator.Evaluate(board, heuristic), null);
            }

            var moveList = _generator.GetMovesForX(board);

            var bestScore = int.MinValue;
            BoardSpace bestMove = null;

            foreach (var move in moveList)
            {
                var boardCopy = new Board(board);
                boardCopy.MoveX(move);

                var tryMoveResult = BestMove(boardCopy, depth - 1, heuristic);

                var tryScore = -tryMoveResult.Score;

                if (tryScore > bestScore)
                {
                    bestScore = tryScore;
                    bestMove = tryMoveResult.Move;
                }
            }

            return new BestMoveResult(bestScore, bestMove);
        }
    }
}
