using System;

namespace Isolation
{
    public class Searcher
    {
        #region singleton

        private static readonly Lazy<Searcher> Singelton = new Lazy<Searcher>(() => new Searcher());
        public static Searcher I { get { return Singelton.Value; } }

        #endregion

        private readonly AlphaBeta _alphaBeta;
        private readonly MoveTimer _timer;
        private readonly MoveGenerator _generator;
        private readonly HeuristicCache _evaluator;

        public Searcher()
        {
            _timer = MoveTimer.I;
            _generator = MoveGenerator.I;
            _evaluator = HeuristicCache.I;

            _alphaBeta = new AlphaBeta(_evaluator, _generator, _timer);
        }

        public BoardSpace GetMyNextMove(Board board)
        {
            _timer.StartTimer();

            var bestMove = _alphaBeta.BestMove(board, 3, int.MinValue, int.MaxValue, new NumberOfMovesHeuristic(_generator));
            
            return bestMove.Move;
        }
    }
}