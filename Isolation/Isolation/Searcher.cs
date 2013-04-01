using System;

namespace Isolation
{
    public class Searcher
    {
        #region singleton

        private static readonly Lazy<Searcher> Singelton = new Lazy<Searcher>(() => new Searcher());
        public static Searcher I { get { return Singelton.Value; } }

        #endregion

        public void Initialize(SearchConfig config)
        {
            _config = config;
            _alphaBeta = new AlphaBeta(HeuristicCache.I, MoveTimer.I);
        }

        private AlphaBeta _alphaBeta;
        private SearchConfig _config;

        public BoardSpace GetMyNextMove(Board board)
        {
            _config.MaxQuiessenceNodes = 1000*_config.DepthLimit;

            var bestMove = _alphaBeta.BestMove(board, _config);

            if (_config.ReportStatistics)
            {
                Console.WriteLine(bestMove.ToString());
            }

            // if we have over half time remaining, increase the depth limit
            if (bestMove.PercentOfTimeRemaining > _config.PercentTimeLeftToIncrementDepthLimit)
            {
                _config.DepthLimit++;
            }

            // if we timed out, decrease depth limit
            if (bestMove.PercentOfTimeRemaining < _config.PercentTimeLeftForTimeout)
            {
                _config.DepthLimit--;
            }

            // if we're half way through the game, switch heuristics
            if (board.EmptySpacesRemaining == 39)
            {
                _config.Heuristic = new OpenAreaHeuristic();
            }

            return bestMove.Move;
        }
    }
}