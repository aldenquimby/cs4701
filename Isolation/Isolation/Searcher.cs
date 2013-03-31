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
            var bestMove = _alphaBeta.BestMove(board, _config, new NumberOfMovesHeuristic());

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

            //AlphaBeta.ShouldOrderMovesDesc = false;
            //var sortedBackwards = _alphaBeta.BestMove(board, depthLimit, new NumberOfMovesHeuristic());
            //Logger.Log(string.Format("\nASCENDING\n{0}Time remaining: {1} ms\n", sortedBackwards, MoveTimer.I.GetTimeRemaining().TotalMilliseconds));

            //AlphaBeta.ShouldOrderMovesDesc = null;
            //var unsorted = _alphaBeta.BestMove(board, depthLimit, new NumberOfMovesHeuristic());
            //Logger.Log(string.Format("\nNO ORDER\n{0}Time remaining: {1} ms\n", unsorted, MoveTimer.I.GetTimeRemaining().TotalMilliseconds));

            //AlphaBeta.ShouldAlphaBeta = false;
            //var minimax = _alphaBeta.BestMove(board, depthLimit, new NumberOfMovesHeuristic());
            //Logger.Log(string.Format("\nMINIMAX\n{0}Time remaining: {1} ms\n", minimax, MoveTimer.I.GetTimeRemaining().TotalMilliseconds));

            return bestMove.Move;
        }
    }
}