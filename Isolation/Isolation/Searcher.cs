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
            var bestMove = _alphaBeta.BestMove(board, _config);

            if (_config.ReportStatistics)
            {
                Console.WriteLine(bestMove.ToString());
            }

            var multiThreaded = new AlphaBetaFaster(HeuristicCache.I, MoveTimer.I).BestMoveNoStats(board, _config);
            Console.WriteLine("SHOULD BE FASTER");
            Console.WriteLine(multiThreaded.ToString());
            if (bestMove.Score != multiThreaded.Score)
            {
                Console.WriteLine("DEATH FAIL");
            }

            // if we have enough time remaining, increase the depth limit
            if (bestMove.PercentOfTimeRemaining > _config.PercentTimeLeftToIncrementDepthLimit)
            {
                _config.DepthLimit++;
            }
            
            // if we timed out, decrease depth limit
            if (bestMove.PercentOfTimeRemaining < 0.01)
            {
                _config.DepthLimit--;
            }

            // if we're half way through the game, switch heuristics
            if (board.GetEmptySpacesRemaining() - _config.DepthLimit == 25)
            {
                _config.Heuristic = new OpenAreaHeuristic();
            }

            return bestMove.Move;
        }
    }
}