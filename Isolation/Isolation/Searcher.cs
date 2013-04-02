using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

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
        }

        private SearchConfig _config;

        public BoardSpace GetMyNextMove(Board board)
        {
            // start the timer
            MoveTimer.I.StartTimer();

            // cancel all tasks that are now irrelevant
            foreach (var task in _taskByOpponentMove.Where(x => !x.Key.Equals(board.LastMove)))
            {
                task.Value.Item2.Cancel();
            }
            HeuristicCache.I.DumpKeyCount();
            BestMoveResult bestMoveResult;

            if (board.LastMove == null || !_taskByOpponentMove.ContainsKey(board.LastMove))
            {
                // we were not precomputing this path, so we need to start fresh
                bestMoveResult = new AlphaBeta(HeuristicCache.I, MoveTimer.I).BestMove(board, _config, new CancellationToken());
            }
            else
            {
                // we already started down this branch, so use the result from the task
                bestMoveResult = _taskByOpponentMove[board.LastMove].Item1.Result;
            }

            // report on time stats and reset timer
            bestMoveResult.PercentOfTimeRemaining = MoveTimer.I.GetPercentOfTimeRemaining();
            bestMoveResult.TotalSecondsElapsed = MoveTimer.I.GetTimeElapsed().TotalSeconds;
            var timedOut = MoveTimer.I.Timeout();
            MoveTimer.I.ResetTimer();

            if (_config.ReportStatistics)
            {
                Console.WriteLine(bestMoveResult.ToString());
            }

            // if we have enough time remaining, increase the depth limit
            if (bestMoveResult.PercentOfTimeRemaining > _config.PercentTimeLeftToIncrementDepthLimit)
            {
                _config.DepthLimit++;
            }
            
            // if we timed out, decrease depth limit
            if (timedOut)
            {
                _config.DepthLimit--;
            }

            // if we're half way through the game, switch heuristics
            if (board.GetEmptySpacesRemaining() - _config.DepthLimit == 25)
            {
                _config.Heuristic = new OpenAreaHeuristic();
            }

            return bestMoveResult.Move;
        }

        public void PreComputeNextMove(Board board)
        {
            // cancel any tasks that are running from last round
            foreach (var task in _taskByOpponentMove.Where(x => !x.Value.Item1.IsCanceled))
            {
                task.Value.Item2.Cancel();
            }

            _taskByOpponentMove.Clear();
            foreach (var move in board.GetValidMoves())
            {
                var ts = new CancellationTokenSource();
                var newBoard = board.Copy().Move(move);
                var task = Task.Factory.StartNew(() => new AlphaBeta(HeuristicCache.I, MoveTimer.I).BestMove(newBoard, _config, ts.Token), ts.Token);
                _taskByOpponentMove[move] = Tuple.Create(task, ts);
            }
        }

        private readonly Dictionary<BoardSpace, Tuple<Task<BestMoveResult>, CancellationTokenSource>> _taskByOpponentMove =
            new Dictionary<BoardSpace, Tuple<Task<BestMoveResult>, CancellationTokenSource>>();
    }
}