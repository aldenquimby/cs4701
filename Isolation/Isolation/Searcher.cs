using System;
using System.IO;

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
        private readonly HeuristicCache _evaluator;

        public Searcher()
        {
            _timer = MoveTimer.I;
            _evaluator = HeuristicCache.I;

            _alphaBeta = new AlphaBeta(_evaluator, _timer);
        }

        public BoardSpace GetMyNextMove(Board board)
        {
            const int depthLimit = 3;

            _timer.StartTimer();
            var bestMove = _alphaBeta.BestMove(board, depthLimit, new NumberOfMovesHeuristic());
            Logger.Log(string.Format("\n\nDESCENDING\n{0}Time remaining: {1} ms\n", bestMove, _timer.GetTimeRemaining().TotalMilliseconds));

            //_timer.StartTimer();
            //AlphaBeta.ShouldOrderMovesDesc = false;
            //var sortedBackwards = _alphaBeta.BestMove(board, depthLimit, new NumberOfMovesHeuristic());
            //Logger.Log(string.Format("\n\nASCENDING\n{0}Time remaining: {1} ms\n", sortedBackwards, _timer.GetTimeRemaining().TotalMilliseconds));

            //_timer.StartTimer();
            //AlphaBeta.ShouldOrderMovesDesc = null;
            //var unsorted = _alphaBeta.BestMove(board, depthLimit, new NumberOfMovesHeuristic());
            //Logger.Log(string.Format("\n\nNO ORDER\n{0}Time remaining: {1} ms\n", unsorted, _timer.GetTimeRemaining().TotalMilliseconds));

            //_timer.StartTimer();
            //AlphaBeta.ShouldAlphaBeta = false;
            //var minimax = _alphaBeta.BestMove(board, depthLimit, new NumberOfMovesHeuristic());
            //Logger.Log(string.Format("\n\nMINIMAX\n{0}Time remaining: {1} ms\n", minimax, _timer.GetTimeRemaining().TotalMilliseconds));

            return bestMove.Move;
        }
    }

    public static class Logger
    {
        private static readonly string FilePath = string.Format(@"C:\Users\AldenQuimby\Desktop\Logs\log-{0}.txt", DateTime.Now.ToString("MMddHHmmssfff"));

        public static void Log(string msg, bool consoleLog = true)
        {
            if (consoleLog)
            {
                Console.WriteLine(msg);
            }
            using (var file = new StreamWriter(FilePath, true))
            {
                file.WriteLine(DateTime.Now.ToString("HH:mm") + " " + msg);
            }
        }
    }
}