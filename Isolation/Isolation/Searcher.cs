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
            const int depthLimit = 4;

            _timer.StartTimer();

            var bestMove = _alphaBeta.BestMove(board, depthLimit, int.MinValue, int.MaxValue, new NumberOfMovesHeuristic());

            //foreach (var move in board.GetValidMoves())
            //{
            //    var tmp = board.Copy();
            //    tmp.Move(move);

            //    var score = _evaluator.Evaluate(tmp, new NumberOfMovesHeuristic());
            //    Logger.Log(tmp.ToString(), false);
            //    Logger.Log("SCORE: " + score + "\n", false);
            //}

            Logger.Log("Time remaining: " + _timer.GetTimeRemaining().TotalMilliseconds + " ms");

            return bestMove.Move;
        }
    }

    public static class Logger
    {
        private const string FilePath = @"C:\Users\AldenQuimby\Desktop\log.txt";

        public static void Log(string msg, bool consoleLog = true)
        {
            if (consoleLog)
            {
                Console.WriteLine(msg);
            }
            return;
            using (var file = new StreamWriter(FilePath, true))
            {
                file.WriteLine(DateTime.Now.ToString("HH:mm") + " " + msg);
            }
        }
    }
}