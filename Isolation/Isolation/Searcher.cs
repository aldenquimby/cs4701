using System;

namespace Isolation
{
    public class Searcher
    {
        #region singleton

        private static readonly Lazy<Searcher> Singelton = new Lazy<Searcher>(() => new Searcher());
        public static Searcher I { get { return Singelton.Value; } }

        #endregion

        private readonly AlphaBeta _alphaBeta = new AlphaBeta(HeuristicCache.I, MoveTimer.I);

        public BoardSpace GetMyNextMove(Board board)
        {
            const int depthLimit = 5;

            var bestMove = _alphaBeta.BestMove(board, depthLimit, new NumberOfMovesHeuristic());
            Console.WriteLine("\nDESCENDING\n{0}Time remaining: {1} ms\n", bestMove, MoveTimer.I.GetTimeRemaining().TotalMilliseconds);

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