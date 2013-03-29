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
        private readonly HeuristicCache _evaluator;

        public Searcher()
        {
            _timer = MoveTimer.I;
            _evaluator = HeuristicCache.I;

            _alphaBeta = new AlphaBeta(_evaluator, _timer);
        }

        public BoardSpace GetMyNextMove(Board board)
        {
            _timer.StartTimer();

            var bestMove = _alphaBeta.BestMove(board.Copy(), 5, int.MinValue, int.MaxValue, new NumberOfMovesHeuristic());
            
            Console.WriteLine("Should I print rough heuristics? (Y/N)");
            if ("Y".Equals(Console.ReadLine(), StringComparison.OrdinalIgnoreCase))
            {
                foreach (var move in board.GetValidMoves())
                {
                    var tmp = board.Copy();
                    tmp.Move(move);

                    var score = _evaluator.Evaluate(tmp, new NumberOfMovesHeuristic());
                    Console.WriteLine(tmp);
                    Console.WriteLine("SCORE: " + score + "\n");
                }
            }

            return bestMove.Move;
        }
    }
}