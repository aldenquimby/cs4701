using System.Threading;

namespace Isolation
{
    public class LongestPath : IBestMoveGetter
    {
        public IBestMoveResult BestMove(Board board, SearchConfig config, MoveTimer timer, CancellationToken cancelToken)
        {
            // should ONLY be used in end game mode
            if (config.GameMode != GameMode.End)
            {
                // falls back to alpha beta if someone tries to use this in another mode
                return new AlphaBetaWithStats().BestMove(board, config, timer, cancelToken);
            }

            var longestPath = LongestPathHeuristic.NextMoveOnLongestPath(board, board.PlayerToMove);
            return new BestMoveResult(longestPath.Item1, longestPath.Item2);
        }
    }
}