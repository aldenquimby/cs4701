using System.Collections.Generic;
using System.Linq;
using System.Threading;

namespace Isolation
{
    public class AlphaBetaFaster
    {
        private readonly HeuristicCache _evaluator;
        private readonly MoveTimer _timer;

        public AlphaBetaFaster(HeuristicCache evaluator, MoveTimer timer)
        {
            _evaluator = evaluator;
            _timer = timer;
        }

        private SearchConfig _config;
        private Dictionary<int, int> _nodesGeneratedByDepth;
        private Dictionary<int, int> _nodesTimedOutByDepth;
        private int _numNodesAtDepthLimit;
        private int _numNodesQuiessenceSearched;

        public BestMoveResult BestMove(Board board, SearchConfig config, CancellationToken cancelToken)
        {
            // initialize stats
            _config = config;
            _nodesGeneratedByDepth = Enumerable.Range(1, _config.DepthLimit).ToDictionary(x => x, x => 0);
            _nodesTimedOutByDepth = Enumerable.Range(1, _config.DepthLimit).ToDictionary(x => x, x => 0);
            _numNodesAtDepthLimit = 0;
            _numNodesQuiessenceSearched = 0;

            var validMoves = board.GetValidMoves();

            var alpha = int.MinValue;
            var beta = int.MaxValue;

            BoardSpace bestMove = null;

            foreach (var move in validMoves.Select(x => new { move = x, newBoard = board.Copy().Move(x) }).OrderByDescending(x => _evaluator.Evaluate(x.newBoard, _config.Heuristic)))
            {
                _nodesGeneratedByDepth[_config.DepthLimit]++;

                var childResult = BestMoveInternal(move.newBoard, _config.DepthLimit - 1, alpha, beta, cancelToken);

                if (childResult > alpha)
                {
                    alpha = childResult;
                    bestMove = move.move;
                }

                // alpha-beta trim - if we win, stop looking
                if (alpha >= beta)
                {
                    break;
                }

                // if we're supposed to cancel, or at timeout, bail
                if (cancelToken.IsCancellationRequested || _timer.GetPercentOfTimeRemaining() < 0.01)
                {
                    _nodesTimedOutByDepth[_config.DepthLimit]++;
                    break;
                }
            }

            if (bestMove == null)
            {
                bestMove = validMoves.FirstOrDefault();
            }

            var result = new BestMoveResult(alpha, bestMove);

            // fill stats
            result.Config = _config;
            result.NumNodesAtDepthLimit = _numNodesAtDepthLimit;
            result.NodesGeneratedByDepth = _nodesGeneratedByDepth;
            result.NumNodesQuiessenceSearched = _numNodesQuiessenceSearched;
            result.NodesTimedOutByDepth = _nodesTimedOutByDepth;
            result.PercentOfTimeRemaining = _timer.GetPercentOfTimeRemaining();
            result.TotalSecondsElapsed = _timer.GetTimeElapsed().TotalSeconds;

            return result;
        }

        private bool IsInterestingMove(Board originalBoard, Board newBoard)
        {
            // if no quiessence search configured, nothing is interesting
            if (!_config.QuiessenceSearch)
            {
                return false;
            }

            // prevent infinite quiessence search with hard coded max generation (should never reach this though)
            if (_numNodesQuiessenceSearched > 1000 * _config.DepthLimit)
            {
                return false;
            }

            var originalScore = _evaluator.Evaluate(originalBoard, _config.Heuristic);
            var newScore = _evaluator.Evaluate(newBoard, _config.Heuristic);

            // must go from negative to positive or positive to negative
            if (!((originalScore > 0 && newScore < 0) || (originalScore < 0 && newScore > 0)))
            {
                return false;
            }

            var percent1 = ((double)(newScore - originalScore) / newScore);
            var percent2 = ((double)(originalScore - newScore) / originalScore);

            var cutoff = _config.DepthLimit + 2;

            // must have large enough percent change
            return percent1 > cutoff || percent1 < -cutoff ||
                   percent2 > cutoff || percent2 < -cutoff;
        }

        private int BestMoveInternal(Board board, int depth, int alpha, int beta, CancellationToken cancelToken)
        {
            // if we reached the bottom, return
            if (depth == 0)
            {
                _numNodesAtDepthLimit++;
                return _evaluator.Evaluate(board, _config.Heuristic);
            }

            var isMaxTurn = board.MyPlayer == board.PlayerToMove;

            var validMoves = board.GetValidMoves();

            // if we hit game over before the depth limit, return infinity/-infinity if it's our/their turn
            if (!validMoves.Any())
            {
                return isMaxTurn ? int.MinValue : int.MaxValue;
            }

            // generate new boards for each move
            var validChildBoards = validMoves.Select(x => board.Copy().Move(x));

            // sort move list only if we're not near the bottom of the tree, because it's expensive
            if (isMaxTurn)
            {
                validChildBoards = validChildBoards.OrderByDescending(x => _evaluator.Evaluate(x, _config.Heuristic));
            }
            else
            {
                validChildBoards = validChildBoards.OrderBy(x => _evaluator.Evaluate(x, _config.Heuristic));
            }

            foreach (var childBoard in validChildBoards)
            {
                _nodesGeneratedByDepth[depth]++;

                int childResult;

                // check quiessence search
                if (IsInterestingMove(board, childBoard))
                {
                    // extend search depth because this move looks interesting
                    _numNodesQuiessenceSearched++;
                    childResult = BestMoveInternal(childBoard, depth, alpha, beta, cancelToken);
                }
                else
                {
                    // normal evaluation
                    childResult = BestMoveInternal(childBoard, depth - 1, alpha, beta, cancelToken);
                }

                if (isMaxTurn) // if it's a max turn, we want to check alpha
                {
                    if (childResult > alpha)
                    {
                        alpha = childResult;
                    }
                }
                else // else it's a min turn, so we want to check beta 
                {
                    if (childResult < beta)
                    {
                        beta = childResult;
                    }
                }

                // alpha-beta trim
                if (alpha >= beta)
                {
                    break;
                }

                // if we're supposed to cancel, or at timeout, bail
                if (cancelToken.IsCancellationRequested || _timer.GetPercentOfTimeRemaining() < 0.01)
                {
                    _nodesTimedOutByDepth[depth]++;
                    break;
                }
            }

            return isMaxTurn ? alpha : beta;
        }
    }
}
