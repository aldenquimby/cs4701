using System.Collections.Generic;
using System.Linq;
using System.Threading;

namespace Isolation
{
    public interface IBestMoveGetter
    {
        IBestMoveResult BestMove(Board board, SearchConfig config, MoveTimer timer, CancellationToken cancelToken);
    }

    public class AlphaBetaWithStats : IBestMoveGetter
    {
        private HeuristicCache _evaluator;
        private MoveTimer _timer;

        private SearchConfig _config;
        private Dictionary<int, int> _nodesGeneratedByDepth;
        private Dictionary<int, int> _nodesTimedOutByDepth;
        private int _numNodesAtDepthLimit;
        private int _numNodesQuiessenceSearched;

        public IBestMoveResult BestMove(Board board, SearchConfig config, MoveTimer timer, CancellationToken cancelToken)
        {
            // initialize stats
            _config = config;
            _timer = timer;
            _evaluator = HeuristicCache.I;
            _nodesGeneratedByDepth = Enumerable.Range(1, _config.DepthLimit).ToDictionary(x => x, x => 0);
            _nodesTimedOutByDepth = Enumerable.Range(1, _config.DepthLimit).ToDictionary(x => x, x => 0);
            _numNodesAtDepthLimit = 0;
            _numNodesQuiessenceSearched = 0;

            var result = BestMoveInternal(board, _config.DepthLimit, int.MinValue, int.MaxValue, cancelToken);
            
            // fill stats
            result.Config = _config;
            result.NumNodesAtDepthLimit = _numNodesAtDepthLimit;
            result.NodesGeneratedByDepth = _nodesGeneratedByDepth;
            result.NumNodesQuiessenceSearched = _numNodesQuiessenceSearched;
            result.NodesTimedOutByDepth = _nodesTimedOutByDepth;

            return result;
        }

        // recursive alpha beta
        private BestMoveResultWithStats BestMoveInternal(Board board, int depth, int alpha, int beta, CancellationToken cancelToken)
        {
            // if we reached the bottom, return
            if (depth == 0)
            {
                _numNodesAtDepthLimit++;
                return new BestMoveResultWithStats(_evaluator.Evaluate(board, _config.Heuristic), null);
            }

            var isMaxTurn = board.MyPlayer == board.PlayerToMove;

            var validMoves = board.GetValidMoves();

            // if we hit game over before the depth limit, return infinity/-infinity if it's our/their turn
            if (!validMoves.Any())
            {
                return new BestMoveResultWithStats(isMaxTurn ? int.MinValue : int.MaxValue, null);
            }

            BoardSpace bestMove = null;

            // generate new boards for each move and evaluate them so we can sort
            var validMovesWithBoard = validMoves.Select(x => new { move = x, newBoard = board.Copy().Move(x) })
                                                .Select(x => new { x.move, x.newBoard, score = _evaluator.Evaluate(x.newBoard, _config.Heuristic) });

            // if we're maxing, sort with largest first, otherwise sort with smallest first
            if (isMaxTurn)
            {
                validMovesWithBoard = validMovesWithBoard.OrderByDescending(x => x.score);
            }
            else
            {
                validMovesWithBoard = validMovesWithBoard.OrderBy(x => x.score);
            }

            // if we're doing a quiessence search, evaluate this board because we'll need to for all children
            var boardScore = _config.QuiessenceSearch ? _evaluator.Evaluate(board, _config.Heuristic) : new int?();

            foreach (var move in validMovesWithBoard)
            {
                _nodesGeneratedByDepth[depth]++;

                BestMoveResultWithStats childResult;

                // if we're doing a quiessence search, check to see if heuristic score change is interesting
                if (boardScore != null && IsInterestingMove(boardScore.Value, move.score))
                {
                    // extend search depth because this move looks interesting
                    _numNodesQuiessenceSearched++;
                    childResult = BestMoveInternal(move.newBoard, depth, alpha, beta, cancelToken);
                }
                else
                {
                    // normal evaluation
                    childResult = BestMoveInternal(move.newBoard, depth - 1, alpha, beta, cancelToken);
                }

                // if we're near timeout or asked to cancel, just bail :(
                if (_timer.Timeout() || cancelToken.IsCancellationRequested)
                {
                    _nodesTimedOutByDepth[depth]++;
                    break;
                }

                if (isMaxTurn) // if it's a max turn, we want to check alpha
                {
                    if (childResult.Score > alpha)
                    {
                        alpha = childResult.Score;
                        bestMove = move.move;
                    }
                }
                else // else it's a min turn, so we want to check beta 
                {
                    if (childResult.Score < beta)
                    {
                        beta = childResult.Score;
                        bestMove = move.move;
                    }
                }

                // alpha-beta trim
                if (alpha >= beta)
                {
                    break;
                }
            }

            // if we didn't find anything good, just return the first one
            if (bestMove == null)
            {
                bestMove = validMoves.First();
            }

            return new BestMoveResultWithStats(isMaxTurn ? alpha : beta, bestMove);
        }

        public bool IsInterestingMove(int originalScore, int newScore)
        {
            // if no quiessence search configured, nothing is interesting
            if (!_config.QuiessenceSearch)
            {
                return false;
            }

            // prevent infinite quiessence search with hard coded max generation (should never reach this though)
            if (_numNodesQuiessenceSearched > 2000 * _config.DepthLimit)
            {
                return false;
            }

            // must go from negative to positive or positive to negative
            if (!((originalScore > 0 && newScore < 0) || (originalScore < 0 && newScore > 0)))
            {
                return false;
            }

            var percent1 = ((double)(newScore - originalScore) / newScore);
            var percent2 = ((double)(originalScore - newScore) / originalScore);

            var interestingScoreChange = _config.DepthLimit + 1.4;

            // must have large enough percent change
            return percent1 > interestingScoreChange || percent1 < -interestingScoreChange ||
                   percent2 > interestingScoreChange || percent2 < -interestingScoreChange;
        }
    }
}
