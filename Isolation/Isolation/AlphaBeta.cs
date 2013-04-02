using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;

namespace Isolation
{
    public class AlphaBeta
    {
        private readonly HeuristicCache _evaluator;
        private readonly MoveTimer _timer;

        public AlphaBeta(HeuristicCache evaluator, MoveTimer timer)
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

            var result = BestMoveInternal(board, _config.DepthLimit, int.MinValue, int.MaxValue, cancelToken);
            
            // fill stats
            result.Config = _config;
            result.NumNodesAtDepthLimit = _numNodesAtDepthLimit;
            result.NodesGeneratedByDepth = _nodesGeneratedByDepth;
            result.NumNodesQuiessenceSearched = _numNodesQuiessenceSearched;
            result.NodesTimedOutByDepth = _nodesTimedOutByDepth;

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
            if (_numNodesQuiessenceSearched > 1000*_config.DepthLimit)
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

            var interestingScoreChange = _config.DepthLimit + 2;

            // must have large enough percent change
            return percent1 > interestingScoreChange || percent1 < -interestingScoreChange ||
                   percent2 > interestingScoreChange || percent2 < -interestingScoreChange;
        }

        // INITIAL CALL NEEDS -inifinity alpha, infinity beta
        private BestMoveResult BestMoveInternal(Board board, int depth, int alpha, int beta, CancellationToken cancelToken)
        {
            // if we reached the bottom, return
            if (depth == 0)
            {
                _numNodesAtDepthLimit++;
                return new BestMoveResult(_evaluator.Evaluate(board, _config.Heuristic), null);
            }

            var isMaxTurn = board.MyPlayer == board.PlayerToMove;

            var validMoves = board.GetValidMoves();

            // if we hit game over before the depth limit, return infinity/-infinity if it's our/their turn
            if (!validMoves.Any())
            {
                return new BestMoveResult(isMaxTurn ? int.MinValue : int.MaxValue, null);
            }

            BoardSpace bestMove = null;

            // generate new boards for each move
            var validMovesWithBoard = validMoves.Select(x => new {move = x, newBoard = board.Copy().Move(x)});

            // sort move list only if we're not near the bottom of the tree, because it's expensive
            if (isMaxTurn)
            {
                validMovesWithBoard = validMovesWithBoard.OrderByDescending(x => _evaluator.Evaluate(x.newBoard, _config.Heuristic));
            }
            else
            {
                validMovesWithBoard = validMovesWithBoard.OrderBy(x => _evaluator.Evaluate(x.newBoard, _config.Heuristic));
            }

            foreach (var move in validMovesWithBoard)
            {
                _nodesGeneratedByDepth[depth]++;

                BestMoveResult childResult;

                // check quiessence search
                if (IsInterestingMove(board, move.newBoard))
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

            return new BestMoveResult(isMaxTurn ? alpha : beta, bestMove);
        }
    }

    public class BestMoveResult
    {
        public BestMoveResult(int score, BoardSpace bestMove)
        {
            Move = bestMove;
            Score = score;
            NodesGeneratedByDepth = new Dictionary<int, int>();
            NodesTimedOutByDepth = new Dictionary<int, int>();
        }

        public BoardSpace Move { get; set; }
        public int Score { get; set; }
        
        public SearchConfig Config { get; set; }
        public IDictionary<int, int> NodesGeneratedByDepth { get; set; }
        public IDictionary<int, int> NodesTimedOutByDepth { get; set; }
        public int NumNodesAtDepthLimit { get; set; }
        public int NumNodesQuiessenceSearched { get; set; }
        public double TotalSecondsElapsed { get; set; }
        public double PercentOfTimeRemaining { get; set; }

        public override string ToString()
        {
            Func<IDictionary<int, int>, string> printByDepth =
                byDepth =>
                    {
                        var total = byDepth.Sum(x => x.Value);
                        if (total == 0)
                        {
                            return "0";
                        }
                        return total + " => " + string.Join(", ", byDepth.OrderByDescending(x => x.Key).Select(x => x.Key + "-" + x.Value));
                    };

            var builder = new StringBuilder();
            builder.AppendLine("Move: " + Move);
            builder.AppendLine("Score: " + Score);
            builder.AppendLine("Heuristic: " + Config.Heuristic.Name);
            builder.AppendLine("Depth Limit: " + Config.DepthLimit);
            builder.AppendLine("Time Taken (s): " + TotalSecondsElapsed);
            builder.AppendLine("Time Left (%): " + PercentOfTimeRemaining*100);
            builder.AppendLine("Node Generation: " + printByDepth(NodesGeneratedByDepth));
            builder.AppendLine("Node Timeouts: " + printByDepth(NodesTimedOutByDepth));
            builder.AppendLine("Depth Limit Nodes: " + NumNodesAtDepthLimit);
            builder.AppendLine("Quiessence Nodes: " + NumNodesQuiessenceSearched);
            return builder.ToString();
        }
    }
}
