using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

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

        public BestMoveResult BestMove(Board board, SearchConfig config)
        {
            // start timer
            _timer.StartTimer();

            // initialize stats
            _config = config;
            _nodesGeneratedByDepth = Enumerable.Range(1, _config.DepthLimit).ToDictionary(x => x, x => 0);
            _nodesTimedOutByDepth = Enumerable.Range(1, _config.DepthLimit).ToDictionary(x => x, x => 0);
            _numNodesAtDepthLimit = 0;
            _numNodesQuiessenceSearched = 0;

            var result = BestMoveInternal(board, _config.DepthLimit, int.MinValue, int.MaxValue);
            
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
            if (_config.InterestingPercentScoreChange == null)
            {
                return false;
            }

            // prevent indefinite quiessence search
            if (_numNodesQuiessenceSearched > _config.MaxQuiessenceNodes)
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

            var cutoff = _config.InterestingPercentScoreChange.Value;

            // must have large enough percent change
            return (percent1 > cutoff || percent1 < -cutoff) ||
                   (percent2 > cutoff || percent2 < -cutoff);
        }

        // INITIAL CALL NEEDS -inifinity alpha, infinity beta
        private BestMoveResult BestMoveInternal(Board board, int depth, int alpha, int beta)
        {
            // if we reached the bottom, return
            if (depth == 0)
            {
                _numNodesAtDepthLimit++;
                return new BestMoveResult(_evaluator.Evaluate(board, _config.Heuristic), null);
            }

            var validMoves = board.GetValidMoves();

            // try to initialize bestMove to the first possible move, so it will be returned if it's the only move
            var bestMove = validMoves.Count > 0 ? validMoves.First() : null;

            // generate new boards for each move
            var validMovesWithBoard = validMoves.Select(x =>
                {
                    var boardCopy = board.Copy();
                    boardCopy.Move(x);
                    return new {move = x, board = boardCopy};
                });

            // sort move list depending on configuration
            if (_config.SortMovesAsc == true)
            {
                validMovesWithBoard = validMovesWithBoard.OrderByDescending(x => _evaluator.Evaluate(x.board, _config.Heuristic));
            }
            else if (_config.SortMovesAsc == false)
            {
                validMovesWithBoard = validMovesWithBoard.OrderBy(x => _evaluator.Evaluate(x.board, _config.Heuristic));
            }

            var isMaxTurn = board.MyPlayer == board.PlayerToMove;

            // TODO: multithread

            foreach (var move in validMovesWithBoard)
            {
                _nodesGeneratedByDepth[depth]++;

                BestMoveResult childResult;

                // check quiessence search
                if (IsInterestingMove(board, move.board))
                {
                    // extend search depth because this move looks interesting
                    _numNodesQuiessenceSearched++;
                    childResult = BestMoveInternal(move.board, depth, alpha, beta);
                }
                else
                {
                    // normal evaluation
                    childResult = BestMoveInternal(move.board, depth - 1, alpha, beta);
                }

                // if we're near timeout, bail
                if (_timer.GetPercentOfTimeRemaining() < _config.PercentTimeLeftForTimeout)
                {
                    _nodesTimedOutByDepth[depth]++;
                    return new BestMoveResult(isMaxTurn ? alpha : beta, bestMove);
                }

                if (isMaxTurn) // if it's a max turn, we want to check alpha
                {
                    if (childResult.Score > alpha)
                    {
                        alpha = childResult.Score;
                        bestMove = move.move;
                    }

                    // alpha-beta trim
                    if (_config.UseAlphaBeta && alpha >= beta)
                    {
                        return new BestMoveResult(alpha, bestMove);
                    }
                }
                else // else it's a min turn, so we want to check beta 
                {
                    if (childResult.Score < beta)
                    {
                        beta = childResult.Score;
                        bestMove = move.move;
                    }

                    // alpha-beta trim
                    if (_config.UseAlphaBeta && alpha >= beta)
                    {
                        return new BestMoveResult(beta, bestMove);
                    }
                }
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
            builder.AppendLine("Move Sorting: " + (Config.SortMovesAsc == null ? "none" : Config.SortMovesAsc.Value ? "ASC" : "DSC"));
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
