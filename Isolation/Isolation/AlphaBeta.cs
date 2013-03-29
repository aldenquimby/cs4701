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

        private HeuristicBase _heuristic;
        private int _numNodesGenerated;
        private int _numNodesAtDepthLimit;

        public static bool ShouldAlphaBeta = true;
        public static bool? ShouldOrderMovesDesc = true;

        public BestMoveResult BestMove(Board board, int depthLimit, HeuristicBase heuristic)
        {
            // initialize stats
            _heuristic = heuristic;
            _numNodesGenerated = 0;
            _numNodesAtDepthLimit = 0;

            var result = BestMove(board, depthLimit, int.MinValue, int.MaxValue);
            
            // fill stats
            result.HeurisiticName = _heuristic.Name;
            result.NumNodesAtDepthLimit = _numNodesAtDepthLimit;
            result.NumNodesGenerated = _numNodesGenerated;
            
            return result;
        }

        private static int GetNumNodesTrimmed(Dictionary<int, int> cuttoffsByDepth, int branchingFactor)
        {
            var result = 0;
            foreach (var kvp in cuttoffsByDepth)
            {
                var depthCutoff = kvp.Key - 1;
                var numCuttoffs = kvp.Value;
                var nodesTrimmedPerCutoff = (int)Math.Pow(branchingFactor, depthCutoff);
                result += nodesTrimmedPerCutoff * numCuttoffs;
            }
            return result;
        }

        // INITIAL CALL NEEDS: even depth, -inifinity alpha, infinity beta
        private BestMoveResult BestMove(Board board, int depth, int alpha, int beta)
        {
            // if we reached the bottom, return
            // TODO quiessence search here
            if (depth == 0)
            {
                _numNodesAtDepthLimit++;
                return new BestMoveResult(_evaluator.Evaluate(board, _heuristic), null);
            }

            // if we have less than 10% of time left, return
            if (_timer.GetPercentOfTimeRemaining() < 0.1)
            {
                // Logger.Log("!!Timeout!!"); //TODO remove
                // return new BestMoveResult(_evaluator.Evaluate(board, _heuristic), null);
            }

            BoardSpace bestMove = null;

            var validMoves = board.GetValidMoves();
            var validMovesWithBoard = validMoves.Select(x =>
                {
                    var boardCopy = board.Copy();
                    boardCopy.Move(x);
                    return new {move = x, board = boardCopy};
                });

            if (ShouldOrderMovesDesc == true)
            {
                validMovesWithBoard = validMovesWithBoard.OrderByDescending(x => _evaluator.Evaluate(x.board, _heuristic));
            }
            else if (ShouldOrderMovesDesc == false)
            {
                validMovesWithBoard = validMovesWithBoard.OrderBy(x => _evaluator.Evaluate(x.board, _heuristic));
            }

            var isMaxTurn = board.MyPlayer == board.PlayerToMove;

            foreach (var move in validMovesWithBoard)
            {
                _numNodesGenerated++;

                var childResult = BestMove(move.board, depth - 1, alpha, beta);

                if (isMaxTurn) // if it's a max turn, we want to check alpha
                {
                    if (childResult.Score > alpha)
                    {
                        alpha = childResult.Score;
                        bestMove = move.move;
                    }

                    if (ShouldAlphaBeta && alpha >= beta)
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

                    if (ShouldAlphaBeta && alpha >= beta)
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
            Score = score;
            Move = bestMove;
        }

        public int Score { get; set; }
        public BoardSpace Move { get; set; }
        public int NumNodesGenerated { get; set; }
        public int NumNodesAtDepthLimit { get; set; }
        public string HeurisiticName { get; set; }

        public override string ToString()
        {
            var builder = new StringBuilder();
            builder.AppendLine("Move: " + Move);
            builder.AppendLine("Score: " + Score);
            builder.AppendLine("Heuristic: " + HeurisiticName);
            builder.AppendLine("Nodes Generated: " + NumNodesGenerated);
            builder.AppendLine("Depth Limit Nodes: " + NumNodesAtDepthLimit);
            return builder.ToString();
        }
    }
}
