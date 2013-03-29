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
        private int _numAlphaBetaCutoffs;
        private int _numNodesAtDepthLimit;

        public static bool ShouldAlphaBeta = true;
        public static bool? ShouldOrderMovesDesc = true;

        public BestMoveResult BestMove(Board board, int depthLimit, HeuristicBase heuristic)
        {
            _heuristic = heuristic;
            _numNodesGenerated = 0;
            _numAlphaBetaCutoffs = 0;
            _numNodesAtDepthLimit = 0;

            return BestMove(board, depthLimit, int.MinValue, int.MaxValue);
        }

        // INITIAL CALL NEEDS: even depth, -inifinity alpha, infinity beta
        private BestMoveResult BestMove(Board board, int depth, int alpha, int beta)
        {
            // if we reached the bottom, return
            // TODO quiessence search here
            if (depth == 0)
            {
                _numNodesAtDepthLimit++;
                return ReturnResult(_evaluator.Evaluate(board, _heuristic), null);
            }

            // if we have less than 10% of time left, return
            if (_timer.GetPercentOfTimeRemaining() < 0.1)
            {
                Logger.Log("!!Timeout!!"); //TODO remove
                // return ReturnResult(_evaluator.Evaluate(board, _heuristic), null);
            }

            BoardSpace bestMove = null;

            var validMoves = board.GetValidMoves().Select(x =>
                {
                    var boardCopy = board.Copy();
                    boardCopy.Move(x);
                    return new {move = x, board = boardCopy};
                });

            if (ShouldOrderMovesDesc == true)
            {
                validMoves = validMoves.OrderByDescending(x => _evaluator.Evaluate(x.board, _heuristic));
            }
            else if (ShouldOrderMovesDesc == false)
            {
                validMoves = validMoves.OrderBy(x => _evaluator.Evaluate(x.board, _heuristic));
            }

            foreach (var move in validMoves)
            {
                _numNodesGenerated++;
                
                var tryMoveResult = BestMove(move.board, depth - 1, -beta, -alpha);
                var tryScore = -tryMoveResult.Score;

                //Logger.Log(string.Format("At depth {0}: Alpha {1} Beta {2} Score {3}", depth, alpha.ToString().PadRight(11), beta.ToString().PadRight(11), tryScore.ToString().PadRight(11)), false);

                // is this the best move so far?
                if (tryScore > alpha)
                {
                    alpha = tryScore;
                    bestMove = move.move;
                }

                if (ShouldAlphaBeta)
                {
                    // alpha-beta cuttoff here
                    if (alpha > beta)
                    {
                        _numAlphaBetaCutoffs++;
                        return ReturnResult(alpha, bestMove);
                    }                    
                }
            }

            return ReturnResult(alpha, bestMove);
        }

        private BestMoveResult ReturnResult(int bestScore, BoardSpace bestMove)
        {
            return new BestMoveResult
                {
                    Score = bestScore,
                    Move = bestMove,
                    NumAlphaBetaCutoffs = _numAlphaBetaCutoffs,
                    NumNodesAtDepthLimit = _numNodesAtDepthLimit,
                    NumNodesGenerated = _numNodesGenerated,
                    HeurisiticName = _heuristic.Name,
                };
        }
    }

    public class BestMoveResult
    {
        public int Score { get; set; }
        public BoardSpace Move { get; set; }
        public int NumNodesGenerated { get; set; }
        public int NumAlphaBetaCutoffs { get; set; }
        public int NumNodesAtDepthLimit { get; set; }
        public string HeurisiticName { get; set; }

        public override string ToString()
        {
            var builder = new StringBuilder();
            builder.AppendLine("Move: " + Move);
            builder.AppendLine("Score: " + Score);
            builder.AppendLine("Heuristic: " + HeurisiticName);
            builder.AppendLine("Nodes Generated: " + NumNodesGenerated);
            builder.AppendLine("Alpha/Beta Trims: " + NumAlphaBetaCutoffs);
            builder.AppendLine("Depth Limit Nodes: " + NumNodesAtDepthLimit);
            return builder.ToString();
        }
    }
}
