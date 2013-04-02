using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace Isolation
{
    public class AlphaBetaMultiThreaded
    {
        private readonly HeuristicCache _evaluator;
        private readonly MoveTimer _timer;

        public AlphaBetaMultiThreaded(HeuristicCache evaluator, MoveTimer timer)
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

            // one thread per initial move

            _alpha = int.MinValue;

            var movesWithBoards = board.GetValidMoves().Select(x => new {move = x, board = board.Copy().Move(x)});

            BoardSpace bestMove = null;

            var tasks = movesWithBoards.OrderByDescending(x => _evaluator.Evaluate(x.board, _config.Heuristic))
                               .Select(x => Task.Factory.StartNew(
                                       () =>
                                           {
                                               var child = BestMoveInternal(x.board, _config.DepthLimit - 1, _alpha,
                                                                            int.MaxValue);
                                               if (child > _alpha)
                                               {
                                                   lock (_alphaLock)
                                                   {
                                                       if (child > _alpha)
                                                       {
                                                           _alpha = child;
                                                           bestMove = x.move;
                                                       }
                                                   }
                                               }
                                           }));

            foreach (var task in tasks)
            {
                task.Wait();
            }

            //Parallel.ForEach(movesWithBoards, x =>
            //    {
            //        var childResult = BestMoveInternal(x.board, _config.DepthLimit - 1, _alpha, int.MaxValue);
            //        if (childResult > _alpha)
            //        {
            //            lock (_alphaLock)
            //            {
            //                if (childResult > _alpha)
            //                {
            //                    _alpha = childResult;
            //                    bestMove = x.move;
            //                }
            //            }
            //        }
            //    });

            //foreach (var moveWithBoard in movesWithBoards.OrderByDescending(x => _evaluator.Evaluate(x.board, _config.Heuristic)))
            //{
            //    var moveWithBoardClosure = moveWithBoard;
            //    var t = new Thread(() =>
            //        {
            //            var childResult = BestMoveInternal(moveWithBoardClosure.board, _config.DepthLimit - 1, _alpha, int.MaxValue);
            //            if (childResult > _alpha)
            //            {
            //                lock (_alphaLock)
            //                {
            //                    if (childResult > _alpha)
            //                    {
            //                        _alpha = childResult;
            //                        bestMove = moveWithBoardClosure.move;
            //                    }
            //                }
            //            }
            //        });
            //    t.Start();
            //    threads.Add(t);
            //}

            //// wait for all threads to finish
            //threads.ForEach(x => x.Join());

            var result = new BestMoveResult(_alpha, bestMove);

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

            var cutoff = _config.InterestingPercentScoreChange.Value;

            // must have large enough percent change
            return percent1 > cutoff || percent1 < -cutoff ||
                   percent2 > cutoff || percent2 < -cutoff;
        }

        private volatile int _alpha;
        private readonly object _alphaLock = new object();

        // INITIAL CALL NEEDS -inifinity alpha, infinity beta
        private int BestMoveInternal(Board board, int depth, int alpha, int beta)
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
            if (depth > 1)
            {
                validChildBoards = validChildBoards.OrderByDescending(x => _evaluator.Evaluate(x, _config.Heuristic));
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
                    childResult = BestMoveInternal(childBoard, depth, alpha, beta);
                }
                else
                {
                    // normal evaluation
                    childResult = BestMoveInternal(childBoard, depth - 1, alpha, beta);
                }

                // if we're near timeout, just bail :(
                //if (_timer.GetPercentOfTimeRemaining() < 0.01)
                //{
                //    _nodesTimedOutByDepth[depth]++;
                //    break;
                //}

                if (childResult == int.MaxValue)
                {
                    Console.WriteLine(board.ToString());
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
                if (alpha >= beta || _alpha >= beta)
                {
                    break;
                }
            }

            return isMaxTurn ? alpha : beta;
        }
    }
}
