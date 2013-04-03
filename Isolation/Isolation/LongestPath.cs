using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;

namespace Isolation
{
    // should ONLY be used in end game mode
    // if we are walled off from the opponent, walk the longest possible path
    // otherwise do alpha/beta
    public class LongestPath : IBestMoveGetter
    {
        private readonly AlphaBetaWithStats _alphaBeta = new AlphaBetaWithStats();

        public IBestMoveResult BestMove(Board board, SearchConfig config, MoveTimer timer, CancellationToken cancelToken)
        {
            if (config.GameMode != GameMode.End)
            {
                // fall back to alpha beta if someone tries to use this in another mode
                return _alphaBeta.BestMove(board, config, timer, cancelToken);
            }

            // first find what kind of area each player is in
            var myArea = OpenAreaHeuristic.GetOpenArea(board, board.MyPlayer);
            var oppArea = OpenAreaHeuristic.GetOpenArea(board, board.OpponentPlayer);

            // if we are walled off, walk our longest path
            if (myArea.All(x => !oppArea.Contains(x)))
            {
                var longestPath = NextMoveOnLongestPath(board, board.PlayerToMove);
                return new BestMoveResult(longestPath.Item1, longestPath.Item2);
            }
            else // otherwise fall back to alpha beta
            {
                var ab = _alphaBeta.BestMove(board, config, timer, cancelToken);
                
                // but if alpha beta thinks we'll lose, do longest move
                if (ab.Score == int.MinValue)
                {
                    var longestPath = NextMoveOnLongestPath(board, board.PlayerToMove);
                    return new BestMoveResult(longestPath.Item1, longestPath.Item2);
                }
                else
                {
                    return ab;
                }
            }
        }

        // gets the next move and the path length on your longest possible path
        // assumes that other player does NOT move while you walk the path (which is sub-optimal)
        public static Tuple<int, BoardSpace> NextMoveOnLongestPath(Board board, Player player)
        {
            Tuple<int, List<Tuple<Board, BoardSpace>>> result;

            if (player == Player.X)
            {
                result = LongestPathInReverseOrder(board, x => x.GetMoves(x.Xposition), (x, y) => x.MoveX(y));
            }
            else
            {
                result = LongestPathInReverseOrder(board, x => x.GetMoves(x.Oposition), (x, y) => x.MoveO(y));
            }

            var pathLength = result.Item1;
            var backwardsPath = result.Item2;

            return Tuple.Create(pathLength, backwardsPath.Count > 0 ? backwardsPath.Last().Item2 : null);
        }

        // depth first search of longest walkable path, returns path length and the sequence of moves and boards in reverse order
        private static Tuple<int, List<Tuple<Board, BoardSpace>>> LongestPathInReverseOrder(Board board, Func<Board, IEnumerable<BoardSpace>> moveGetter, Action<Board, BoardSpace> makeMove)
        {
            var longestPathLength = 0;
            var path = new List<Tuple<Board, BoardSpace>>();

            foreach (var move in moveGetter(board))
            {
                var newBoard = board.Copy();
                makeMove(newBoard, move);

                var child = LongestPathInReverseOrder(newBoard, moveGetter, makeMove);
                var pathLength = child.Item1 + 1;

                if (pathLength > longestPathLength)
                {
                    longestPathLength = pathLength;
                    path = new List<Tuple<Board, BoardSpace>>(child.Item2) { Tuple.Create(newBoard, move) };
                }
            }

            return Tuple.Create(longestPathLength, path);
        }
    }
}