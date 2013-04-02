using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.Caching;

namespace Isolation
{
    // Generic isolation board evaluator
    public abstract class HeuristicBase
    {
        public abstract int Evaluate(Board board);
        public abstract string Name { get; }
    }
    
    // Number possible moves for me vs. opponent
    public class NumberOfMovesHeuristic : HeuristicBase
    {
        public override int Evaluate(Board board)
        {
            var myMoveCount = board.GetMyValidMoves().Count();
            var opponenentMoveCount = board.GetOpponentValidMoves().Count();

            if (myMoveCount == 0 && opponenentMoveCount == 0)
            {
                return board.PlayerToMove == board.MyPlayer ? int.MinValue : int.MaxValue;
            }

            if (myMoveCount == 0)
            {
                return int.MinValue;
            }
        
            if (opponenentMoveCount == 0)
            {
                return int.MaxValue;
            }

            return myMoveCount - opponenentMoveCount;
        }

        public override string Name { get { return "NumberOfMoves"; } }
    }

    // Number of empty spaces reachable by me vs. opponent
    public class OpenAreaHeuristic : HeuristicBase
    {
        private IEnumerable<BoardSpace> GetSurroundingSpaces(BoardSpace space)
        {
            if (space.Row > 0)
            {
                yield return new BoardSpace((byte)(space.Row - 1), space.Col);

                if (space.Col > 0)
                {
                    yield return new BoardSpace((byte)(space.Row - 1), (byte)(space.Col - 1));
                    yield return new BoardSpace(space.Row, (byte)(space.Col - 1));
                }
                if (space.Col < 7)
                {
                    yield return new BoardSpace((byte)(space.Row - 1), (byte)(space.Col + 1));
                    yield return new BoardSpace(space.Row, (byte)(space.Col + 1));
                }
            }
            if (space.Row < 7)
            {
                yield return new BoardSpace((byte)(space.Row + 1), space.Col);

                if (space.Col > 0)
                {
                    yield return new BoardSpace((byte)(space.Row + 1), (byte)(space.Col - 1));
                }
                if (space.Col < 7)
                {
                    yield return new BoardSpace((byte)(space.Row + 1), (byte)(space.Col + 1));
                }
            }
        } 

        private HashSet<BoardSpace> GetOpenArea(Board board, Player player)
        {
            var initialPosition = player == Player.X ? board.Xposition : board.Oposition;

            var toExamine = new Queue<BoardSpace>(new []{initialPosition});
            var closed = new HashSet<BoardSpace>{initialPosition};
            var accessible = new HashSet<BoardSpace>();

            while (toExamine.Count > 0)
            {
                var space = toExamine.Dequeue();

                // enqueue empty spaces immediately next to this space
                foreach (var successor in GetSurroundingSpaces(space))
                {
                    // skip spaces we've seen
                    if (closed.Contains(successor))
                    {
                        continue;
                    }

                    // mark this sapce as seen
                    closed.Add(successor);

                    if (board[successor.Row, successor.Col] == BoardSpaceValue.Empty)
                    {
                        accessible.Add(successor);
                        toExamine.Enqueue(successor);
                    }
                }
            }

            return accessible;
        }

        public override int Evaluate(Board board)
        {
            var myOpenArea = GetOpenArea(board, board.MyPlayer);
            var opponentOpenArea = GetOpenArea(board, board.OpponentPlayer);

            if (myOpenArea.Count == 0 && opponentOpenArea.Count == 0)
            {
                return board.PlayerToMove == board.MyPlayer ? int.MinValue : int.MaxValue;
            }

            if (myOpenArea.Count == 0)
            {
                return int.MinValue;
            }
        
            if (opponentOpenArea.Count == 0)
            {
                return int.MaxValue;
            }

            // if we're in completely separate areas, whoever has a bigger area will win
            if (myOpenArea.All(x => !opponentOpenArea.Contains(x)))
            {
                // if areas are the same size, whoever has to move right now will lose
                if (myOpenArea.Count == opponentOpenArea.Count)
                {
                    return board.PlayerToMove == board.MyPlayer ? int.MinValue : int.MaxValue;
                }
                
                return myOpenArea.Count > opponentOpenArea.Count ? int.MaxValue : int.MinValue;
            }

            // since we're in the same area, use current move count
            return board.GetMyValidMoves().Count() - board.GetOpponentValidMoves().Count();
        }

        public override string Name { get { return "OpenArea"; } }
    }

    // Global static heuristic evaluator
    public class HeuristicCache
    {
        #region singleton

        private static readonly Lazy<HeuristicCache> Singleton = new Lazy<HeuristicCache>(() => new HeuristicCache());
        public static HeuristicCache I { get { return Singleton.Value; } }

        #endregion

        private readonly IDictionary<string, IDictionary<string, int>> _cache;

        public HeuristicCache()
        {
            _cache = new ConcurrentDictionary<string, IDictionary<string, int>>();
        }

        public void DumpKeyCount()
        {
            Console.WriteLine(_cache.Keys.Count);
        }

        public int Evaluate(Board board, HeuristicBase heuristic)
        {
            if (board == null || heuristic == null)
            {
                return 0;
            }

            var boardString = board.ToFlatString();

            //if (_cache.Keys.Count > 10000000)
            //{
            //    if (_cache.ContainsKey(boardString) && _cache[boardString].ContainsKey(heuristic.Name))
            //    {
            //        return _cache[boardString][heuristic.Name];
            //    }
            //    else
            //    {
            //        return heuristic.Evaluate(board);
            //    }
            //}

            if (!_cache.ContainsKey(boardString))
            {
                _cache[boardString] = new ConcurrentDictionary<string, int>();
            }

            if (!_cache[boardString].ContainsKey(heuristic.Name))
            {
                _cache[boardString][heuristic.Name] = heuristic.Evaluate(board);
            }

            return _cache[boardString][heuristic.Name];
        }

        public void RemoveFromCache(Board board)
        {
            _cache.Remove(board.ToFlatString());
        }

        public void LoadCache(IList<HeuristicDto> dtos)
        {
            _cache.Clear();

            foreach (var dto in dtos)
            {
                if (!_cache.ContainsKey(dto.Board))
                {
                    _cache[dto.Board] = new Dictionary<string, int>();
                }

                _cache[dto.Board][dto.Heuristic] = dto.Score;
            }
        }

        public IList<HeuristicDto> DumpCache()
        {
            var heuristics = _cache.SelectMany(kvp => kvp.Value, (kvp, kvp2) => new HeuristicDto
                {
                    Board = kvp.Key,
                    Heuristic = kvp2.Key,
                    Score = kvp2.Value,
                }).ToList();

            _cache.Clear();

            return heuristics;
        }
    }
}
