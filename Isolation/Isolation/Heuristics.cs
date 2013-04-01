using System;
using System.Collections.Generic;
using System.Linq;

namespace Isolation
{
    // Generic isolation board evaluator
    public abstract class HeuristicBase
    {
        public abstract int Evaluate(Board board);
        public abstract string Name { get; }
    }
    
    // Number possible moves for me, minus number possible moves for opponent
    public class NumberOfMovesHeuristic : HeuristicBase
    {
        public override int Evaluate(Board board)
        {
            var myMoveCount = board.GetMyValidMoves().Count;

            if (myMoveCount == 0)
            {
                return int.MinValue;
            }

            var opponenentMoveCount = board.GetOpponentValidMoves().Count;

            if (opponenentMoveCount == 0)
            {
                return int.MaxValue;
            }

            return myMoveCount - opponenentMoveCount;
        }

        public override string Name { get { return "NumberOfMoves"; } }
    }

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

            // if i can't move, i lose
            if (myOpenArea.Count == 0)
            {
                return int.MinValue;
            }

            // if opponent can't move, i win
            if (opponentOpenArea.Count == 0)
            {
                return int.MaxValue;
            }

            // if we're in completely separate areas, whoever has a bigger area will win
            if (myOpenArea.All(x => !opponentOpenArea.Contains(x)))
            {
                return myOpenArea.Count > opponentOpenArea.Count ? int.MaxValue : int.MinValue;
            }

            // use open area size difference
            return myOpenArea.Count - opponentOpenArea.Count;
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

        private readonly Dictionary<string, Dictionary<string, int>> _cache;

        public HeuristicCache()
        {
            _cache = new Dictionary<string, Dictionary<string, int>>();
        }

        public int Evaluate(Board board, HeuristicBase heuristic)
        {
            if (board == null || heuristic == null)
            {
                return 0;
            }

            if (!_cache.ContainsKey(heuristic.Name))
            {
                _cache[heuristic.Name] = new Dictionary<string, int>();
            }

            var boardString = board.ToFlatString();

            if (!_cache[heuristic.Name].ContainsKey(boardString))
            {
                _cache[heuristic.Name][boardString] = heuristic.Evaluate(board);
            }

            return _cache[heuristic.Name][boardString];
        }

        public void LoadCache(IList<HeuristicDto> dtos)
        {
            _cache.Clear();

            foreach (var dto in dtos)
            {
                if (!_cache.ContainsKey(dto.Heuristic))
                {
                    _cache[dto.Heuristic] = new Dictionary<string, int>();
                }

                _cache[dto.Heuristic][dto.Board] = dto.Score;
            }
        }

        public IList<HeuristicDto> DumpCache()
        {
            var heuristics = _cache.SelectMany(heuristic => heuristic.Value, (heuristic, board) => new HeuristicDto
                {
                    Heuristic = heuristic.Key,
                    Score = board.Value,
                    Board = board.Key,
                }).ToList();

            _cache.Clear();

            return heuristics;
        }
    }
}
