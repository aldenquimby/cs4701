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
            return board.GetMyValidMoves().Count - board.GetOpponentValidMoves().Count;
        }

        public override string Name { get { return "NumberOfMoves"; } }
    }

    // Global static heuristic evaluator
    public class HeuristicCache
    {
        #region singleton

        private static readonly Lazy<HeuristicCache> Singleton = new Lazy<HeuristicCache>(() => new HeuristicCache());
        public static HeuristicCache I { get { return Singleton.Value; } }

        #endregion

        private readonly Dictionary<string, Dictionary<Board, int>> _cache;

        public HeuristicCache()
        {
            _cache = new Dictionary<string, Dictionary<Board, int>>();
        }

        public int Evaluate(Board board, HeuristicBase heuristic)
        {
            if (board == null || heuristic == null)
            {
                return 0;
            }

            if (!_cache.ContainsKey(heuristic.Name))
            {
                _cache[heuristic.Name] = new Dictionary<Board, int>();
            }

            if (!_cache[heuristic.Name].ContainsKey(board))
            {
                _cache[heuristic.Name][board] = heuristic.Evaluate(board);
            }

            return _cache[heuristic.Name][board];
        }

        public void LoadCache(IList<HeuristicDto> dtos)
        {
            _cache.Clear();

            foreach (var dto in dtos)
            {
                var board = new Board(dto.Board);

                if (!_cache.ContainsKey(dto.Heuristic))
                {
                    _cache[dto.Heuristic] = new Dictionary<Board, int>();
                }

                _cache[dto.Heuristic][board] = dto.Score;
            }
        }

        public IList<HeuristicDto> DumpCache()
        {
            var heuristics = _cache.SelectMany(heuristic => heuristic.Value, (heuristic, board) => new HeuristicDto
                {
                    Heuristic = heuristic.Key,
                    Score = board.Value,
                    Board = board.Key.ToFlatString(),
                }).ToList();

            _cache.Clear();

            return heuristics;
        }
    }
}
