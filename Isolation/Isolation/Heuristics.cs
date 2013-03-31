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
