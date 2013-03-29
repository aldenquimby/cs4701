using System;
using System.Collections.Generic;

namespace Isolation
{
    // Generic isolation board evaluator
    public abstract class HeuristicBase
    {
        public abstract int Evaluate(Board board);
        public abstract string Name { get; }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj))
            {
                return false;
            }
            if (ReferenceEquals(this, obj))
            {
                return true;
            }
            return string.Equals(Name, ((HeuristicBase) obj).Name);
        }

        public override int GetHashCode()
        {
            return 17 ^ Name.GetHashCode();
        }
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

    /// <summary>
    /// Global static heuristic evaluator
    /// </summary>
    public class HeuristicCache
    {
        #region singleton

        private static readonly Lazy<HeuristicCache> Singleton = new Lazy<HeuristicCache>(() => new HeuristicCache());
        public static HeuristicCache I { get { return Singleton.Value; } }

        #endregion

        private readonly Dictionary<HeuristicBase, Dictionary<Board, int>> _cache;

        public HeuristicCache()
        {
            _cache = new Dictionary<HeuristicBase, Dictionary<Board, int>>();
        }

        public int Evaluate(Board board, HeuristicBase heuristic)
        {
            if (!_cache.ContainsKey(heuristic))
            {
                _cache[heuristic] = new Dictionary<Board, int>();
            }

            if (!_cache[heuristic].ContainsKey(board))
            {
                _cache[heuristic][board] = heuristic.Evaluate(board);
            }

            return _cache[heuristic][board];
        }

        public void LoadCacheFromDb()
        {
            throw new NotImplementedException();
        }

        public void DumpCacheToDb()
        {
            throw new NotImplementedException();
        }
    }
}
