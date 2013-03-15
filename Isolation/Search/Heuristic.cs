using System.Collections.Generic;

namespace Search
{
    /// <summary>
    /// Generic heuristic evaluator interface
    /// </summary>
    public interface IHeuristic
    {
        int Evaluate(StateBase state, StateBase goal);
        string Name { get; }
    }

    /// <summary>
    /// Wrapper for heuristic interface to make implementation cleaner
    /// </summary>
    public abstract class HeuristicBase<T> : IHeuristic where T : StateBase
    {
        public int Evaluate(StateBase state, StateBase goal)
        {
            return Evaluate(state as T, goal as T);
        }

        public abstract int Evaluate(T state, T goal);

        public abstract string Name { get; }
    }

    /// <summary>
    /// Cache heuristic calculations for specific goal state and heuristic
    /// so that we don't evalaute heuristic on same state multiple times
    /// </summary>
    public class HeuristicCache
    {
        private readonly IDictionary<StateBase, int> _cache;
        private readonly StateBase _goal;
        private readonly IHeuristic _heuristic;

        public HeuristicCache(StateBase goal, IHeuristic heuristic)
        {
            _goal = goal;
            _heuristic = heuristic;
            _cache = new Dictionary<StateBase, int>();
        }

        public int Evaluate(StateBase state)
        {
            if (_heuristic == null || _goal == null || state == null)
            {
                return 0;
            }

            if (_cache.ContainsKey(state))
            {
                return _cache[state];
            }

            var result = _heuristic.Evaluate(state, _goal);
            _cache[state] = result;
            return result;
        }
    }
}