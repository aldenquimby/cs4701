using System.Collections.Generic;

namespace Search
{
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