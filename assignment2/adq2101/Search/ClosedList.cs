using System.Collections.Generic;
using System.Linq;

namespace Search
{
    /// <summary>
    /// Closed list is HashTable of nodes by state for O(1) push/find/remove
    /// </summary>
    public class ClosedList
    {
        private readonly Dictionary<StateBase, List<SearchNode>> _hashByState = new Dictionary<StateBase, List<SearchNode>>();

        public SearchNode Find(StateBase state)
        {
            if (!_hashByState.ContainsKey(state) || _hashByState[state].Count == 0)
            {
                return null;
            }
            return _hashByState[state].First();
        }

        public void Push(SearchNode node)
        {
            if (!_hashByState.ContainsKey(node.State))
            {
                _hashByState[node.State] = new List<SearchNode>();
            }
            _hashByState[node.State].Add(node);

            Count++;
        }

        public void Remove(SearchNode node)
        {
            if (_hashByState.ContainsKey(node.State))
            {
                _hashByState[node.State].Remove(node);
                Count--;
            }
        }

        public int Count { get; private set; }
    }
}