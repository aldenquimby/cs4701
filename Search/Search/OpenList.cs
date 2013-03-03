using System;
using System.Collections.Generic;
using System.Linq;

namespace Search
{
    /// <summary>
    /// OpenList is Heap sorted by specified comparator for O(log(N)) push/pop/replace, 
    /// and HashTable for O(1) find
    /// </summary>
    public class OpenList
    {
        private readonly OpenListComparator _compareNodes;
        private readonly Dictionary<StateBase, List<SearchNode>> _hashByState; 
        private readonly SortedList<HeapKey, List<SearchNode>> _heap;

        public OpenList(OpenListComparator compareNodes)
        {
            _compareNodes = compareNodes;
            _hashByState = new Dictionary<StateBase, List<SearchNode>>();
            _heap = new SortedList<HeapKey, List<SearchNode>>();
        }

        public int Count { get; private set; }

        public SearchNode Find(StateBase state)
        {
            return _hashByState.ContainsKey(state) && _hashByState[state].Count > 0 ? _hashByState[state].First() : null;
        }

        public void Push(SearchNode node)
        {
            var key = new HeapKey(node, _compareNodes);

            // add to heap
            if (!_heap.ContainsKey(key))
            {
                _heap[key] = new List<SearchNode>();
            }
            _heap[key].Add(node);
            
            // add to state hash
            if (!_hashByState.ContainsKey(node.State))
            {
                _hashByState[node.State] = new List<SearchNode>();
            }
            _hashByState[node.State].Add(node);
            
            // increment count
            Count++;
        }

        public void Replace(SearchNode inOpenList, SearchNode newNode)
        {
            _hashByState[inOpenList.State].Remove(inOpenList);
            _hashByState[newNode.State].Add(newNode);

            _heap[new HeapKey(inOpenList, _compareNodes)].Remove(inOpenList);
            _heap[new HeapKey(newNode, _compareNodes)].Add(newNode);
        }

        public SearchNode Pop()
        {
            if (Count == 0)
            {
                return null;
            }

            // decrement count
            Count--;

            // pop heap
            var nextNodes = _heap.First().Value;
            var node = nextNodes.First();

            if (nextNodes.Count > 1)
            {
                nextNodes.RemoveAt(0);
            }
            else
            {
                _heap.RemoveAt(0);
            }

            // clean up state hash
            _hashByState[node.State].Remove(node);

            return node;
        }

        #region HeapKey

        private class HeapKey : IComparable<HeapKey>, IEquatable<HeapKey>
        {
            private readonly SearchNode _node;
            private readonly OpenListComparator _compareNodes;

            public HeapKey(SearchNode node, OpenListComparator compareNodes)
            {
                _node = node;
                _compareNodes = compareNodes;
            }

            public int CompareTo(HeapKey other)
            {
                return _compareNodes(_node, other._node);
            }

            public bool Equals(HeapKey other)
            {
                return other != null && _node.State.Equals(other._node.State);
            }

            public override bool Equals(object obj)
            {
                return Equals(obj as HeapKey);
            }

            public override int GetHashCode()
            {
                return _node.State.GetHashCode();
            }
        }

        #endregion
    }
}