using System;

namespace Search
{
    public class HeapKey : IComparable<HeapKey>, IEquatable<HeapKey>
    {
        private readonly SearchNode _node;
        private readonly Func<SearchNode, SearchNode, int> _compareNodes;

        public HeapKey(SearchNode node, Func<SearchNode, SearchNode, int> compareNodes)
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
}