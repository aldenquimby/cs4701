using System;
using System.Collections.Generic;
using System.Linq;

namespace Search
{
    public interface ISearchList
    {
        SearchNode Find(StateBase state);
        void Push(SearchNode node);
        int Count { get; }
    }

    public class ClosedList : ISearchList
    {
        private readonly Dictionary<StateBase, List<SearchNode>> _listByState = new Dictionary<StateBase, List<SearchNode>>();

        public SearchNode Find(StateBase state)
        {
            if (!_listByState.ContainsKey(state) || _listByState[state].Count == 0)
            {
                return null;
            }
            return _listByState[state].First();
        }

        public void Push(SearchNode node)
        {
            if (_listByState.ContainsKey(node.State))
            {
                _listByState[node.State] = new List<SearchNode>();
            }
            _listByState[node.State].Add(node);

            Count++;
        }

        public void Remove(SearchNode node)
        {
            if (_listByState.ContainsKey(node.State))
            {
                _listByState[node.State].Remove(node);
                Count--;
            }
        }

        public int Count { get; private set; }
    }

    public abstract class OpenListBase : ISearchList
    {
        public abstract Func<SearchNode, SearchNode, int> CompareNodes { get; } 
        public abstract string AlgorithmName { get; }

        private readonly Dictionary<StateBase, List<SearchNode>> _stateHash = new Dictionary<StateBase, List<SearchNode>>(); 
        private readonly SortedList<HeapKey, List<SearchNode>> _heap = new SortedList<HeapKey, List<SearchNode>>(); 

        public SearchNode Find(StateBase state)
        {
            return _stateHash.ContainsKey(state) && _stateHash[state].Count > 0 ? _stateHash[state].First() : null;
        }

        public void Push(SearchNode node)
        {
            var key = new HeapKey(node, CompareNodes);

            // add to heap
            if (!_heap.ContainsKey(key))
            {
                _heap[key] = new List<SearchNode>();
            }
            _heap[key].Add(node);
            
            // add to state hash
            if (!_stateHash.ContainsKey(node.State))
            {
                _stateHash[node.State] = new List<SearchNode>();
            }
            _stateHash[node.State].Add(node);
            
            // increment count
            Count++;
        }

        public int Count { get; private set; }

        public void Replace(SearchNode inOpenList, SearchNode newNode)
        {
            var stateKey = inOpenList.State;
            _stateHash[stateKey].Remove(inOpenList);
            _stateHash[stateKey].Add(newNode);

            var heapKey = new HeapKey(inOpenList, CompareNodes);
            _heap[heapKey].Remove(inOpenList);
            _heap[heapKey].Add(newNode);
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
            _stateHash[node.State].Remove(node);

            return node;
        }
    }

    public class AStarOpenList : OpenListBase
    {
        public override Func<SearchNode, SearchNode, int> CompareNodes
        {
            get { return (node1, node2) => node1.FHat.CompareTo(node2.FHat); }
        }

        public override string AlgorithmName
        {
            get { return "A*"; }
        }
    }

    public class GreedyOpenList : OpenListBase
    {
        public override Func<SearchNode, SearchNode, int> CompareNodes
        {
            get { return (node1, node2) => node1.Hhat.CompareTo(node2.Hhat); }
        }

        public override string AlgorithmName
        {
            get { return "Greedy"; }
        }
    }

    public class UniformCostOpenList : OpenListBase
    {
        public override Func<SearchNode, SearchNode, int> CompareNodes
        {
            get { return (node1, node2) => node1.Ghat.CompareTo(node2.Ghat); }
        }

        public override string AlgorithmName
        {
            get { return "Uniform Cost"; }
        }
    }
}