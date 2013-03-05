using System;
using System.Collections.Generic;
using System.Linq;

namespace Search
{
    /// <summary>
    /// Node in search tree
    /// </summary>
    public class SearchNode
    {
        public SearchNode(CostFunc cost, StateBase state, string op, SearchNode parent, Func<StateBase, int> hHatGetter)
        {
            State = state;
            Operator = op;
            Parent = parent;
            Hhat = hHatGetter(state);
            if (parent != null)
            {
                Ghat = parent.Ghat + cost(parent.State, state);
                Depth = parent.Depth + 1;
            }
        }

        public StateBase State { get; set; }
        public string Operator { get; set; }
        public SearchNode Parent { get; set; }

        public int Depth { get; set; }
        public int Ghat { get; set; }
        public int Hhat { get; set; }
        public int FHat { get { return Ghat + Hhat; } }

        public IEnumerable<SearchNode> Successors(CostFunc costFunc, Func<StateBase, int> hHatGetter)
        {
            return State.Successors().Select(x => new SearchNode(costFunc, x.Value, x.Key, this, hHatGetter));
        }
    }

    /// <summary>
    /// Specific problems (like 15-puzzle) need to implement this to represent a state
    /// </summary>
    public abstract class StateBase : IEquatable<StateBase>
    {
        public abstract bool Equals(StateBase other);
        public abstract override bool Equals(object obj);
        public abstract override int GetHashCode();
        public abstract IDictionary<string, StateBase> Successors();
    }

    /// <summary>
    /// Final search node and statistics on search
    /// </summary>
    public class SearchResult
    {
        public SearchResult(SearchNode solution, int nodesGen, int nodesPrevGen, int nodesOnOpen,
                            int nodesOnClosed, string algName, IHeuristic heuristic)
        {
            Solution = solution;
            NodesGenerated = nodesGen;
            NodesPrevGenerated = nodesPrevGen;
            NodesOnOpenList = nodesOnOpen;
            NodesOnClosedList = nodesOnClosed;
            AlgorithmName = algName;
            HeuristicName = heuristic == null ? null : heuristic.Name;
        }

        public SearchNode Solution { get; private set; }
        public int NodesGenerated { get; private set; }
        public int NodesPrevGenerated { get; private set; }
        public int NodesOnOpenList { get; private set; }
        public int NodesOnClosedList { get; private set; }
        public string AlgorithmName { get; private set; }
        public string HeuristicName { get; private set; }

        public override string ToString()
        {
            var name = AlgorithmName + (HeuristicName == null ? ":" : " (" + HeuristicName + "):");

            var listOfMoves = new Stack<string>();
            var node = Solution;
            while (node != null)
            {
                if (node.Operator != null)
                {
                    listOfMoves.Push(node.Operator);
                }
                node = node.Parent;
            }

            var listOfMovesString = listOfMoves.Count == 0 ? "FAIL" : string.Format(@"""{0}""", string.Join(@""" """, listOfMoves));

            var solution = string.Format("((({0}) {1}) {2} {3} {4} {5})", listOfMovesString, listOfMoves.Count,
                                         NodesGenerated, NodesPrevGenerated, NodesOnOpenList, NodesOnClosedList);

            return string.Format("{0}{1}{2}{1}", name, Environment.NewLine, solution);
        }
    }
}
