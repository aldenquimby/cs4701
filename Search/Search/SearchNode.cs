using System;
using System.Collections.Generic;
using System.Linq;

namespace Search
{
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
}
