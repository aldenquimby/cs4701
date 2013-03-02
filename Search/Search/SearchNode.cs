using System.Collections.Generic;
using System.Linq;

namespace Search
{
    public class SearchNode
    {
        public SearchNode(CostFunc cost, StateBase state, string op, SearchNode parent, HeuristicCache cache)
        {
            State = state;
            Operator = op;
            Parent = parent;
            if (parent != null)
            {
                Ghat = parent.Ghat + cost(parent.State, state);
            }
            if (cache != null)
            {
                Hhat = cache.Evaluate(state);
            }
        }

        public StateBase State { get; set; }
        public string Operator { get; set; }
        public SearchNode Parent { get; set; }
        
        public int Ghat { get; set; }
        public int Hhat { get; set; }
        public int FHat { get { return Ghat + Hhat; } }

        public IEnumerable<SearchNode> Successors(CostFunc costFunc, HeuristicCache cache)
        {
            return State.Successors().Select(x => new SearchNode(costFunc, x.Value, x.Key, this, cache));
        }
    }
}
