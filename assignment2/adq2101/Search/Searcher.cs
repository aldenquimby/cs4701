namespace Search
{
    /// <summary>
    /// Delegate for cost to get from one state to another
    /// </summary>
    public delegate int CostFunc(StateBase from, StateBase to);
    
    /// <summary>
    /// Delegate for node comparator used to sort open list
    /// </summary>
    public delegate int OpenListComparator(SearchNode node1, SearchNode node2);
    
    /// <summary>
    /// Given an initial and goal state, able to perform all types of searches
    /// </summary>
    public class Searcher
    {
        private readonly StateBase _initialState;
        private readonly StateBase _goalState;

        public Searcher(StateBase initialState, StateBase goalState)
        {
            _initialState = initialState;
            _goalState = goalState;
        }

        /// <summary>
        /// Compares F-hat values for open list
        /// </summary>
        public SearchResult AStarSearch(CostFunc cost, IHeuristic heuristic)
        {
            OpenListComparator compareNodes = (node1, node2) => node1.FHat.CompareTo(node2.FHat);
            return GenericSearch(_initialState, _goalState, "A*", compareNodes, cost, heuristic);
        }

        /// <summary>
        /// Compares H-hat values for open list
        /// </summary>
        public SearchResult GreedySearch(CostFunc cost, IHeuristic heuristic)
        {
            OpenListComparator compareNodes = (node1, node2) => node1.Hhat.CompareTo(node2.Hhat);
            return GenericSearch(_initialState, _goalState, "Greedy", compareNodes, cost, heuristic);
        }

        /// <summary>
        /// Copmares G-hat values for open list
        /// </summary>
        public SearchResult UniformCostSearch(CostFunc cost)
        {
            OpenListComparator compareNodes = (node1, node2) => node1.Ghat.CompareTo(node2.Ghat);
            return GenericSearch(_initialState, _goalState, "UCS", compareNodes, cost, null);
        }

        /// <summary>
        /// Uniform Cost Search with cost = 1 (use depth instead of G-hat)
        /// </summary>
        public SearchResult BreadthFirstSearch()
        {
            OpenListComparator compareNodes = (node1, node2) => node1.Depth.CompareTo(node2.Depth);
            return GenericSearch(_initialState, _goalState, "BFS", compareNodes, (state1, state2) => 1, null);
        }

        /// <summary>
        /// Compare negative depth for open list
        /// </summary>
        public SearchResult DepthFirstSearch(int depthLimit, string name = null)
        {
            name = name ?? "DFS (limit " + depthLimit + ")";
            OpenListComparator compareNodes = (node1, node2) => (-node1.Depth).CompareTo(-node2.Depth);
            return GenericSearch(_initialState, _goalState, name, compareNodes, (state1, state2) => 1, null, depthLimit);
        }

        /// <summary>
        /// DFS with incrementing depth limit
        /// </summary>
        public SearchResult IterativeDeepeningDfs(int increment = 1)
        {
            var depthLimit = 1;

            var result = DepthFirstSearch(depthLimit, "IDDFS");

            while (result.Solution == null)
            {
                depthLimit += increment;
                result = DepthFirstSearch(depthLimit, "IDDFS");
            }

            return result;
        }

        /// <summary>
        /// Generic search algorithm
        /// </summary>
        private static SearchResult GenericSearch(StateBase initialState, StateBase goalState, string algName, OpenListComparator comparator, CostFunc cost, IHeuristic heuristic, int? depthLimit = null)
        {
            var openList = new OpenList(comparator);
            var closedList = new ClosedList();
            var cache = new HeuristicCache(goalState, heuristic);
            var nodesGenerated = 0;
            var nodesPrevGenerated = 0;

            // add initial node to open list
            openList.Push(new SearchNode(cost, initialState, null, null, cache.Evaluate));

            while (true)
            {
                // if nothing on open list, fail
                if (openList.Count == 0)
                {
                    return new SearchResult(null, nodesGenerated, nodesPrevGenerated, openList.Count, closedList.Count, algName, heuristic);
                }

                // get next node to expand
                var node = openList.Pop();
                closedList.Push(node);

                // if at goal state, success
                if (node.State.Equals(goalState))
                {
                    return new SearchResult(node, nodesGenerated, nodesPrevGenerated, openList.Count, closedList.Count, algName, heuristic);
                }

                // if at depth limit, don't generate successors
                if (depthLimit != null && node.Depth == depthLimit)
                {
                    continue;
                }

                var daughters = node.Successors(cost, cache.Evaluate);

                foreach (var daughter in daughters)
                {
                    nodesGenerated++;

                    // if this state is already in open list, replace old node with new node if g-hat is better
                    var foundInOpen = openList.Find(daughter.State);
                    if (foundInOpen != null)
                    {
                        nodesPrevGenerated++;
                        if (daughter.Ghat < foundInOpen.Ghat)
                        {
                            openList.Replace(foundInOpen, daughter);
                        }
                    }
                    else
                    {
                        // else if this state is already in closed list, move from closed to open if g-hat is better
                        var foundInClosed = closedList.Find(daughter.State);
                        if (foundInClosed != null)
                        {
                            nodesPrevGenerated++;
                            if (daughter.Ghat < foundInClosed.Ghat)
                            {
                                openList.Push(daughter);
                                closedList.Remove(foundInClosed);
                            }
                        }
                        else
                        {
                            // else didn't find in open or closed, add to open
                            openList.Push(daughter);
                        }
                    }
                }
            }
        }
    }
}
