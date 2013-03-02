namespace Search
{
    public delegate int CostFunc(StateBase from, StateBase to);

    public class GenericSearch
    {
        private readonly StateBase _initialState;
        private readonly StateBase _goalState;

        public GenericSearch(StateBase initialState, StateBase goalState)
        {
            _initialState = initialState;
            _goalState = goalState;
        }

        /// <summary>
        /// 
        /// </summary>
        public SearchResult AStarSearch(CostFunc cost, IHeuristic heuristic)
        {
            return Search(_initialState, _goalState, new AStarOpenList(), cost, heuristic);
        }

        /// <summary>
        /// 
        /// </summary>
        public SearchResult GreedySearch(CostFunc cost, IHeuristic heuristic)
        {
            return Search(_initialState, _goalState, new GreedyOpenList(), cost, heuristic);
        }

        /// <summary>
        /// No heuristic
        /// </summary>
        public SearchResult UniformCostSearch(CostFunc cost)
        {
            return Search(_initialState, _goalState, new UniformCostOpenList(), cost, null);
        }

        /// <summary>
        /// Uniform Cost Search with cost = 1
        /// </summary>
        public SearchResult BreadthFirstSearch()
        {
            return Search(_initialState, _goalState, new BreadthFirstSearchOpenList(), (state1, state2) => 1, null);
        }

        /// <summary>
        /// 
        /// </summary>
        public SearchResult DepthFirstSearch(int depthLimit, string name = "DFS")
        {
            return Search(_initialState, _goalState, new DepthFirstSearchOpenList(name), (state1, state2) => 1, null, depthLimit);
        }

        /// <summary>
        /// 
        /// </summary>
        public SearchResult IterativeDeepeningDfs()
        {
            var depthLimit = 1;

            var result = DepthFirstSearch(depthLimit, "IDDFS");

            while (result.Solution == null)
            {
                depthLimit++;
                result = DepthFirstSearch(depthLimit, "IDDFS");
            }

            return result;
        }

        /// <summary>
        /// 
        /// </summary>
        private static SearchResult Search(StateBase initialState, StateBase goalState, OpenListBase openList, CostFunc cost, IHeuristic heuristic, int? depthLimit = null)
        {
            var closedList = new ClosedList();
            var nodesGenerated = 0;
            var nodesPrevGenerated = 0;

            var cache = new HeuristicCache(goalState, heuristic);

            var initialNode = new SearchNode(cost, initialState, null, null, cache.Evaluate);

            openList.Push(initialNode);

            while (true)
            {
                // if nothing on open list, fail
                if (openList.Count == 0)
                {
                    return GetResult(null, openList, closedList, heuristic, nodesGenerated, nodesPrevGenerated);
                }

                var node = openList.Pop();
                closedList.Push(node);

                // if at goal state, success
                if (node.State.Equals(goalState))
                {
                    return GetResult(node, openList, closedList, heuristic, nodesGenerated, nodesPrevGenerated);
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
                            // didn't find in open or closed
                            openList.Push(daughter);
                        }
                    }
                }
            }
        }

        /// <summary>
        /// 
        /// </summary>
        private static SearchResult GetResult(SearchNode solution, OpenListBase openList, ClosedList closedList,
                                              IHeuristic heuristic, int nodesGenerated, int nodesPrevGenerated)
        {
            return new SearchResult
            {
                NodesGenerated = nodesGenerated,
                NodesOnClosedList = closedList.Count,
                NodesOnOpenList = openList.Count,
                NodesPrevGenerated = nodesPrevGenerated,
                Solution = solution,
                AlgorithmName = openList.AlgorithmName,
                HeuristicName = heuristic == null ? null : heuristic.Name,
            };
        }
    }
}
