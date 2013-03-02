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

        public string AStarSearch(CostFunc cost, IHeuristic heuristic)
        {
            var cache = new HeuristicCache(_goalState, heuristic);
            var result = Search(_initialState, _goalState, new AStarOpenList(), cost, heuristic, cache);
            return result.PrintResults();
        }

        public string GreedySearch(CostFunc cost, IHeuristic heuristic)
        {
            var cache = new HeuristicCache(_goalState, heuristic);
            var result = Search(_initialState, _goalState, new GreedyOpenList(), cost, heuristic, cache);
            return result.PrintResults();
        }

        public string UniformCostSearch(CostFunc cost)
        {
            var result = Search(_initialState, _goalState, new UniformCostOpenList(), cost, null, null);
            return result.PrintResults();
        }

        public string BreadthFirstSearch()
        {
            // no heuristic and a uniform cost of 1
            var result = Search(_initialState, _goalState, new UniformCostOpenList(), (state1, state2) => 1, null, null);
            return result.PrintResults();
        }

        private static SearchResult Search(StateBase initialState, StateBase goalState, OpenListBase openList, CostFunc cost, IHeuristic heuristic, HeuristicCache cache)
        {
            var closedList = new ClosedList();
            var nodesGenerated = 0;
            var nodesPrevGenerated = 0;

            var initialNode = new SearchNode(cost, initialState, null, null, cache);

            openList.Push(initialNode);

            while (true)
            {
                if (openList.Count == 0)
                {
                    return GetResults(null, openList, closedList, heuristic, nodesGenerated, nodesPrevGenerated);
                }

                var node = openList.Pop();

                closedList.Push(node);

                if (node.State.Equals(goalState))
                {
                    return GetResults(node, openList, closedList, heuristic, nodesGenerated, nodesPrevGenerated);
                }

                var daughters = node.Successors(cost, cache);

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

        private static SearchResult GetResults(SearchNode solution, OpenListBase openList, ClosedList closedList,
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
