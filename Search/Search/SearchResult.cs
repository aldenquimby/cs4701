using System;
using System.Collections.Generic;

namespace Search
{
    public class SearchResult
    {
        public SearchNode Solution { get; set; }
        public int NodesGenerated { get; set; }
        public int NodesPrevGenerated { get; set; }
        public int NodesOnOpenList { get; set; }
        public int NodesOnClosedList { get; set; }
        public string AlgorithmName { get; set; }
        public string HeuristicName { get; set; }

        public string Print()
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