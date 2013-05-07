using System.Collections.Generic;

namespace DecisionTree
{
    /// <summary>
    /// Represents one node in the decision tree.
    /// If the node has a ClassLabel, it's a leaf node.
    /// Otherwise ColNum represents the attribute in question at the node
    /// and Children represents branches based on the attribute values.
    /// </summary>
    public class TreeNode
    {
        public TreeNode()
        {
            Children = new Dictionary<string, TreeNode>();
        }

        public int ColNum { get; set; }
        public string ClassLabel { get; set; }
        public IDictionary<string, TreeNode> Children { get; set; } 
    }
}
