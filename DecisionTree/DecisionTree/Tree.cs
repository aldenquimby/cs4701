using System.Collections.Generic;

namespace DecisionTree
{
    public class Tree
    {
        public Tree()
        {
            Children = new List<Tree>();
        }

        public string Name { get; set; }
        public IList<Tree> Children { get; set; }
    }
}
