using System.Collections.Generic;
using DecisionTree;

namespace DecisionTreeLearner
{
    public class Learner
    {
        public Tree Run(DataSet trainingData)
        {
            return new Tree{Name = "Parent", Children = new List<Tree>
                {
                    new Tree{ Name = "Child1"},
                    new Tree{ Name = "Child2"},
                }};
        }
    }
}
