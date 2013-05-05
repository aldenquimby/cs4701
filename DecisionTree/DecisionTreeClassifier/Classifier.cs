using System;
using System.Linq;
using DecisionTree;

namespace DecisionTreeClassifier
{
    public class Classifier
    {
        /// <summary>
        /// Classifies test data examples using decision tree, and outputs results to stdout.
        /// </summary>
        public static void Run(TreeNode decisionTree, DataSet testData)
        {
            try
            {
                foreach (var example in testData.Examples)
                {
                    var classLabel = GetClassLabel(decisionTree, example);

                    var attrs = example.Attributes.OrderBy(x => x.Key).Select(x => x.Value);

                    Console.WriteLine("{0}   ==> {1}", string.Join(",", attrs), classLabel);
                }
            }
            catch // we will only get an exception if the inputs were invalid
            {
                throw new Exception("Test data cannot be used with decision tree - one of them is invalid.");
            }
        }

        /// <summary>
        /// Recursive down the decision tree using the example's attributes
        /// until we hit a leaf node, which has the class label
        /// </summary>
        private static string GetClassLabel(TreeNode decisionTree, Example testExample)
        {
            if (decisionTree.ClassLabel != null)
            {
                return decisionTree.ClassLabel;
            }

            var exampleAttrVal = testExample.Attributes[decisionTree.ColNum];

            var subTree = decisionTree.Children[exampleAttrVal];

            return GetClassLabel(subTree, testExample);
        }
    }
}
