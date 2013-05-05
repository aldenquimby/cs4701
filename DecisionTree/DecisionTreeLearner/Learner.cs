using System;
using System.Collections.Generic;
using System.Linq;
using DecisionTree;

namespace DecisionTreeLearner
{
    public class Learner
    {
        /// <summary>
        /// Construct decision tree from training data
        /// </summary>
        public static TreeNode ConstructDecisionTree(DataSet trainingData)
        {
            return LearnInternal(trainingData.Examples, trainingData.Attributes, new List<Example>());
        }

        /// <summary>
        /// Recursively construct decision tree from examples and attributes
        /// </summary>
        private static TreeNode LearnInternal(ICollection<Example> examples, ICollection<DataAttribute> attributes, IEnumerable<Example> parentExamples)
        {
            // if no examples, use parent plurality
            if (examples.Count == 0)
            {
                return PluralityValue(parentExamples);
            }

            // if all examples have same classification, use it
            if (examples.Select(x => x.ClassLabel).Distinct().Count() == 1)
            {
                return new TreeNode
                    {
                        ClassLabel = examples.First().ClassLabel,
                    };
            }

            // if no attributes, use plurality
            if (attributes.Count == 0)
            {
                return PluralityValue(examples);
            }

            // pick most important attribute
            var attr = attributes.OrderByDescending(x => Importance(examples, x)).First();

            // get other attributes
            var remainingAttrs = attributes.Where(x => x != attr).ToList();

            // group the examples by their value for selected attribute
            var examplesByAttrValue = examples.ToLookup(x => x.Attributes[attr.ColNum]);

            // construct a decision tree for this attribute
            var tree = new TreeNode
                {
                    ColNum = attr.ColNum,
                };

            // for each value, recursively construct a sub-tree
            foreach (var attrValue in attr.Values)
            {
                var childExamples = examplesByAttrValue[attrValue].ToList();

                var subTree = LearnInternal(childExamples, remainingAttrs, examples);

                tree.Children.Add(attrValue, subTree);
            }

            return tree;
        }

        /// <summary>
        /// Selects most common ClassLabel, breaking ties randomly
        /// </summary>
        private static TreeNode PluralityValue(IEnumerable<Example> examples)
        {
            var classLabel = examples.ToLookup(x => x.ClassLabel)
                                     .OrderByDescending(x => x.Count())
                                     .First()
                                     .Key;

            return new TreeNode
                {
                    ClassLabel = classLabel,
                };
        }

        /// <summary>
        /// Information gain for an attribute on a set of examples
        /// </summary>
        private static double Importance(ICollection<Example> examples, DataAttribute attr)
        {
            return Entropy(examples) - Remainder(examples, attr);
        } 

        /// <summary>
        /// Entropy of a set of examples
        /// </summary>
        private static double Entropy(ICollection<Example> examples)
        {
            var exampleValues = examples.ToLookup(x => x.ClassLabel);

            var probabilities = exampleValues.Select(x => (double)x.Count() / examples.Count);

            return -probabilities.Sum(x => x * Math.Log(x, 2));
        }
    
        /// <summary>
        /// Remaining entropy after attribute divides example set
        /// </summary>
        private static double Remainder(ICollection<Example> examples, DataAttribute attr)
        {
            var subsets = examples.ToLookup(x => x.Attributes[attr.ColNum]);

            return subsets.Select(x => x.ToList())
                          .Select(subset => new
                              {
                                  prob = (double)subset.Count / examples.Count,
                                  entropy = Entropy(subset),
                              })
                          .Sum(x => x.prob * x.entropy);
        }
    }
}
