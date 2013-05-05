using System;
using DecisionTree;
using Newtonsoft.Json;

namespace DecisionTreeClassifier
{
    public class Classifier
    {
        private static void LogInput(TreeNode decisionTree, DataSet testData)
        {

            Console.WriteLine("Tree");
            Console.WriteLine(JsonConvert.SerializeObject(decisionTree, Formatting.Indented));
            Console.WriteLine();
            Console.WriteLine("Data");
            Console.WriteLine(JsonConvert.SerializeObject(testData, Formatting.Indented));

        }

        public static void Run(TreeNode decisionTree, DataSet testData)
        {


            foreach (var example in testData.Examples)
            {

            }





        }
    }
}
