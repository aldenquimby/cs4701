using System;
using DecisionTree;
using Newtonsoft.Json;

namespace DecisionTreeClassifier
{
    public class Classifier
    {
        public void Run(Tree decisionTree, DataSet testData)
        {
            Console.WriteLine("Tree");
            Console.WriteLine(JsonConvert.SerializeObject(decisionTree, Formatting.Indented));
            Console.WriteLine();
            Console.WriteLine("Data");
            Console.WriteLine(JsonConvert.SerializeObject(testData, Formatting.Indented));




        }
    }
}
