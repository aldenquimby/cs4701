using System;
using Newtonsoft.Json;
using DecisionTree;

namespace DecisionTreeClassifier
{
    public class Program
    {
        public static void Main(string[] args)
        {
            Console.WriteLine("Cool this works!");

            var tree = JsonConvert.DeserializeObject<Tree>(SerializedDecisionTree);

            var reSerialized = JsonConvert.SerializeObject(tree, Formatting.Indented);

            Console.WriteLine(reSerialized);

            Console.ReadKey();
        }

        // the DecisionTreeLearner will modify this program
        // and put a JSON serialized version of the tree here
        public static string SerializedDecisionTree = "{\"Name\":\"Parent\",\"Children\":[{\"Name\":\"Child1\",\"Children\":[]},{\"Name\":\"Child2\",\"Children\":[]}]}";
    }
}
