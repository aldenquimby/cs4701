using System;
using System.IO;
using System.Reflection;
using Newtonsoft.Json;
using DecisionTree;

namespace DecisionTreeClassifier
{
    public class Program
    {
        public static void Main(string[] args)
        {
            try
            {
                // construct test data
                var fileName = GetTestDataFilePath(args);
                var testData = DataSet.ConstructFromCsv(fileName);

                // construct decision tree
                var decisionTree = GetDecisionTree();

                // run the classifier
                Classifier.Run(decisionTree, testData);

                // output the result
                Console.WriteLine("Result goes here");
            }
            catch (Exception e)
            {
                Console.WriteLine("OH NO, SOMETHING FAILED!");
                Console.WriteLine(e.Message);
            }

            Console.WriteLine("Done! Press any key to terminate.");
            Console.ReadKey(); 
        }

        private static string GetTestDataFilePath(string[] args)
        {
            var fileName = args.Length == 1 ? args[0] : "test_data.csv";

            var assemblyLocation = Assembly.GetExecutingAssembly().Location;
            var assemblyDirectory = new FileInfo(assemblyLocation).DirectoryName ?? "";

            return Path.Combine(assemblyDirectory, fileName);
        }

        private static TreeNode GetDecisionTree()
        {
            // the DecisionTreeLearner will modify this line to be a JSON serialized tree
            const string serializedDecisionTree = "{\"Name\":\"Parent\",\"Children\":[{\"Name\":\"Child1\",\"Children\":[]},{\"Name\":\"Child2\",\"Children\":[]}]}";

            return JsonConvert.DeserializeObject<TreeNode>(serializedDecisionTree);
        }
    }
}
