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
                var testData = DataSet.ConstructFromCsv(fileName, hasClassLabel:false);

                // construct decision tree
                var decisionTree = GetDecisionTree();

                // run the classifier
                Classifier.Run(decisionTree, testData);
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
            const string serializedDecisionTree = "{\"ColNum\":4,\"ClassLabel\":null,\"Children\":{\"Some\":{\"ColNum\":0,\"ClassLabel\":\"Yes\",\"Children\":{}},\"Full\":{\"ColNum\":3,\"ClassLabel\":null,\"Children\":{\"Yes\":{\"ColNum\":8,\"ClassLabel\":null,\"Children\":{\"French\":{\"ColNum\":0,\"ClassLabel\":\"No\",\"Children\":{}},\"Thai\":{\"ColNum\":2,\"ClassLabel\":null,\"Children\":{\"No\":{\"ColNum\":0,\"ClassLabel\":\"No\",\"Children\":{}},\"Yes\":{\"ColNum\":0,\"ClassLabel\":\"Yes\",\"Children\":{}}}},\"Burger\":{\"ColNum\":0,\"ClassLabel\":\"Yes\",\"Children\":{}},\"Italian\":{\"ColNum\":0,\"ClassLabel\":\"No\",\"Children\":{}}}},\"No\":{\"ColNum\":0,\"ClassLabel\":\"No\",\"Children\":{}}}},\"None\":{\"ColNum\":0,\"ClassLabel\":\"No\",\"Children\":{}}}}";

            return JsonConvert.DeserializeObject<TreeNode>(serializedDecisionTree);
        }
    }
}
