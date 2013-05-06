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

            Console.WriteLine("\nDone! Press any key to terminate.");
            Console.ReadKey(); 
        }

        private static string GetTestDataFilePath(string[] args)
        {
            if (args.Length != 1)
            {
                throw new ArgumentException("Usage: DecisionTreeClassifier.exe <test_data_file>");
            }

            var fileName = args[0];

            // if the file exists, great!
            if (File.Exists(fileName))
            {
                return fileName;
            }

            // otherwise it might be a relative path
            var assemblyLocation = Assembly.GetExecutingAssembly().Location;
            var assemblyDirectory = new FileInfo(assemblyLocation).DirectoryName ?? "";

            var fullPath = Path.Combine(assemblyDirectory, fileName);

            if (File.Exists(fullPath))
            {
                return fullPath;
            }

            throw new ArgumentException("Could not find file " + fileName);
        }

        private static TreeNode GetDecisionTree()
        {
            // the DecisionTreeLearner will modify this line to be a JSON serialized tree
            const string serializedDecisionTree = "{\"ColNum\":12,\"ClassLabel\":null,\"Children\":{\"4\":{\"ColNum\":0,\"ClassLabel\":null,\"Children\":{\"1\":{\"ColNum\":0,\"ClassLabel\":\"1\",\"Children\":{}},\"0\":{\"ColNum\":5,\"ClassLabel\":null,\"Children\":{\"0\":{\"ColNum\":0,\"ClassLabel\":\"3\",\"Children\":{}},\"1\":{\"ColNum\":7,\"ClassLabel\":null,\"Children\":{\"1\":{\"ColNum\":0,\"ClassLabel\":\"5\",\"Children\":{}},\"0\":{\"ColNum\":0,\"ClassLabel\":\"7\",\"Children\":{}}}}}}}},\"0\":{\"ColNum\":11,\"ClassLabel\":null,\"Children\":{\"0\":{\"ColNum\":7,\"ClassLabel\":null,\"Children\":{\"1\":{\"ColNum\":0,\"ClassLabel\":\"3\",\"Children\":{}},\"0\":{\"ColNum\":0,\"ClassLabel\":\"7\",\"Children\":{}}}},\"1\":{\"ColNum\":2,\"ClassLabel\":null,\"Children\":{\"0\":{\"ColNum\":0,\"ClassLabel\":\"1\",\"Children\":{}},\"1\":{\"ColNum\":0,\"ClassLabel\":\"4\",\"Children\":{}}}}}},\"2\":{\"ColNum\":0,\"ClassLabel\":null,\"Children\":{\"1\":{\"ColNum\":0,\"ClassLabel\":\"1\",\"Children\":{}},\"0\":{\"ColNum\":0,\"ClassLabel\":\"2\",\"Children\":{}}}},\"6\":{\"ColNum\":5,\"ClassLabel\":null,\"Children\":{\"0\":{\"ColNum\":0,\"ClassLabel\":\"6\",\"Children\":{}},\"1\":{\"ColNum\":0,\"ClassLabel\":\"7\",\"Children\":{}}}},\"8\":{\"ColNum\":0,\"ClassLabel\":\"7\",\"Children\":{}},\"5\":{\"ColNum\":0,\"ClassLabel\":\"7\",\"Children\":{}}}}";

            return JsonConvert.DeserializeObject<TreeNode>(serializedDecisionTree);
        }
    }
}
