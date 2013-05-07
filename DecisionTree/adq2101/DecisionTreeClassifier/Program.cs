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
            const string serializedDecisionTree = "{\"ColNum\":0,\"ClassLabel\":null,\"Children\":{\"Some\":{\"ColNum\":4,\"ClassLabel\":null,\"Children\":{\"$$$\":{\"ColNum\":0,\"ClassLabel\":\"Yes\",\"Children\":{}},\"$\":{\"ColNum\":2,\"ClassLabel\":null,\"Children\":{\"Yeah\":{\"ColNum\":0,\"ClassLabel\":\"Maybe\",\"Children\":{}},\"Nope\":{\"ColNum\":0,\"ClassLabel\":\"Yes\",\"Children\":{}}}},\"$$\":{\"ColNum\":0,\"ClassLabel\":\"Yes\",\"Children\":{}}}},\"Full\":{\"ColNum\":7,\"ClassLabel\":null,\"Children\":{\"0-10\":{\"ColNum\":0,\"ClassLabel\":\"No\",\"Children\":{}},\"30-60\":{\"ColNum\":0,\"ClassLabel\":\"Yes\",\"Children\":{}},\"10-30\":{\"ColNum\":0,\"ClassLabel\":\"Yes\",\"Children\":{}},\">60\":{\"ColNum\":2,\"ClassLabel\":null,\"Children\":{\"Yeah\":{\"ColNum\":4,\"ClassLabel\":null,\"Children\":{\"$$$\":{\"ColNum\":5,\"ClassLabel\":null,\"Children\":{\"Nope\":{\"ColNum\":0,\"ClassLabel\":\"No\",\"Children\":{}},\"Yeah\":{\"ColNum\":0,\"ClassLabel\":\"Maybe\",\"Children\":{}}}},\"$\":{\"ColNum\":0,\"ClassLabel\":\"No\",\"Children\":{}},\"$$\":{\"ColNum\":0,\"ClassLabel\":\"Maybe\",\"Children\":{}}}},\"Nope\":{\"ColNum\":0,\"ClassLabel\":\"Yes\",\"Children\":{}}}}}},\"None\":{\"ColNum\":0,\"ClassLabel\":\"No\",\"Children\":{}}}}";

            return JsonConvert.DeserializeObject<TreeNode>(serializedDecisionTree);
        }
    }
}
