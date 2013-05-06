using System;
using System.IO;
using System.Reflection;
using DecisionTree;

namespace DecisionTreeLearner
{
    public class Program
    {
        public static void Main(string[] args)
        {
            try
            {
                // construct training data
                var fileName = GetTrainingDataFilePath(args);
                var trainingData = DataSet.ConstructFromCsv(fileName, hasClassLabel:true);

                // run the learner
                var decisionTree = Learner.ConstructDecisionTree(trainingData);

                // build the classifer
                var classifierExe = Compiler.CompileClassifier(decisionTree);
                
                // ouput result
                Console.WriteLine("\nClassify new data using this program:");
                Console.WriteLine(classifierExe);
            }
            catch (Exception e)
            {
                Console.WriteLine("OH NO, SOMETHING FAILED!");
                Console.WriteLine(e.Message);
            }

            Console.WriteLine("\nDone! Press any key to terminate.");
            Console.ReadKey(); 
        }

        private static string GetTrainingDataFilePath(string[] args)
        {
            if (args.Length != 1)
            {
                throw new ArgumentException("Usage: DecisionTreeLearner.exe <training_data_file>");
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
    }
}
