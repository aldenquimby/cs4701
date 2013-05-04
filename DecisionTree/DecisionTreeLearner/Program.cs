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
                var trainingData = DataSet.ConstructFromCsv(fileName);

                // run the learner
                var decisionTree = new Learner().Run(trainingData);

                // build the classifer
                var classifierExe = new ClassifierCompiler().CompileClassifier(decisionTree);
                
                // ouput result
                Console.WriteLine("Classify new data using this program: " + classifierExe);
            }
            catch (Exception e)
            {
                Console.WriteLine("OH NO, SOMETHING FAILED!");
                Console.WriteLine(e.Message);
            }

            Console.WriteLine("Done! Press any key to terminate.");
            Console.ReadKey(); 
        }

        private static string GetTrainingDataFilePath(string[] args)
        {
            var fileName = args.Length == 1 ? args[0] : "training_data.csv";

            var assemblyLocation = Assembly.GetExecutingAssembly().Location;
            var assemblyDirectory = new FileInfo(assemblyLocation).DirectoryName ?? "";

            return Path.Combine(assemblyDirectory, fileName);
        }
    }
}
