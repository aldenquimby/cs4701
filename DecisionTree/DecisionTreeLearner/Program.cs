using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using DecisionTree;
using Newtonsoft.Json;

namespace DecisionTreeLearner
{
    public class Program
    {
        public static void Main(string[] args)
        {
            const string classifierDir = @"C:\src\columbia\cs4701\DecisionTree\DecisionTreeClassifier";
            const string projFileName = @"DecisionTreeClassifier.csproj";
            const string mainFileName = @"Program.cs";

            var projFullPath = Path.Combine(classifierDir, projFileName);
            var mainFullPath = Path.Combine(classifierDir, mainFileName);

            var tree = RandomTree();
            var serTree = JsonConvert.SerializeObject(tree, Formatting.None);
            serTree = JsonConvert.SerializeObject(serTree, Formatting.None); // double serialize

            const string toReplace = @"public static string SerializedDecisionTree = ";
            const string replaceWith = @"public static string SerializedDecisionTree = {0};";

            var fileContents = File.ReadAllText(mainFullPath);
            var lineToReplace = fileContents.Split(new []{"\r\n"}, StringSplitOptions.None).Single(x => x.Contains(toReplace));
            var newLine = lineToReplace.Substring(0, lineToReplace.IndexOf(toReplace) + toReplace.Length) + serTree + ";";
            var newContents = fileContents.Replace(lineToReplace, newLine);
            File.WriteAllText(mainFullPath, newContents);

            var compiler = new Compiler();

            var thisExeFilePath = Assembly.GetExecutingAssembly().Location;
            var thisExeDir = new FileInfo(thisExeFilePath).DirectoryName;

            var newExePath = compiler.CompileProject(projFullPath, thisExeDir);

            Console.WriteLine("Compiled to here: " + newExePath);
            Console.ReadKey();
        }

        private static Tree RandomTree()
        {
            return new Tree
                {
                    Name = "Parent",
                    Children = new List<Tree>
                        {
                            new Tree {Name = "Child1"},
                            new Tree {Name = "Child2"},
                        }
                };
        }
    }
}
