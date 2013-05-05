using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using DecisionTree;
using Microsoft.Build.Evaluation;
using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;
using Newtonsoft.Json;

namespace DecisionTreeLearner
{
    /// <summary>
    /// Utility for building C# projects
    /// </summary>
    public class Compiler
    {
        /// <summary>
        /// Creates a DecisionTreeClassifier.exe from a decision tree, returning the file path of the new exe
        /// </summary>
        public static string CompileClassifier(TreeNode decisionTree)
        {
            var assemblyLocation = Assembly.GetExecutingAssembly().Location;
            var assemblyDirectory = new FileInfo(assemblyLocation).DirectoryName ?? "";

            var classifierDir = Path.Combine(assemblyDirectory, "DecisionTreeClassifier");

            if (!Directory.Exists(classifierDir))
            {
                // TODO remove this debug
                classifierDir = Path.Combine(new DirectoryInfo(assemblyDirectory).Parent.Parent.Parent.FullName, "DecisionTreeClassifier");
            }

            var projFullPath = Path.Combine(classifierDir, "DecisionTreeClassifier.csproj");
            var mainFullPath = Path.Combine(classifierDir, "Program.cs");

            ReplaceSerializedTreeLine(mainFullPath, decisionTree);

            // load up the classifier project
            var proj = new Project(projFullPath);

            // set project to compile special DecisionTree config
            proj.SetProperty("Configuration", "DecisionTree");

            // set the output path
            proj.SetProperty("DecisionTreeOutputPath", assemblyDirectory);

            // make a logger to catch possible build errors
            var logger = new SimpleBuildLogger();

            // if we failed to build the classifier, we're screwed
            if (!proj.Build(logger))
            {
                var sb = new StringBuilder();
                sb.AppendLine("***************************************");
                sb.AppendLine("**** Failed To Compile Classifier! ****");
                sb.AppendLine("***************************************");
                foreach (var error in logger.Errors)
                {
                    sb.AppendLine(error.Message + " " + error.File + ": " + error.LineNumber);
                }
                throw new Exception(sb.ToString());
            }

            // return the executable name
            var exeFileName = proj.GetProperty("AssemblyName").EvaluatedValue + ".exe";
            return Path.Combine(assemblyDirectory, exeFileName);
        }

        /// <summary>
        /// Serializes the decision tree and writes it into the classifier main program
        /// </summary>
        private static void ReplaceSerializedTreeLine(string mainFilePath, TreeNode decisionTree)
        {
            // double serialize the tree to escape " characters
            var serializedTree = JsonConvert.SerializeObject(JsonConvert.SerializeObject(decisionTree));

            // find the line in the file where we should stick the serialized tree
            const string serializedTreeLine = @"const string serializedDecisionTree = ";

            var fileContents = File.ReadAllText(mainFilePath);
            var lineToReplace = fileContents.Split(new[] { "\r\n" }, StringSplitOptions.None).Single(x => x.Contains(serializedTreeLine));

            // stick the serialized tree in that line
            var newLine = lineToReplace.Substring(0, lineToReplace.IndexOf(serializedTreeLine) + serializedTreeLine.Length) + serializedTree + ";";
            var newContents = fileContents.Replace(lineToReplace, newLine);
            File.WriteAllText(mainFilePath, newContents);
        }
    }

    /// <summary>
    /// Catches build errors when trying to compile a C# project
    /// </summary>
    public class SimpleBuildLogger : Logger
    {
        public IList<BuildErrorEventArgs> Errors = new List<BuildErrorEventArgs>();

        public override void Initialize(IEventSource eventSource)
        {
            if (eventSource != null)
            {
                eventSource.ErrorRaised += (sender, eventArgs) => Errors.Add(eventArgs);
            }
        }
    }
}
