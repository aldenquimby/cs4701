using System;
using System.CodeDom.Compiler;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Microsoft.Build.Evaluation;
using Microsoft.Build.Framework;
using Microsoft.Build.Logging;
using Microsoft.Build.Utilities;
using Microsoft.CSharp;

namespace DecisionTreeLearner
{
    public class BuildLogger : Logger
    {
        public IList<BuildErrorEventArgs> Errors = new List<BuildErrorEventArgs>();

        public override void Initialize(IEventSource eventSource)
        {
            if (eventSource != null)
            {
                eventSource.ErrorRaised += OnBuildError;
            }
        }

        private void OnBuildError(object sender, BuildErrorEventArgs e)
        {
            Errors.Add(e);
        }
    }

    public class Compiler
    {
        public string CompileProject(string projectFile, string outputPath)
        {
            if (!File.Exists(projectFile))
            {
                return null;
            }

            var proj = new Project(projectFile);

            // set project to compile special DecisionTree config
            proj.SetProperty("Configuration", "DecisionTree");

            // set the output path
            proj.SetProperty("DecisionTreeOutputPath", outputPath);

            var logger = new BuildLogger();

            if (!proj.Build(logger))
            {
                Console.WriteLine("****************************");
                Console.WriteLine("**** Failed to compile! ****");
                Console.WriteLine("****************************");

                foreach (var error in logger.Errors)
                {
                    Console.WriteLine(error.Message + " " + error.File + ": " + error.LineNumber);
                }

                return null;
            }

            return outputPath;
        }

        public string CompileProgramFromFolder(string dirName, string outputName)
        {
            if (!Directory.Exists(dirName))
            {
                return null;
            }

            var dirInfo = new DirectoryInfo(dirName);
            var fileNames = dirInfo.GetFiles()
                                   .Where(x => x.Extension.Equals(".cs"))
                                   .Select(x => x.FullName)
                                   .ToArray();

            var referenceAssemblies = new[] {"System"};

            var compileParams = new CompilerParameters(referenceAssemblies, outputName)
                {
                    GenerateExecutable = true,
                    MainClass = "Program",
                };

            var codeProvider = new CSharpCodeProvider();

            var compilerResult = codeProvider.CompileAssemblyFromFile(compileParams, fileNames);

            if (compilerResult.Errors.Count > 0)
            {
                Console.WriteLine("****************************");
                Console.WriteLine("**** Failed to compile! ****");
                Console.WriteLine("****************************");

                foreach (CompilerError error in compilerResult.Errors)
                {
                    Console.WriteLine(error.ErrorText + " " + error.FileName + ": " + error.Line);
                }

                return null;
            }

            return compilerResult.PathToAssembly;
        }
    }
}
