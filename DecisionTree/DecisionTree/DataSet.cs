using System;
using System.Collections.Generic;
using System.IO;

namespace DecisionTree
{
    /// <summary>
    /// A set of data (either training or test) with examples
    /// </summary>
    public class DataSet
    {
        public DataSet()
        {
            Examples = new List<Example>();
            Attributes = new List<DataAttribute>();
        }

        public IList<Example> Examples { get; set; }
        public IList<DataAttribute> Attributes { get; set; } 

        public static DataSet ConstructFromCsv(string filePath, bool hasClassLabel)
        {
            var set = new DataSet();

            var contents = File.ReadAllText(filePath);
            var entries = contents.Split(new[] { '\n' }, StringSplitOptions.RemoveEmptyEntries);

            var correctAttrCount = 0;

            foreach (var entry in entries)
            {
                var example = new Example();

                var fields = entry.Split(new[] { ',' }, StringSplitOptions.RemoveEmptyEntries);

                // if we have a class label, the last field is not an attribute
                var attrCount = hasClassLabel ? fields.Length - 1 : fields.Length;

                // make sure that all entries have the same number of attributes
                if (correctAttrCount == 0)
                {
                    correctAttrCount = attrCount;
                }
                else if (correctAttrCount != attrCount)
                {
                    throw new Exception("Invalid CSV entry, wrong number of attributes.");    
                }

                // add all attributes to example
                for (var i = 0; i < attrCount; i++)
                {
                    var value = fields[i].Trim();
                    
                    example.Attributes[i] = value;

                    if (set.Attributes.Count == i)
                    {
                        set.Attributes.Add(new DataAttribute(i));
                    }
                    set.Attributes[i].Values.Add(value);
                }

                // add class label if we have it
                if (hasClassLabel)
                {
                    example.ClassLabel = fields[attrCount].Trim();
                }

                set.Examples.Add(example);
            }

            return set;
        }
    }
}
