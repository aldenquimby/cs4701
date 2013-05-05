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

        public static DataSet ConstructFromCsv(string filePath)
        {
            var set = new DataSet();

            var contents = File.ReadAllText(filePath);
            var entries = contents.Split(new[] { '\n' }, StringSplitOptions.RemoveEmptyEntries);

            var attrCount = 0;

            foreach (var entry in entries)
            {
                var example = new Example();

                var fields = entry.Split(new[] { ',' }, StringSplitOptions.RemoveEmptyEntries);

                // make sure that all entries have the same number of attributes
                if (attrCount == 0)
                {
                    attrCount = fields.Length;
                }
                else if (attrCount != fields.Length)
                {
                    throw new Exception("Invalid CSV entry, wrong number of fields.");    
                }

                // add all attributes to example
                for (var i = 0; i < fields.Length - 1; i++)
                {
                    var value = fields[i].Trim();
                    
                    example.Attributes[i] = value;

                    if (set.Attributes.Count == i)
                    {
                        set.Attributes[i] = new DataAttribute(i);
                    }
                    set.Attributes[i].Values.Add(value);
                }

                // class label is always the last attribute
                example.ClassLabel = fields[fields.Length - 1].Trim();

                set.Examples.Add(example);
            }

            return set;
        }
    }
}
