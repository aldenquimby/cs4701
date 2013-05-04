using System.Collections.Generic;
using System.IO;

namespace DecisionTree
{
    public class DataAttribute
    {
        public DataAttribute(int colNum, string value)
        {
            ColNum = colNum;
            Value = value;
        }

        public int ColNum { get; private set; }
        public string Value { get; private set; }
    }

    public class DataItem
    {
        public DataItem()
        {
            Attributes = new List<DataAttribute>();
        }

        public IList<DataAttribute> Attributes { get; private set; } 
    }

    public class DataSet
    {
        public DataSet()
        {
            Items = new List<DataItem>();
        }

        public IList<DataItem> Items { get; set; } 

        public static DataSet ConstructFromCsv(string filePath)
        {
            var set = new DataSet();

            var contents = File.ReadAllText(filePath);
            var entries = contents.Split('\n');

            foreach (var entry in entries)
            {
                var item = new DataItem();

                var fields = entry.Split(',');

                for (var i = 0; i < fields.Length; i++)
                {
                    var value = fields[i].Trim();
                    var attribute = new DataAttribute(i, value);
                    item.Attributes.Add(attribute);
                }
            }

            return set;
        }
    }
}
