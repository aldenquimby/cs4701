using System.Collections.Generic;

namespace DecisionTree
{
    /// <summary>
    /// One example (or row) of a data set, with attribute values and the class label
    /// </summary>
    public class Example
    {
        public Example()
        {
            Attributes = new Dictionary<int, string>();
        }

        public string ClassLabel { get; set; }
        public IDictionary<int, string> Attributes { get; set; }
    }
}