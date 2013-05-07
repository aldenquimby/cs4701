using System.Collections.Generic;

namespace DecisionTree
{
    /// <summary>
    /// One attribute of a data set, uniquely identified by the column number
    /// </summary>
    public class DataAttribute
    {
        public DataAttribute(int colNum)
        {
            ColNum = colNum;
            Values = new HashSet<string>();
        }

        public int ColNum { get; private set; }
        public HashSet<string> Values { get; private set; }
    }
}