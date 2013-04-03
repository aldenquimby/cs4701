namespace Isolation
{
    public class SearchConfig
    {
        public SearchConfig(string input)
        {
            //TODO: input should be timeout in seconds

            // defaults
            LoadHeuristicCacheFromDb = false;
            SaveHeuristicCacheToDb = false;
            DepthLimit = 7;
            PercentTimeLeftToIncrementDepthLimit = 0.92;
            ReportStatistics = true;
            QuiessenceSearch = true;
            Heuristic = new NumberOfMovesHeuristic();

            // from input
            if ("1".Equals(input))
            {
                DepthLimit = 5;
                QuiessenceSearch = false;
                //PercentTimeLeftToIncrementDepthLimit = 1;
                //SortMovesAsc = false;
            }
        }

        // load pre-computed heuristic evaluation on game start
        public bool LoadHeuristicCacheFromDb { get; set; }

        // save heursitic evaluation on game end
        public bool SaveHeuristicCacheToDb { get; set; }
        
        // how many plys to search
        public int DepthLimit { get; set; }

        // increment depth limit as the game plays out if more than this percent of time remains for any search
        public double PercentTimeLeftToIncrementDepthLimit { get; set; }

        // output search statistics
        public bool ReportStatistics { get; set; }

        // quiessence search: extend depth if nodes look interesting
        public bool QuiessenceSearch { get; set; }

        // heuristic evaluator to use when searching
        public HeuristicBase Heuristic { get; set; }
    }
}