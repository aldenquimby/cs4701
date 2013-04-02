namespace Isolation
{
    public class SearchConfig
    {
        public SearchConfig(string input)
        {
            // defaults
            LoadHeuristicCacheFromDb = false;
            SaveHeuristicCacheToDb = false;
            DepthLimit = 5;
            PercentTimeLeftToIncrementDepthLimit = 1;
            ReportStatistics = true;
            InterestingPercentScoreChange = 7.5;
            Heuristic = new NumberOfMovesHeuristic();

            // from input
            if ("1".Equals(input))
            {
                DepthLimit = 5;
                InterestingPercentScoreChange = null;
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

        // quiessence search: extend depth if score changes by more than this percent, null for no quiessence
        public double? InterestingPercentScoreChange { get; set; }

        // heuristic evaluator to use when searching
        public HeuristicBase Heuristic { get; set; }
    }
}