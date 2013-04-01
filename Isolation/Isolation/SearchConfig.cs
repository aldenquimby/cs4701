namespace Isolation
{
    public class SearchConfig
    {
        public SearchConfig(string input)
        {
            // defaults
            LoadHeuristicCacheFromDb = true;
            SaveHeuristicCacheToDb = true;
            SortMovesAsc = false;
            UseAlphaBeta = true;
            DepthLimit = 6;
            PercentTimeLeftForTimeout = 0.01;
            PercentTimeLeftToIncrementDepthLimit = 0.95;
            ReportStatistics = true;
            NumberOfThreads = 4;
            InterestingPercentScoreChange = 1.85;
            Heuristic = new NumberOfMovesHeuristic();

            // from input
            if ("1".Equals(input))
            {
                DepthLimit = 1;
                PercentTimeLeftToIncrementDepthLimit = 0.98;
                SortMovesAsc = true;
            }
        }

        // load pre-computed heuristic evaluation on game start
        public bool LoadHeuristicCacheFromDb { get; set; }

        // save heursitic evaluation on game end
        public bool SaveHeuristicCacheToDb { get; set; }
        
        // true for ascending, false for descending, null for no sorting
        public bool? SortMovesAsc { get; set; }

        // plain old minimix if false
        public bool UseAlphaBeta { get; set; }

        // how many plys to search
        public int DepthLimit { get; set; }

        // stop search execution if we're about to timeout
        public double PercentTimeLeftForTimeout { get; set; }

        // increment depth limit as the game plays out if more than this percent of time remains for any search
        public double PercentTimeLeftToIncrementDepthLimit { get; set; }

        // output search statistics
        public bool ReportStatistics { get; set; }

        // multi-threaded search?
        public int NumberOfThreads { get; set; }

        // quiessence search: extend depth if score changes by more than this percent, null for no quiessence
        public double? InterestingPercentScoreChange { get; set; }

        // heuristic evaluator to use when searching
        public HeuristicBase Heuristic { get; set; }

        // max number of nodes to quiessence expand
        public int MaxQuiessenceNodes { get; set; }
    }
}