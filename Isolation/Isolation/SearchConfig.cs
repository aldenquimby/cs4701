using System;

namespace Isolation
{
    public class SearchConfig
    {
        public SearchConfig(SearchConfig toCopy)
        {
            LoadHeuristicCacheFromDb = toCopy.LoadHeuristicCacheFromDb;
            SaveHeuristicCacheToDb = toCopy.SaveHeuristicCacheToDb;
            DepthLimit = toCopy.DepthLimit;
            PercentTimeLeftToIncrementDepthLimit = toCopy.PercentTimeLeftToIncrementDepthLimit;
            ReportStatistics = toCopy.ReportStatistics;
            QuiessenceSearch = toCopy.QuiessenceSearch;
            MoveTimeout = toCopy.MoveTimeout;
            GameMode = toCopy.GameMode;
            _beginningHeuristic = toCopy._beginningHeuristic;
            _middleHeuristic = toCopy._middleHeuristic;
            _endHeuristic = toCopy._endHeuristic;
        }

        public SearchConfig(string input)
        {
            // defaults
            LoadHeuristicCacheFromDb = false;
            SaveHeuristicCacheToDb = false;
            DepthLimit = 7;
            PercentTimeLeftToIncrementDepthLimit = 0.90;
            ReportStatistics = true;
            QuiessenceSearch = true;
            MoveTimeout = TimeSpan.FromSeconds(55); // TODO: get this from std in
            GameMode = GameMode.Beginning;
            _beginningHeuristic = new NumberOfMovesHeuristic();
            _middleHeuristic = new OpenAreaHeuristic();
            _endHeuristic = new LongestPathHeuristic();

            // from input
            if ("1".Equals(input))
            {
                QuiessenceSearch = false;
                _middleHeuristic = new NumberOfMovesHeuristic();
                //PercentTimeLeftToIncrementDepthLimit = 1;
                //SortMovesAsc = false;
            }
        }

        // maximum allowed time per move
        public TimeSpan MoveTimeout { get; set; }

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

        private readonly HeuristicBase _beginningHeuristic;
        private readonly HeuristicBase _middleHeuristic;
        private readonly HeuristicBase _endHeuristic;

        // heuristic evaluator to use when searching
        public HeuristicBase Heuristic
        {
            get
            {
                switch (GameMode)
                {
                    case GameMode.Beginning:
                        return _beginningHeuristic;
                    case GameMode.Middle:
                        return _middleHeuristic;
                    case GameMode.End:
                        return _endHeuristic;
                    default:
                        return _beginningHeuristic;
                }
            }
        }

        // beginning, middle, or end game
        public GameMode GameMode { get; set; }
    }
}