using System;

namespace Isolation
{
    public class SearchConfig
    {
        public SearchConfig(SearchConfig toCopy)
        {
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

        public SearchConfig(string configInput)
        {
            var parts = configInput.Split(',');
            MoveTimeout = TimeSpan.FromSeconds(int.Parse(parts[0]));

            // defaults
            ReportStatistics = true;
            QuiessenceSearch = true;
            GameMode = GameMode.Beginning;
            _beginningHeuristic = new NumberOfMovesHeuristic();
            _middleHeuristic = new OpenAreaHeuristic();
            _endHeuristic = new LongestPathHeuristic();

            // timeout dependent config
            if (MoveTimeout.TotalSeconds > 45)
            {
                DepthLimit = 8;
                PercentTimeLeftToIncrementDepthLimit = 0.85;
            }
            else
            {
                DepthLimit = 7;
                PercentTimeLeftToIncrementDepthLimit = 0.92;
            }

            // allow depth limit to be entered manually
            int depthLimitFromInput;
            if (parts.Length > 1 && int.TryParse(parts[1], out depthLimitFromInput))
            {
                DepthLimit = depthLimitFromInput;
            }

            // TODO: remove this
            if ("59".Equals(configInput))
            {
                DepthLimit = 7;
                QuiessenceSearch = false;
                _middleHeuristic = new NumberOfMovesHeuristic();
                //PercentTimeLeftToIncrementDepthLimit = 1;
                //SortMovesAsc = false;
            }
        }

        // maximum allowed time per move
        public TimeSpan MoveTimeout { get; set; }

        // how many plys to search
        public int DepthLimit { get; set; }

        // increment depth limit as the game plays out if more than this percent of time remains for any search
        public double PercentTimeLeftToIncrementDepthLimit { get; set; }

        // output search statistics
        public bool ReportStatistics { get; set; } // TODO: remove this

        // quiessence search: extend depth if nodes look interesting
        public bool QuiessenceSearch { get; set; } // TODO: remove this

        // beginning, middle, or end game
        public GameMode GameMode { get; set; }

        // heuristic evaluator to use when searching, depends on game mode
        private readonly HeuristicBase _beginningHeuristic;
        private readonly HeuristicBase _middleHeuristic;
        private readonly HeuristicBase _endHeuristic;
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
    }
}