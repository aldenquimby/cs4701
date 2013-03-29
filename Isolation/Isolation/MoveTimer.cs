using System;
using System.Diagnostics;

namespace Isolation
{
    public class MoveTimer
    {
        #region singleton

        private static readonly Lazy<MoveTimer> Singleton = new Lazy<MoveTimer>(() => new MoveTimer());
        public static MoveTimer I { get { return Singleton.Value; }}
        
        #endregion
        
        private readonly Stopwatch _sw;
        private TimeSpan _timeout;

        public MoveTimer()
        {
            _sw = new Stopwatch();
            _timeout = TimeSpan.FromSeconds(10); // allow 10 seconds to calculate move
        }

        public void StartTimer()
        {
            _sw.Restart();
        }

        public TimeSpan GetTimeRemaining()
        {
            return _timeout.Subtract(_sw.Elapsed);
        }

        public double GetPercentOfTimeRemaining()
        {
            return ((double) (_timeout.Ticks - _sw.ElapsedTicks))/_timeout.Ticks;
        }
    }
}