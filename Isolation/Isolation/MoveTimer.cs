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
            _timeout = TimeSpan.FromSeconds(50); // allowed time to calculate move
        }

        public void StartTimer()
        {
            _sw.Restart();
        }

        public TimeSpan GetTimeElapsed()
        {
            return _sw.Elapsed;
        }

        public double GetPercentOfTimeRemaining()
        {
            return (_timeout.TotalMilliseconds - _sw.ElapsedMilliseconds) / _timeout.TotalMilliseconds;
        }
    }
}