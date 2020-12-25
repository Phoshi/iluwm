using System;
using System.Windows.Threading;

namespace Views.State
{
    public class Clock
    {
        private static Lazy<Clock> _self = new Lazy<Clock>(() => new Clock());
        public static Clock Instance => _self.Value;

        public delegate void ClockTickArgs(Clock clock);

        public event ClockTickArgs ClockStatusChanged;

        private DispatcherTimer _timer;
        private DateTime _lastTime;

        private Clock()
        {
            _timer = new DispatcherTimer{Interval = TimeSpan.FromSeconds(1)};
            _timer.Tick += TimerOnTick;
            _timer.Start();
            
            _lastTime = DateTime.Now;
        }

        private void TimerOnTick(object? sender, EventArgs e)
        {
            if (_lastTime.Minute != DateTime.Now.Minute)
            {
                ClockStatusChanged?.Invoke(this);
            }
            _lastTime = DateTime.Now;
        }
        
        public DateTime Now => DateTime.Now;
    }
}