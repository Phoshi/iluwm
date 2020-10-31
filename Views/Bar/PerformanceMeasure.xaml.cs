using System;
using System.Windows.Controls;
using System.Windows.Threading;

namespace Views.Bar
{
    public partial class PerformanceMeasure : TwimeBarComponent
    {
        private readonly Func<string> _measure;
        private DispatcherTimer _timer;

        public PerformanceMeasure(Func<string> measure)
        {
            _measure = measure;
            InitializeComponent();
            
            _timer = new DispatcherTimer();
            _timer.Tick += TimerOnTick;
            _timer.Interval = TimeSpan.FromSeconds(1);
            _timer.Start();
        }

        ~PerformanceMeasure()
        {
            Close();
        }

        private void SetMeasure()
        {
            Measure.Text = _measure();
        }

        private void TimerOnTick(object? sender, EventArgs e)
        {
            SetMeasure();
        }

        public override void Close()
        {
            _timer.Stop();
            base.Close();
        }
    }
}