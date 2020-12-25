using System;
using System.Linq;
using System.Windows.Controls;
using System.Windows.Threading;

namespace Views.Bar
{
    public partial class Clock : TwimeBarComponent
    {
        private readonly string _format;
        public Clock(string format)
        {
            _format = format;
            InitializeComponent();
            
            State.Clock.Instance.ClockStatusChanged += ClockTick;
            
            SetTime(State.Clock.Instance);
        }

        private void ClockTick(State.Clock clock)
        {
            SetTime(clock);
        }

        ~Clock()
        {
            Close();
        }

        public override void Close()
        {
            State.Clock.Instance.ClockStatusChanged -= ClockTick;
            base.Close();
        }

        private void SetTime(State.Clock clock)
        {
            if (_format == "fuzzy")
            {
                Time.Text = Fuzzy(clock.Now);
            }
            else
            {
                Time.Text = clock.Now.ToString(_format);
            }
        }
        
        private string[] hours = new[]
        {
            "midnight",
            "one",
            "two",
            "three",
            "four",
            "five",
            "six",
            "seven",
            "eight",
            "nine",
            "ten",
            "eleven",
            "twelve",
            "one",
            "two",
            "three",
            "four",
            "five",
            "six",
            "seven",
            "eight",
            "nine",
            "ten",
            "eleven",
        };

        private (int cutoff, string template, bool useNextHour)[] times = new[]
        {
            (0, "{0}", false),
            (3, "Around {0}", false),
            (5, "Just after {0}", false),
            (9, "Coming up on ten past {0}", false),
            (10, "Ten past {0}", false),
            (14, "Nearly quarter past {0}", false),
            (15, "Quarter past {0}", false),
            (16, "Just after quarter past {0}", false),
            (19, "Coming up on twenty past {0}", false),
            (20, "Twenty past {0}", false),
            (23, "Just after twenty past {0}", false),
            (25, "Coming up on half past {0}", false),
            (29, "Nearly half past {0}", false),
            (30, "Half past {0}", false),
            (33, "Just after half past {0}", false),
            (35, "A little after half past {0}", false),
            (39, "Almost twenty to {0}", true),
            (40, "Twenty to {0}", true),
            (42, "Nearly quarter to {0}", true),
            (44, "About quarter to {0}", true),
            (45, "Quarter to {0}", true),
            (48, "Just after quarter to {0}", true),
            (50, "Ten to {0}", true),
            (55, "Coming up on {0}", true),
            (59, "Just before {0}", true),
        };

            private string Fuzzy(DateTime now)
        {
            var hour = now.Hour;
            var minute = now.Minute;

            var template = times
                .First(t => t.cutoff >= minute);

            return string.Format(template.template,
                template.useNextHour ? hours[(hour + 1) % hours.Length] : hours[hour]);
        }
    }
}