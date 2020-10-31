using System;
using System.IO;
using System.Linq;
using System.Text.Json.Serialization;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Controls.Primitives;
using System.Windows.Media;
using System.Windows.Media.TextFormatting;
using System.Windows.Threading;
using Windows.Media.Control;
// using Windows.Media.Control;
using Newtonsoft.Json;
using static Windows.Media.Control.GlobalSystemMediaTransportControlsSessionPlaybackStatus;

namespace Views.Bar
{
    public partial class NowPlaying : TwimeBarComponent
    {
        private DispatcherTimer _timer;
        private static GlobalSystemMediaTransportControlsSession _gsmtcsm;

        public NowPlaying()
        {
            InitializeComponent();
            
            _timer = new DispatcherTimer();
            _timer.Tick += TimerOnTick;
            _timer.Interval = TimeSpan.FromSeconds(0.5);
            _timer.Start();
            SetNowPlaying();
        }

        ~NowPlaying()
        {
            Close();
        }

        public override void Close()
        {
            _timer.Stop();
            base.Close();
        }

        private void SetNowPlaying()
        {
            try
            {
                if (_gsmtcsm == null)
                {
                    _gsmtcsm = GlobalSystemMediaTransportControlsSessionManager.RequestAsync().GetAwaiter().GetResult()
                        .GetCurrentSession();
                }

                var mediaProperties = _gsmtcsm.TryGetMediaPropertiesAsync().GetAwaiter().GetResult();
                
                NowPlayingText.Text = $"{PlayPaused()} {mediaProperties.Artist} - {mediaProperties.Title}";
                
                string PlayPaused()
                {
                    return _gsmtcsm.GetPlaybackInfo().PlaybackStatus switch
                    {
                        Playing => "▶️",
                        Paused => "⏸️",
                        Stopped => "⏹️",
                        _ => ""
                    };
                }
            }
            catch
            {
                return;
            }
            
        }


        private void TimerOnTick(object? sender, EventArgs e)
        {
            SetNowPlaying();
        }
    }
}