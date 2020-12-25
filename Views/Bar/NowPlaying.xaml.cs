using System;
using System.Windows;
using System.Windows.Threading;
using Windows.Media.Control;
using static Windows.Media.Control.GlobalSystemMediaTransportControlsSessionPlaybackStatus;

namespace Views.Bar
{
    public partial class NowPlaying : TwimeBarComponent
    {
        public NowPlaying()
        {
            InitializeComponent();
            
            State.NowPlaying.Instance.NowPlayingChanged += InstanceOnNowPlayingChanged;
            SetNowPlaying(State.NowPlaying.Instance);
        }

        private void InstanceOnNowPlayingChanged(State.NowPlaying sender)
        {
            SetNowPlaying(sender);
        }

        ~NowPlaying()
        {
            Close();
        }

        public override void Close()
        {
            State.NowPlaying.Instance.NowPlayingChanged -= InstanceOnNowPlayingChanged;
            base.Close();
        }

        private void SetNowPlaying(State.NowPlaying session)
        {
            try
            {
                var mediaProperties = session.NowPlayingSong();

                Application.Current.Dispatcher.Invoke(() =>
                    NowPlayingText.Text = $"{PlayPaused()} {mediaProperties.Artist} - {mediaProperties.Title}");
                
                string PlayPaused()
                {
                    return session.NowPlayingStatus().PlaybackStatus switch
                    {
                        Playing => "▶️",
                        Paused => "⏸️",
                        Stopped => "⏹️",
                        _ => ""
                    };
                }
            }
            catch (Exception e)
            {
                return;
            }
            
        }
    }
}