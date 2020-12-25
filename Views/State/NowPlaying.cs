using System;
using Windows.Media.Control;

namespace Views.State
{
    public class NowPlaying
    {
        private static Lazy<NowPlaying> _self = new Lazy<NowPlaying>(() => new NowPlaying());
        public static NowPlaying Instance => _self.Value;

        public delegate void NowPlayingArgs(NowPlaying sender);

        public event NowPlayingArgs NowPlayingChanged;
        
        private GlobalSystemMediaTransportControlsSession _currentSession;
        private GlobalSystemMediaTransportControlsSessionMediaProperties _currentSong;
        private GlobalSystemMediaTransportControlsSessionPlaybackInfo _playbackInfo;

        private NowPlaying()
        {
            var manager = GlobalSystemMediaTransportControlsSessionManager.RequestAsync().GetAwaiter().GetResult();
            RegisterSession(manager.GetCurrentSession());
            
            manager.CurrentSessionChanged += ManagerOnCurrentSessionChanged;
        }

        private void RaiseEvent()
        {
            NowPlayingChanged?.Invoke(this);
        }

        private void ManagerOnCurrentSessionChanged(GlobalSystemMediaTransportControlsSessionManager sender, CurrentSessionChangedEventArgs args)
        {
            _currentSession.MediaPropertiesChanged -= CurrentSessionOnMediaPropertiesChanged;
            _currentSession.PlaybackInfoChanged -= CurrentSessionOnPlaybackInfoChanged;

            var newSession = sender.GetCurrentSession();

            if (newSession != null)
            {
                RegisterSession(newSession);
                RaiseEvent();
            }
        }

        private void RegisterSession(GlobalSystemMediaTransportControlsSession session)
        {
            if (session == null)
                return;
            _currentSession = session;
            
            _currentSession.MediaPropertiesChanged += CurrentSessionOnMediaPropertiesChanged;
            _currentSession.PlaybackInfoChanged += CurrentSessionOnPlaybackInfoChanged;

            _currentSong = _currentSession.TryGetMediaPropertiesAsync().GetAwaiter().GetResult();
            _playbackInfo = _currentSession.GetPlaybackInfo();
        }

        private void CurrentSessionOnPlaybackInfoChanged(GlobalSystemMediaTransportControlsSession sender, PlaybackInfoChangedEventArgs args)
        {
            _playbackInfo = sender.GetPlaybackInfo();
            RaiseEvent();
        }

        private void CurrentSessionOnMediaPropertiesChanged(GlobalSystemMediaTransportControlsSession sender, MediaPropertiesChangedEventArgs args)
        {
            _currentSong = sender.TryGetMediaPropertiesAsync().GetAwaiter().GetResult();
            RaiseEvent();
        }

        public GlobalSystemMediaTransportControlsSessionPlaybackInfo NowPlayingStatus() => _playbackInfo;

        public GlobalSystemMediaTransportControlsSessionMediaProperties NowPlayingSong() => _currentSong;
    }
}