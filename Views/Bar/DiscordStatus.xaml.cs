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
using Newtonsoft.Json;
using Views.State;

namespace Views.Bar
{
    public partial class DiscordStatus : TwimeBarComponent
    {
        private readonly bool _notify;
        private readonly int _notifyDuration;
        private DateTime? _lastNotification;
        private Status _lastStatus;
        
        public DiscordStatus(string path, bool notify, int notifyDuration)
        {
            _notify = notify;
            _notifyDuration = notifyDuration;
            InitializeComponent();
            
            State.DiscordStatus.Instance.Register(Environment.ExpandEnvironmentVariables(path));
            State.DiscordStatus.Instance.DiscordStatusChanged += OnStatusUpdate;
            SetStatus(State.DiscordStatus.Instance.Status);
        }

        private void OnStatusUpdate(Status status)
            => SetStatus(status);

        ~DiscordStatus()
        {
            Close();
        }

        public override void Close()
        {
            State.DiscordStatus.Instance.DiscordStatusChanged -= OnStatusUpdate;
            base.Close();
        }

        private void SetStatus(Status status)
        {
            UpdateNotificationTime(status);

            Status.Text = TextFor(status);
            var (fore, back) = ColoursFor(status);
            this.Background = back;
            this.Foreground = fore;

            if (string.IsNullOrEmpty(Status.Text))
            {
                this.Padding = new Thickness(0);
            }
            else
            {
                this.Padding = new Thickness(5);
            }
        }

        private void UpdateNotificationTime(Status status)
        {
            if (_lastStatus == null)
            {
                _lastStatus = status;
                return;
            }

            if (status.Guild_Mention > _lastStatus.Guild_Mention)
            {
                _lastNotification = DateTime.UtcNow;
            }

            _lastStatus = status;
        }

        private (Brush, Brush) ColoursFor(Status status)
        {
            var notificationBrush = B("#ebcb8b");
            var normal = B("#d8dee9");
            var inverted = B("#2e3440");

            if (_lastNotification > DateTime.UtcNow.AddSeconds(-_notifyDuration) && _notify)
            {
                if (DateTime.UtcNow.Second % 2 == 0)
                {
                    return (inverted, notificationBrush);
                }
            }
            
            if (string.IsNullOrEmpty(status.Voice_Channel))
            {
                return (normal, B("#2e3440"));
            }

            if (status.Microphone == "Muted")
            {
               return (normal, B("#d08770"));
            }
            
            return (normal, B("#2e3440"));

            Brush B(string hex) => 
                (Brush)new BrushConverter().ConvertFrom(hex);
        }

        private string TextFor(Status status)
        {
            if (string.IsNullOrEmpty(status.Voice_Channel))
            {
                if (status.Guild_Mention > 0)
                {
                    return $"{MessageCount()} DM{(status.Guild_Mention == 1 ? "" : "s")}";
                }

                return string.Empty;
            }

            return $"{Muted()}{status.Voice_Channel}{(status.Guild_Mention > 0 ? $" ({MessageCount()})" : string.Empty)}";

            string MessageCount() =>
                status.Guild_Mention > 0 ? $"{status.Guild_Mention}" : string.Empty;

            string Muted() => status.Microphone == "Muted" ? "🔇" : "🔊";
        }
    }
}