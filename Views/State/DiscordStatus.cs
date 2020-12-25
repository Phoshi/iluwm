using System;
using System.IO;
using System.Windows;
using Newtonsoft.Json;

namespace Views.State
{
    public class DiscordStatus
    {
        private static Lazy<DiscordStatus> _self = new Lazy<DiscordStatus>(() => new DiscordStatus());
        public static DiscordStatus Instance => _self.Value;

        public delegate void DiscordStatusArgs(Status status);

        public event DiscordStatusArgs DiscordStatusChanged;

        private FileSystemWatcher _watcher;
        private string _path;
        
        public Status Status { get; internal set; } = new Status();

        private DiscordStatus()
        {
        }

        public void Register(string path)
        {
            if (path ==  _path) return;
            
            _path = path;
            var filename = Path.GetFileName(path);
            var directory = Path.GetDirectoryName(path);
            _watcher = new FileSystemWatcher
            {
                Path = directory,
                Filter = filename,
                EnableRaisingEvents = true,
            };
            
            _watcher.Changed += WatcherOnChanged;
        }

        private void UpdateFile()
        {
            if (File.Exists(_path))
            {
                var contents = File.ReadAllText(_path);
                if (string.IsNullOrEmpty(contents))
                {
                    //the plugin status writer seems to produce blank files sometimes :(
                    return;
                }

                Status = JsonConvert.DeserializeObject<Status>(contents);
            }
        }

        private void WatcherOnChanged(object sender, FileSystemEventArgs e)
        {
            try
            {
                UpdateFile();
                Application.Current.Dispatcher.Invoke(() => 
                    DiscordStatusChanged?.Invoke(Status));
            }
            catch (Exception exception)
            {
            }
        }
    }

    public class Status
    {
        public int Guild_Mention { get; set; }
        public string Voice_Channel { get; set; } = "";
        public string Microphone { get; set; } = "";
        public string Headphone { get; set; } = "";
    }
}