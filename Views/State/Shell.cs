using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Documents;
using System.Windows.Threading;

namespace Views.State
{
    public class Shell
    {
        private static Lazy<Shell> _self = new Lazy<Shell>(() => new Shell());
        public static Shell Instance => _self.Value;

        public delegate void ShellUpdateArgs(Shell clock, string command);

        public event ShellUpdateArgs ShellUpdate;

        private DispatcherTimer _timer;

        private IDictionary<string, string> _shellCommands =
            new Dictionary<string, string>();

        private Shell()
        {
            _timer = new DispatcherTimer{Interval = TimeSpan.FromSeconds(60)};
            _timer.Tick += TimerOnTick;
            _timer.Start();
        }

        private void TimerOnTick(object? sender, EventArgs e)
        {
            foreach (var command in _shellCommands.Keys)
            {
                var _result = Run(command);
                _result.ContinueWith(r =>
                {
                    var result = r.Result;
                    if (result != _shellCommands[command])
                    {
                        _shellCommands[command] = result;
                        Application.Current.Dispatcher.Invoke(() => 
                            ShellUpdate?.Invoke(this, command));
                    }
                });
            }
        }

        private Task<string> Run(string command)
        {
            try
            {
                ProcessStartInfo psi = new ProcessStartInfo();
                psi.FileName = "PowerShell.exe";
                psi.Arguments = command;
                psi.UseShellExecute = false;
                psi.RedirectStandardOutput = true;
                psi.RedirectStandardError = true;
                psi.CreateNoWindow = true;

                var process = new Process
                {
                    StartInfo = psi
                };

                return Task.Run(() =>
                {
                    process.Start();

                    string err = process.StandardError.ReadToEnd();

                    if (!string.IsNullOrEmpty(err))
                    {
                        return $"error: {err}";
                    }

                    var output = process.StandardOutput.ReadToEnd();

                    process.WaitForExit();

                    return output;
                });

            }
            catch (Exception e)
            {
                return Task.FromResult($"exn: {e.Message}");
            }
        }

        public string Result(string command)
        {
            if (!_shellCommands.ContainsKey(command))
            {
                _shellCommands[command] = "";
            }

            return _shellCommands[command];
        }
    }
}