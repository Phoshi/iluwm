using System;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Threading;

namespace Views.Bar
{
    public partial class Shell : TwimeBarComponent
    {
        private readonly string _command;

        public Shell(string command)
        {
            _command = command;
            InitializeComponent();
            
            State.Shell.Instance.ShellUpdate += InstanceOnShellUpdate;

            SetText(State.Shell.Instance.Result(command));
        }

        private void InstanceOnShellUpdate(State.Shell clock, string command)
        {
            if (command == _command)
            {
                var result = clock.Result(command);
                SetText(result);
            }
        }

        private void SetText(string text)
        {
            if (string.IsNullOrEmpty(text))
            {
                this.Padding = new Thickness(0);
            }
            else
            {
                this.Padding = new Thickness(5);
            }
            
            ShellOutput.Text = text;
        }

        ~Shell()
        {
            Close();
        }

        public override void Close()
        {
            State.Shell.Instance.ShellUpdate -= InstanceOnShellUpdate;
            base.Close();
        }
    }
}