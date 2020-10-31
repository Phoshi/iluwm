using System;
using System.Windows.Controls;
using System.Windows.Input;

namespace Views.Bar
{
    public partial class Button : TwimeBarComponent
    {
        private readonly Action _onClick;

        public Button(string text, Action onClick)
        {
            _onClick = onClick;
            InitializeComponent();

            Text.Text = text;
        }

        private void OnClick(object sender, ExecutedRoutedEventArgs e)
        {
            _onClick();
        }
    }
}