using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;

namespace Views.Bar
{
    public partial class TrayButton : TwimeBarComponent
    {
        private readonly Action<Point> _onClick;

        public TrayButton(string text, Action<Point> onClick)
        {
            _onClick = onClick;
            InitializeComponent();

            Text.Text = text;
        }

        private void OnClick(object sender, ExecutedRoutedEventArgs e)
        {
            Point position;
            try
            {
                position = PointToScreen(new Point(0, ActualHeight));
            }
            catch (Exception exception)
            {
                Console.WriteLine(exception);
            }
            
            _onClick(position);
        }
    }
}