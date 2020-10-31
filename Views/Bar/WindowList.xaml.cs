using System;
using System.Windows.Controls;
using System.Windows.Input;

namespace Views.Bar
{
    public partial class WindowList : TwimeBarComponent
    {
        private readonly Action<object> _f;

        public WindowList(Action<object> f)
        {
            _f = f;
            InitializeComponent();
        }

        private void TreeTransform_Executed(object sender, ExecutedRoutedEventArgs e)
        {
            _f(e.Parameter);
        }

        private void TreeTransform_CanExecute(object sender, CanExecuteRoutedEventArgs e)
        {
            e.CanExecute = true;
        }
    }
}