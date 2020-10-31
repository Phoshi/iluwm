using System;
using System.Windows.Controls;
using System.Windows.Input;

namespace Views.Bar
{
    public partial class TagDisplay : TwimeBarComponent
    {
        private readonly Action<string> _f;

        public TagDisplay(Action<string> f)
        {
            _f = f;
            InitializeComponent();
        }
        
        
        private void TreeTransform_Executed(object sender, ExecutedRoutedEventArgs e)
        {
            _f((string)e.Parameter);
        }

        private void TreeTransform_CanExecute(object sender, CanExecuteRoutedEventArgs e)
        {
            e.CanExecute = true;
        }
    }
}