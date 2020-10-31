using System.Windows.Input;

namespace Views.Commands
{
    public static class TreeTransformationCommands
    {
        public static readonly  RoutedUICommand Transform = new RoutedUICommand(
            "Transform",
            "Transform",
            typeof(TreeTransformationCommands));
        
    }
}