using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace TWiME_PoC
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
            var tree = Tree();
            
            Move(tree, "Firefox", MoveDirection.Down);
            
            RenderTree(tree);
        }

        private ITreeItem Tree()
        {
            return new ContainerItem(Direction.Horizontal,
                new WindowItem("Rider"),
                new ContainerItem(Direction.Vertical,
                    new WindowItem("Firefox"),
                    new WindowItem("Explorer")));
        }

        private void Move(ITreeItem item, string windowName, MoveDirection direction)
        {
            if (item is IContainer c)
            {
                var window = c.Window(windowName);
                window.Parent.MoveWindow(direction, window);
            }
        }
        

        private void RenderTree(ITreeItem tree, int left = 0, int top = 0, int right = 512, int bottom = 288)
        {
            if (tree is WindowItem wi)
            {
                DrawBox(left, top, right, bottom, wi.Name);
            }
            else if (tree is ContainerItem ci)
            {
                if (ci.Direction == Direction.Horizontal)
                {
                    var chunkSize = (right - left) / ci.Nodes.Count;

                    var effectiveLeft = left;
                    
                    foreach (var node in ci.Nodes)
                    {
                        RenderTree(node, effectiveLeft, top, chunkSize, bottom);
                        effectiveLeft += chunkSize;
                    }
                } 
                if (ci.Direction == Direction.Vertical)
                {
                    var chunkSize = (bottom - top) / ci.Nodes.Count;

                    var effectiveTop = top;
                    
                    foreach (var node in ci.Nodes)
                    {
                        RenderTree(node, left, effectiveTop, right, chunkSize);
                        effectiveTop += chunkSize;
                    }
                } 
            }
        }

        private void DrawBox(int x, int y, int width, int height, string name)
        {
            System.Windows.Shapes.Rectangle rect;
            rect = new System.Windows.Shapes.Rectangle();
            rect.Stroke = new SolidColorBrush(Colors.Black);
            rect.StrokeThickness = 1;
            rect.Width = width;
            rect.Height = height;
            Canvas.SetLeft(rect, x);
            Canvas.SetTop(rect, y);
            drawing.Children.Add(rect);
            
            TextBlock textBlock = new TextBlock();
            textBlock.Text = name;
            textBlock.Foreground = new SolidColorBrush(Colors.Black);
            Canvas.SetLeft(textBlock, x);
            Canvas.SetTop(textBlock, y);
            drawing.Children.Add(textBlock);
        }
    }
}