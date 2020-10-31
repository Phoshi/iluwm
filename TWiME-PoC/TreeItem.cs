using System;
using System.Collections.Generic;
using System.Linq;

namespace TWiME_PoC
{
    public interface ITreeItem
    {
        IContainer Parent { get; set; }
    }

    public interface IContainer : ITreeItem
    {
        void MoveWindow(MoveDirection fromDirection, WindowItem window, ITreeItem source = null);
        WindowItem Window(string name);
    }
    
    public class ContainerItem : ITreeItem, IContainer {
        public Direction Direction { get; set; }
        public List<ITreeItem> Nodes { get; set; }

        public ContainerItem(Direction direction, params ITreeItem[] nodes)
        {
            Direction = direction;
            Nodes = nodes.ToList();
            foreach (var node in Nodes)
            {
                node.Parent = this;
            }
        }
        
        public void MoveWindow(MoveDirection moveDirection, WindowItem window, ITreeItem source = null)
        {
            if (OurWindow(window))
            {
                if (CompatibleDirection(moveDirection))
                {
                    var position = Nodes.IndexOf(window);

                    if (Direction == Direction.Horizontal)
                    {
                        if (moveDirection == MoveDirection.Left && position == 0)
                        {
                            DeferToParent(moveDirection, window);
                            return;
                        }

                        if (moveDirection == MoveDirection.Right && position == (Nodes.Count - 1))
                        {
                            DeferToParent(moveDirection, window);
                            return;
                        }
                    }
                    
                    if (Direction == Direction.Vertical)
                    {
                        if (moveDirection == MoveDirection.Up && position == 0)
                        {
                            DeferToParent(moveDirection, window);
                            return;
                        }

                        if (moveDirection == MoveDirection.Down && position == (Nodes.Count - 1))
                        {
                            DeferToParent(moveDirection, window);
                            return;
                        }
                    }

                    var newPosition = position + (moveDirection == MoveDirection.Right || moveDirection == MoveDirection.Down ? 1 : -1);

                    var oldItem = Nodes[newPosition];

                    if (oldItem is IContainer c)
                    {
                        DeferTo(c, moveDirection, window);
                    }
                    else
                    {
                        Nodes[newPosition] = window;
                        Nodes[position] = oldItem;
                    }
                }
                else
                {
                    DeferToParent(moveDirection, window);
                }

                return;
            }

            var defaultIndex = 0;
            if (source != null)
            {
                defaultIndex = Nodes.IndexOf(source);
            }
            
            if (Direction == Direction.Horizontal)
            {
                if (moveDirection == MoveDirection.Left)
                {
                    defaultIndex -= 0;
                }
                else if (moveDirection == MoveDirection.Right)
                {
                    defaultIndex += 1;
                }
            }
            else
            {
                if (moveDirection == MoveDirection.Up)
                {
                    defaultIndex -= 0;
                }
                else if (moveDirection == MoveDirection.Down)
                {
                    defaultIndex += 1;
                }
            }

            var index = defaultIndex;
            if (index < 0) index = Nodes.Count - index;
            
            Nodes.Insert(index, window);

            window.Parent = this;
        }

        public WindowItem Window(string name)
        {
            foreach (var node in Nodes)
            {
                if (node is WindowItem wi)
                {
                    if (wi.Name == name) return wi;
                }

                if (node is IContainer c)
                {
                    return c.Window(name);
                }
            }
            
            throw new Exception();
        }

        private void DeferTo(IContainer container, MoveDirection moveDirection, WindowItem window)
        {
            Nodes.Remove(window);
            container.MoveWindow(moveDirection, window, this);
        }

        private void DeferToParent(MoveDirection d, WindowItem i) => DeferTo(Parent, d, i);

        public IContainer Parent { get; set; }

        private bool CompatibleDirection(MoveDirection d)
        {
            if (Direction == Direction.Horizontal)
            {
                return d == MoveDirection.Left || d == MoveDirection.Right;
            }

            return d == MoveDirection.Up || d == MoveDirection.Down;
        }

        private bool OurWindow(WindowItem i) => Nodes.Contains(i);
    }

    public class WindowItem : ITreeItem
    {
        public string Name { get; set; }

        public WindowItem(string name)
        {
            Name = name;
        }

        public IContainer Parent { get; set; }
    }

    public enum Direction
    {
        Horizontal,
        Vertical,
    }

    public enum MoveDirection
    {
        Left,
        Right,
        Up,
        Down,
    }
}