using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Linq;
using System.Runtime.InteropServices;
using System.Windows;
using System.Windows.Interop;

namespace Views.Bar
{
    public partial class Bar : Window
    {
        private ObservableCollection<TwimeBarComponent> Items = new ObservableCollection<TwimeBarComponent>();

        public Bar()
        {
            InitializeComponent();

            BarItems.ItemsSource = Items;

            this.Closing += (c, e) => CloseComponents();
        }

        private void CloseComponents()
        {
            foreach (var item in Items)
            {
                item.Close();
            }
        }

        public void SetItems(IEnumerable<TwimeBarComponent> elems)
        {
            var twimeBarComponents = elems.ToList();
            
            foreach (var component in twimeBarComponents)
            {
                component.Attached(this);
            }

            CloseComponents();

            Items = new ObservableCollection<TwimeBarComponent>(twimeBarComponents);
            BarItems.ItemsSource = Items;
        }

        private void Bar_OnLoaded(object sender, RoutedEventArgs e)
        {
            var wndHelper = new WindowInteropHelper(this);

            var exStyle = (int) GetWindowLong(wndHelper.Handle, (int) GetWindowLongFields.GWL_EXSTYLE);

            exStyle |= (int) ExtendedWindowStyles.WS_EX_TOOLWINDOW;
            SetWindowLong(wndHelper.Handle, (int) GetWindowLongFields.GWL_EXSTYLE, (IntPtr) exStyle);

            SetWindowPos(wndHelper.Handle, (IntPtr) HWND_BOTTOM, 0, 0, 0, 0, SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE);
        }

        [Flags]
        public enum ExtendedWindowStyles
        {
            // ...
            WS_EX_TOOLWINDOW = 0x00000080,
            // ...
        }

        public enum GetWindowLongFields
        {
            // ...
            GWL_EXSTYLE = (-20),
            // ...
        }

        [DllImport("user32.dll")]
        public static extern IntPtr GetWindowLong(IntPtr hWnd, int nIndex);

        public static IntPtr SetWindowLong(IntPtr hWnd, int nIndex, IntPtr dwNewLong)
        {
            int error = 0;
            IntPtr result = IntPtr.Zero;
            // Win32 SetWindowLong doesn't clear error on success
            SetLastError(0);

            if (IntPtr.Size == 4)
            {
                // use SetWindowLong
                Int32 tempResult = IntSetWindowLong(hWnd, nIndex, IntPtrToInt32(dwNewLong));
                error = Marshal.GetLastWin32Error();
                result = new IntPtr(tempResult);
            }
            else
            {
                // use SetWindowLongPtr
                result = IntSetWindowLongPtr(hWnd, nIndex, dwNewLong);
                error = Marshal.GetLastWin32Error();
            }

            if ((result == IntPtr.Zero) && (error != 0))
            {
                throw new System.ComponentModel.Win32Exception(error);
            }

            return result;
        }

        [DllImport("user32.dll", EntryPoint = "SetWindowLongPtr", SetLastError = true)]
        private static extern IntPtr IntSetWindowLongPtr(IntPtr hWnd, int nIndex, IntPtr dwNewLong);

        [DllImport("user32.dll", EntryPoint = "SetWindowLong", SetLastError = true)]
        private static extern Int32 IntSetWindowLong(IntPtr hWnd, int nIndex, Int32 dwNewLong);

        private static int IntPtrToInt32(IntPtr intPtr)
        {
            return unchecked((int) intPtr.ToInt64());
        }

        [DllImport("kernel32.dll", EntryPoint = "SetLastError")]
        public static extern void SetLastError(int dwErrorCode);

        [DllImport("user32.dll")]
        static extern bool SetWindowPos(IntPtr hWnd, IntPtr hWndInsertAfter, int X,
            int Y, int cx, int cy, uint uFlags);


        public const uint SWP_NOSIZE = 0x0001;
        public const uint SWP_NOMOVE = 0x0002;
        public const uint SWP_NOACTIVATE = 0x0010;
        public const int HWND_BOTTOM = 1;
    }
}