namespace Integration

open System
open System.Runtime.InteropServices
open Logume

module Hook =
    [<AutoOpen>]
    module Native =
        type KeyboardHookStruct =
            struct
                val vkCode: int32
                val scanCode: int32
                val flags: int32
                val time: int32
                val dwExtraInfo: int32
            end
            
        type HookProc = delegate of int * int * IntPtr -> IntPtr
        
        let WH_KEYBOARD_LL = 13
        type KeyState =
            | WM_KEYDOWN = 0x0100
            | WM_KEYUP = 0x0101
        
        [<DllImport("User32.dll", SetLastError = true)>]
        extern IntPtr SetWindowsHookEx(int id, HookProc proc, IntPtr hInstance, uint32 threadId)
        
        [<DllImport "User32.dll">]
        extern IntPtr UnhookWindowsHookEx(IntPtr hInstance)
        [<DllImport "User32.dll">]
        extern IntPtr CallNextHookEx(IntPtr id, int nCode, int wParam, IntPtr lParam)
        [<DllImport "kernel32.dll">]
        extern IntPtr LoadLibrary(string lpFileName)
        [<DllImport "User32.dll">]
        extern uint16 GetKeyState(int nVirtKey)
        
        [<DllImport "User32.dll">]
        extern uint16 GetAsyncKeyState(int vKey)
        
        let isPressed (key: Keystroke.T) =
            let state = GetAsyncKeyState((int)key)
            
            let pressed = (state &&& 0x8000us) > 0us
            
            pressed
        
    module Key =
        type KeyState = Up | Down
        type T = {
            key: Keystroke.T
            state: KeyState
            modifiers: Keystroke.T list
            isMatch: Keystroke.T list -> Keystroke.T -> bool
        }
        
        let isMatch heldKeys pressedKey queryModifiers queryKey =
            if pressedKey <> queryKey then false
            else
                if heldKeys <> queryModifiers then
                    false
                else true
        
        let create k state modifiers =
            {
                key = k
                state = state
                modifiers = modifiers
                isMatch = isMatch modifiers k
            }
        
            
        
    
    type T internal (proc: HookProc) =
        let mutable hook: IntPtr = IntPtr.Zero
        let cleanup() =
            let unhooked = UnhookWindowsHookEx(hook)
            
            ()
            
        member self.Hook() =
            let u32Handle = LoadLibrary "User32"
                        
            hook <- SetWindowsHookEx(WH_KEYBOARD_LL, proc, u32Handle, 0u)
            
        member self.Rehook() =
            UnhookWindowsHookEx(hook)
            |> ignore
            
            self.Hook()
                
        member self.handle
            with get() = hook
            
        member self.proc
            with get() = proc
            
        interface IDisposable with
            member self.Dispose() =
                Console.WriteLine("Disposed!")
                cleanup()
                GC.SuppressFinalize(self)
                
        override self.Finalize() =
            cleanup()
    let private create hook = new T(hook)
    let private handle (h: T) = h.handle
    
    let private key (wParam: int) (lParam: IntPtr) =
        let vkCode = Marshal.ReadInt32 lParam
        
        let character =
            vkCode
            |> Keystroke.create
            
        let state =
            if wParam = (int KeyState.WM_KEYDOWN) then
                Key.KeyState.Down
            else
                Key.KeyState.Up
                
        Key.create character state
    
    let matchingKey keys keysDown =
        keys
        |> List.where (fun k -> HotkeyAction.matches keysDown k = HotkeyAction.Match.Full)
        |> List.sortByDescending (fun k -> List.length (HotkeyAction.keys k))
        |> List.tryHead
    
    let addKeyTo list key =
        list @ [key]
        |> List.distinct
        
    let mutable matchedKeysDown: Keystroke.T list = []
        
    let private hookWith log (runner: Twime.TwimeRoot.Update -> unit) (keys: HotkeyAction.T list) : HookProc =
        let _hookWith code wParam lParam =
            let callNextHook =
                if code >= 0 then
                    let k = key wParam lParam matchedKeysDown
                    
                    isPressed (k.key) |> ignore
                    
                    if k.state = Key.KeyState.Down then
                            
                        let matches = keys
                                        |> List.map (HotkeyAction.matches (addKeyTo matchedKeysDown k.key))
                                        
                        
                        if List.exists ((<>) HotkeyAction.Match.NoMatch) matches then
                            matchedKeysDown <-
                                addKeyTo matchedKeysDown k.key
                                
                            let keyToExecute = matchingKey keys matchedKeysDown
                            
                            if Option.isSome keyToExecute then
                                let k = Option.get keyToExecute
                               
                                Message.message (sprintf "Hotkey pressed: %A" (HotkeyAction.keys k))
                                |> Message.debug
                                |> log
                                
                                runner (HotkeyAction.action k)
                                false
                            else
                                true
                        else
                            true
                    else
                        if List.contains k.key matchedKeysDown then 
                            matchedKeysDown <- 
                                List.filter ((<>) k.key) matchedKeysDown
                            if isPressed k.key then
                                true
                            else
                                false
                        else
                            true
                else
                    true
                    
            if callNextHook then
                CallNextHookEx(IntPtr.Zero, code, wParam, lParam)
            else
                IntPtr 1
                
        HookProc _hookWith
    
    let hook (log: Logger.Logger) (runner: Twime.TwimeRoot.Update -> unit) (keys: HotkeyAction.T list) =
        let hook = hookWith log runner keys
        
        let t = create hook
        t.Hook()
        
        t
        
    let unHook (t: T) =
        (t :> IDisposable).Dispose()
