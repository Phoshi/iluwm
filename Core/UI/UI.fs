namespace Twime.UI

open Twime
open Twime.LayoutUIComponents

module UI =
    type T = Display.T -> int -> Box.T -> TreeNavigation.NodeReference -> LayoutTree.T -> UIComponent.T list

