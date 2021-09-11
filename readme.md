iluwm
=====

_'I love you' window manager_

Description
-----------

iluwm is a dynamic tiling window manager for the Windows platform. 

It supports multiple monitors, and each monitor can have multiple virtual desktop 'tags'. Each tag contains a set of windows in a given layout. The state is persistent between program restarts and config changes, though not between reboots. Dynamic layout configuration is done according to relative weighting, and so adding and removing windows from a layout will respect the intent of the various window sizes, rather than trying to adjust a fixed layout.

Additionally, iluwm supports:
- window spacing ('gaps'), configured per-tag, either static amounts or scaling depending on the total window weight
- a lot of Windows™ nonsense, like windows that refuse to resize or have minimum sizes, full screen games, windows with fully custom chrome, that way that if you precisely position a window Windows actually puts it a few pixels off for some reason, and et cetera. Windows is a pain, but I think I have things working fairly well despite that.
- an IPC protocol for communication with other applications, and a plugin for [Keypirinha](https://keypirinha.com/) that exposes a text-driven interface that can make it easier to do some actions than hotkeys would
- a vim-like 'mark' system to mark certain windows, and switch directly to them 
- configuration in code for full flexiblity, but configuration files are dynamically compiled so that they're easy to change without impacting performance
- hotkeys can be bound to any action, or you can write your own actions to do something specific, or run external applications or scripts, or so on
- hotkeys can be standard multi-key affairs, or a 'leader key' style series of keystrokes
- configurable UI with interactive modules (including 'now playing', a better-discord integration to flag up DMs, a script runner, and a fuzzy clock!) that's sufficient to be a basic shell replacement if you have something else to launch applications
- override rules for specific window creation to do stuff like always open certain windows in a sidebar, or on a particular tag, with a particular mark, or to split the current window, or so on
- override rules for all the other window actions too, so you can configure what happens when windows open, close, change titles, or so on. I think the defaults are pretty sane, but if you want to always split the current window when a new thing opens or something, you can
- support for full-screen, floating, and focus modes, with bindable hotkeys
- no arbitrary limitations on what can be bound to a hotkey. Everything is a transformation on the tree structure, so you can bind anything to anything.

Concepts
--------

In iluwm, every tag contains a layout. A layout is a tree with nodes which are either a window, or a container. The root of the tree is a little bit special, in that it is the only container which is allowed to contain zero children. In other cases, a childless container is automatically destroyed.

A container can have one of three modes, which are dynamic and can be changed:
- horizontal, where children are laid out horizontally, left to right
- vertical, where children are laid out vertically, top to bottom
- tabbed, where children are given the full container space, but only one is visible at a time

A container can be a child of another container, so these three modes should be sufficient to produce some complicated layouts if necessary. Less flexible, but more automatic, layouts like you'd find in Awesome WM are not provided and can't be done entirely in configuration, but could be implemented by introducing a new "layout engine" and binding hotkeys to assign that new mode to a container. 

Usage
-----

Download the latest release, extract the archive, and execute both the iluwm runner and the keyboard hook. Neither need elevation to work, but if you grant it to the runner it will be able to manipulate elevated windows, and if you grant it to the keyboard hook it will be able to catch hotkeys when an elevated window is active.

Additionally, you will need the .net 5.0 runtime and the .net framework 4.7.2 runtime. 
