local wezterm = require 'wezterm'

local config = {}

config.color_scheme = 'my-theme'
config.window_background_opacity = 0.85
config.font = wezterm.font 'FiraCode Nerd Font Mono'
config.font_size = 12

config.hide_tab_bar_if_only_one_tab = false
config.tab_bar_at_bottom = true
config.use_fancy_tab_bar = false
config.show_new_tab_button_in_tab_bar = false
config.audible_bell = "Disabled"
config.check_for_updates = false

config.automatically_reload_config = false

config.hide_mouse_cursor_when_typing = false

-- NOTE: WebGpu is broken
config.front_end = "OpenGL"
config.enable_wayland = true

config.window_close_confirmation = 'AlwaysPrompt'

local act = wezterm.action

wezterm.on('update-right-status', function(window, pane)
  right_status = nil

  local name = window:active_key_table()
  if name then
    name = 'MODE: ' .. name
    right_status = name
  end

  window:set_right_status(right_status or '')
end)

config.disable_default_key_bindings = true
config.keys = {
   { key = '0', mods = 'CTRL', action = act.ResetFontSize },
   { key = '-', mods = 'CTRL', action = act.DecreaseFontSize },
   { key = '+', mods = 'SHIFT|CTRL', action = act.IncreaseFontSize },

   { key = '1', mods = 'ALT', action = act.ActivateTab(0) },
   { key = '2', mods = 'ALT', action = act.ActivateTab(1) },
   { key = '3', mods = 'ALT', action = act.ActivateTab(2) },
   { key = '4', mods = 'ALT', action = act.ActivateTab(3) },
   { key = '5', mods = 'ALT', action = act.ActivateTab(4) },
   { key = '6', mods = 'ALT', action = act.ActivateTab(5) },
   { key = '7', mods = 'ALT', action = act.ActivateTab(6) },
   { key = '8', mods = 'ALT', action = act.ActivateTab(7) },
   { key = '9', mods = 'ALT', action = act.ActivateTab(8) },
   { key = 'w', mods = 'ALT', action = act.CloseCurrentTab{ confirm = true } },
   { key = 't', mods = 'ALT', action = act.SpawnTab 'CurrentPaneDomain' },
   { key = 'h', mods = 'ALT', action = act.ActivateTabRelative(-1) },
   { key = 'l', mods = 'ALT', action = act.ActivateTabRelative(1) },

   { key = 'f', mods = 'ALT', action = act.TogglePaneZoomState },
   { key = 'j', mods = 'ALT', action = act.ActivatePaneDirection 'Next' },
   { key = 'k', mods = 'ALT', action = act.ActivatePaneDirection 'Prev' },
   { key = 'Enter', mods = 'SHIFT|ALT', action = act.SplitVertical { domain = "CurrentPaneDomain" } },
   { key = 'Enter', mods = 'ALT', action = act.SplitHorizontal { domain = "CurrentPaneDomain" } },
   { key = 'q', mods = 'SHIFT|ALT', action = act.CloseCurrentPane { confirm = true } },

   { key = 'p', mods = 'SHIFT|CTRL', action = act.ActivateCommandPalette },

   { key = 'v', mods = 'SHIFT|CTRL', action = act.PasteFrom 'Clipboard' },
   { key = 'c', mods = 'SHIFT|CTRL', action = act.CopyTo 'Clipboard' },

   { key = 'Space', mods = 'SHIFT|CTRL', action = act.ActivateCopyMode },

   { key = 'r', mods = 'ALT', action = act.ActivateKeyTable {
	name = 'resize_pane',
	one_shot = false,
   }},
}
config.key_tables = {
   copy_mode = {
      { key = '$', mods = 'SHIFT', action = act.CopyMode 'MoveToEndOfLineContent' },
      { key = '0', action = act.CopyMode 'MoveToStartOfLine' },
      { key = 'w', action = act.CopyMode 'MoveForwardWord' },
      { key = 'e', action = act.CopyMode 'MoveForwardWordEnd' },
      { key = 'b', action = act.CopyMode 'MoveBackwardWord' },
      { key = 'h', action = act.CopyMode 'MoveLeft' },
      { key = 'j', action = act.CopyMode 'MoveDown' },
      { key = 'k', action = act.CopyMode 'MoveUp' },
      { key = 'l', action = act.CopyMode 'MoveRight' },
      { key = 'v', mods = 'SHIFT', action = act.CopyMode { SetSelectionMode =  'Line' } },
      { key = 'v', action = act.CopyMode { SetSelectionMode =  'Cell' } },
      { key = 'v', mods = 'CTRL', action = act.CopyMode { SetSelectionMode =  'Block' } },
      { key = 'y', action = act.Multiple {
	   act.CopyTo 'PrimarySelection',
	   act.ClearSelection,
	   act.CopyMode 'ClearSelectionMode',
      } },
      { key = 'Escape', action = act.CopyMode 'Close' },
      { key = 'c', mods = 'CTRL', action = act.CopyMode 'Close' },
   },

   resize_pane = {
      { key = 'h', action = act.AdjustPaneSize { 'Left', 3 } },
      { key = 'l', action = act.AdjustPaneSize { 'Right', 3 } },
      { key = 'j', action = act.AdjustPaneSize { 'Down', 3 } },
      { key = 'k', action = act.AdjustPaneSize { 'Up', 3 } },

      { key = 'r', action = act.RotatePanes 'Clockwise' },
      { key = 'r', mods = 'SHIFT', action = act.RotatePanes 'CounterClockwise' },

      { key = 'Escape', action = act.PopKeyTable },
   },
}

return config
