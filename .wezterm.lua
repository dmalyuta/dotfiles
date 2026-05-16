-- Pull in the wezterm API
local wezterm = require 'wezterm'
local act = wezterm.action;

-- This table will hold the configuration.
local config = wezterm.config_builder()

-- Initial geometry for new windows.
config.initial_cols = 100
config.initial_rows = 40

-- Quality of life.
config.window_close_confirmation = 'NeverPrompt'

-- For example, changing the color scheme:
local THEME = 'Tomorrow Night'
config.color_scheme = THEME
config.colors = {
  scrollbar_thumb = '#49404f',
}

-- Font
config.font = wezterm.font "CaskaydiaCove Nerd Font Mono"
config.font_size = 12

-- Tab bar.
local tabline = wezterm.plugin.require "https://github.com/michaelbrusegard/tabline.wez"
tabline.setup({
  options = {
    theme = THEME,
    tabs_enabled = true, -- false will allow customizing tab title.
  },
  sections = {
    tab_active = {
      '*',
      'index',
      'tab',
    },
    tab_inactive = {
      'index',
      'tab',
    },
  },
})
tabline.apply_to_config(config)

-- GUI features
config.scrollback_lines = 50000
config.enable_scroll_bar = true
config.default_cursor_style = "SteadyBlock"

-- Change the default click behavior so that it only selects
-- text and doesn't open hyperlinks
config.mouse_bindings = {
  {
    event = { Up = { streak = 1, button = "Left" } },
    mods = "NONE",
    action = act.CompleteSelection("PrimarySelection"),
  },

  -- and make CTRL-Click open hyperlinks
  {
    event = { Up = { streak = 1, button = "Left" } },
    mods = "CTRL",
    action = act.OpenLinkAtMouseCursor,
  },
}

-- Custom window title.
wezterm.on('format-window-title', function(tab, pane, tabs, panes, config)
  return "Terminal"
end)

-- Rendering
-- Switch to WebGpu after https://github.com/wezterm/wezterm/issues/6815 is fixed
config.front_end = "OpenGL"

-- Keybindings
config.leader = { key = 'z', mods = 'ALT', timeout_milliseconds = 1000 }
local function move_pane(key, direction)
  return {
    key = key,
    mods = 'ALT',
    action = wezterm.action.ActivatePaneDirection(direction),
  }
end
local function resize_pane(key, direction)
  return {
    key = key,
    mods = 'ALT|SHIFT',
    action = wezterm.action.AdjustPaneSize { direction, 3 },
  }
end
config.keys = {
  {
    key = 'z',
    -- When we're in leader mode _and_ CTRL+z is pressed...
    mods = 'LEADER|CTRL',
    -- Actually send CTRL+z key to the terminal
    action = wezterm.action.SendKey { key = 'z', mods = 'CTRL' },
  },
  -- Splitting panes.
  { key = '-',  mods = 'LEADER', action = act.SplitVertical { domain = "CurrentPaneDomain" } },
  { key = '\\', mods = 'LEADER', action = act.SplitHorizontal { domain = "CurrentPaneDomain" } },
  { key = 's',  mods = 'LEADER', action = act.PaneSelect },
  -- Moving between panes.
  move_pane('DownArrow', 'Down'),
  move_pane('UpArrow', 'Up'),
  move_pane('LeftArrow', 'Left'),
  move_pane('RightArrow', 'Right'),
  -- Resizing panes.
  resize_pane('DownArrow', 'Down'),
  resize_pane('UpArrow', 'Up'),
  resize_pane('LeftArrow', 'Left'),
  resize_pane('RightArrow', 'Right'),
  -- Working with tabs.
  {
    -- When we push LEADER + t...
    key = 't',
    mods = 'LEADER',
    -- Activate the `manipulate_tabs` keytable
    action = wezterm.action.ActivateKeyTable {
      name = 'manipulate_tabs',
      -- Ensures the keytable stays active after it handles its first keypress.
      one_shot = false,
      -- Deactivate the keytable after a timeout.
      timeout_milliseconds = 1000,
    }
  },
}
config.key_tables = {
  manipulate_tabs = {
    -- Create tabs.
    {
      key = 'c',
      action = act.SpawnTab 'CurrentPaneDomain',
    },
    -- Naming tabs.
    {
      key = 'n',
      action = act.PromptInputLine {
        description = 'Enter new name for tab',
        action = wezterm.action_callback(function(window, pane, line)
          if line then
            window:active_tab():set_title(line)
          end
        end),
      },
    },
    -- Moving between tabs.
    { key = ',', action = act.ActivateTabRelative(-1) },
    { key = '.', action = act.ActivateTabRelative(1) },
    -- Moving tabs.
    { key = '<', mods = 'SHIFT',                      action = act.MoveTabRelative(-1) },
    { key = '>', mods = 'SHIFT',                      action = act.MoveTabRelative(1) },
  },
}

-- and finally, return the configuration to wezterm
return config
