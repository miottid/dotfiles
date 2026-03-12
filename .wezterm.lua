-- Pull in the wezterm API
local wezterm = require("wezterm")
local mux = wezterm.mux
local act = wezterm.action

-- This will hold the configuration.
local config = wezterm.config_builder()

config.audible_bell = "Disabled"

-- This is where you actually apply your config choices
config.font = wezterm.font("Iosevka Nerd Font Mono")
-- config.font = wezterm.font("Myna")
config.font_size = 16
config.hide_tab_bar_if_only_one_tab = true
config.window_background_opacity = 0.95
config.window_padding = { left = 0, right = 0, top = 0, bottom = 0 }
config.initial_rows = 45
config.initial_cols = 170
config.window_close_confirmation = "NeverPrompt"
-- config.window_decorations = "NONE"

-- Configure Leader key and layout to mimic tmux
-- config.leader = { key = 'q', mods = 'CTRL', timeout_milliseconds = 1000 }
-- config.keys = {
--     {
--         key = '|',
--         mods = 'LEADER|SHIFT',
--         action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
--     },
--     {
--         key = '-',
--         mods = 'LEADER',
--         action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' },
--     },
--     {
--         key = 'o',
--         mods = 'LEADER',
--         action = wezterm.action.ActivatePaneDirection 'Next',
--     },
--     {
--         key = 'o',
--         mods = 'LEADER|CTRL',
--         action = wezterm.action.PaneSelect { mode = 'SwapWithActiveKeepFocus' },
--     },
--     {
--         key = 'w',
--         mods = 'LEADER',
--         action = wezterm.action.PaneSelect,
--     },
--     {
--         key = 'z',
--         mods = 'LEADER',
--         action = wezterm.action.TogglePaneZoomState,
--     },
--     {
--         key = 'q',
--         mods = 'LEADER',
--         action = wezterm.action.SendKey { key = 'q', mods = 'CTRL' },
--     },
-- }

-- Make CMD send Meta/Alt (so Emacs in terminal uses CMD as Meta like GUI Emacs)
config.send_composed_key_when_left_alt_is_pressed = false
config.send_composed_key_when_right_alt_is_pressed = false

config.keys = {
	{ key = "o", mods = "CMD", action = act.SendKey({ key = "o", mods = "ALT" }) },
	{ key = "x", mods = "CMD", action = act.SendKey({ key = "x", mods = "ALT" }) },
	{ key = "g", mods = "CMD", action = act.SendKey({ key = "g", mods = "ALT" }) },
	{ key = "w", mods = "CMD", action = act.SendKey({ key = "w", mods = "ALT" }) },
	{ key = "s", mods = "CMD", action = act.SendKey({ key = "s", mods = "ALT" }) },
	{ key = "d", mods = "CMD", action = act.SendKey({ key = "d", mods = "ALT" }) },
	{ key = "f", mods = "CMD", action = act.SendKey({ key = "f", mods = "ALT" }) },
	{ key = "b", mods = "CMD", action = act.SendKey({ key = "b", mods = "ALT" }) },
	{ key = "<", mods = "CMD", action = act.SendKey({ key = "<", mods = "ALT" }) },
	{ key = ">", mods = "CMD", action = act.SendKey({ key = ">", mods = "ALT" }) },
	{ key = "%", mods = "CMD", action = act.SendKey({ key = "%", mods = "ALT" }) },
	{ key = "0", mods = "CMD", action = act.SendKey({ key = "0", mods = "ALT" }) },
}

config.color_scheme = "Dark Pastel"

config.colors = {
	foreground = "#ffffff",
	background = "#181818",

	ansi = {
		"#000000",
		"#f43841",
		"#73c936",
		"#ffdd33",
		"#5f627f",
		"#9e95c7",
		"#5f627f",
		"#ffffff",
	},
	brights = {
		"#ffffff",
		"#196a6c",
		"#73c936",
		"#ffdd33",
		"#96a6c8",
		"#9e95c7",
		"#96a6c8",
		"#ffffff",
	},
}

local scratch = "_scratch" -- Keep this consisten with Hammerspoon
wezterm.on("gui-attached", function(domain)
                               local workspace = mux.get_active_workspace()
                               if workspace ~= scratch then return end

                               local screen = wezterm.gui.screens().active
                               local ratio = 0.5
                               local width, height = screen.width * ratio, screen.height * ratio
                               local tab, pane, window = wezterm.mux.spawn_window(cmd or {
                            	   position = {
                                       x = (screen.width - width) / 2,
                                       y = (screen.height - height) / 2,
                                       origin = 'ActiveScreen',
                                   },
                               })
                               window:gui_window():set_inner_size(width, height)


                               for _, window in ipairs(mux.all_windows()) do
                                   local gwin = window:gui_window()
                                   if gwin ~= nil then
                                       gwin:perform_action(act.SetWindowLevel "AlwaysOnTop", wgwin:active_pane())
                                   end
                               end
end)

return config
