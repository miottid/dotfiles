-- Pull in the wezterm API
local wezterm = require("wezterm")

-- This will hold the configuration.
local config = wezterm.config_builder()

config.audible_bell = "Disabled"

-- This is where you actually apply your config choices
config.font = wezterm.font("Iosevka Nerd Font Mono")
config.font_size = 14
config.hide_tab_bar_if_only_one_tab = true
config.window_background_opacity = 0.90
config.window_padding = { left = 0, right = 0, top = 0, bottom = 0 }
config.initial_rows = 40
config.initial_cols = 108
config.window_close_confirmation = "NeverPrompt"

-- config.color_scheme = 'darkermatrix'
config.color_scheme = "Dark Pastel"

config.colors = {
	foreground = "#ffffff",
	-- background = "#0d0e1c",
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

-- and finally, return the configuration to wezterm
return config
