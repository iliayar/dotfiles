local nixcfg = require("nixcfg")
local hy3 = hl.plugin.hy3

hl.monitor({
    output = "",
    mode = "preferred",
    position = "auto",
    scale = 1,
})

hl.env("XCURSOR_SIZE", 24)
hl.env("HYPRCURSOR_SIZE", 24)

if nixcfg.hyprcursor then
    hl.env("HYPRCURSOR_THEME", nixcfg.hyprcursor)
end
if nixcfg.xcursor then
    hl.env("XCURSOR_THEME", nixcfg.xcursor)
end

hl.env("BROWSER", nixcfg.browser)

hl.on("hyprland.start", function()
    hl.exec_cmd("swww-daemon -q")
    hl.exec_cmd("waypaper --restore")
    hl.exec_cmd("waybar & pypr")
end)

for i, cmd in ipairs(nixcfg.startupExtra) do
    hl.exec_cmd(cmd)
end

if nixcfg.autoLock then
    hl.exec_cmd(nixcfg.autoLock)
end

hl.config({
    debug = {
     disable_logs = true,
    },
    general = {
      layout = "hy3",
      gaps_in = 0,
      gaps_out = 0,
    },

    plugin = {
      hy3 = {
          no_gaps_when_only = 1,
          tabs = {
              height = 20,
              padding = 0,
              radius = 0,
              border_width = 0,
              text_font = nixcfg.theme.font,
              text_height = 12,
              colors = {
                  active_border = nixcfg.theme.colActiveBg,
                  active_text = nixcfg.theme.colActiveFg,
                  inactive_border = nixcfg.theme.colInactiveBg,
                  inactive_text = nixcfg.theme.colInactiveFg,
                  focused_border = nixcfg.theme.colFocusedBg,
                  focused_text = nixcfg.theme.colFocusedFg,
              },
              blur = false,
          },
      },
    },
    decoration = {
      shadow = {
        enabled = false,
      },
      rounding = false,
      blur = {
        enabled = false,
      },
    },
    animations = {
      enabled = false,
    },
    cursor = {
      no_warps = true,
      enable_hyprcursor = false,
      no_hardware_cursors = false,
    },
    input = {
      repeat_rate = 50,
      repeat_delay = 200,
      follow_mouse = 2,
      float_switch_override_focus = 0,
      kb_layout = "us,ru",
      kb_options = nixcfg.kbOptions,
      tablet = {
        transform = 0,
      },
    },
    misc = {
      mouse_move_focuses_monitor = false,
      disable_hyprland_logo = true,
      force_default_wallpaper = 0,
    }
})

if nixcfg.flameshot then
    hl.window_rule({
        name = "flameshot-multi-display-fix",
        match = { title = "flameshot" },
        animation = "fade",
        rounding = 0,
        border_size = 0,
        fullscreen_state = "0 0",
        float = true,
        pin = true,
        monitor = "DP-2",
        move = "0 0",
        size = nixcfg.flameshot.size,
    })
end

function M(keys)
    return nixcfg.mainMod .. " + " .. keys
end

hl.bind(M "mouse:272", hl.dsp.window.drag(), { mouse = true })
hl.bind(M "mouse:273", hl.dsp.window.resize(), { mouse = true })

hl.bind(M "Return", hl.dsp.exec_cmd(nixcfg.terminal))
hl.bind(M "D", hl.dsp.exec_cmd("vicinae open"))
hl.bind(M "SHIFT + Q", hl.dsp.window.close())
hl.bind(M "F", hl.dsp.window.fullscreen({ action = "toggle" }))
hl.bind(M "comma", hl.dsp.window.float({ action = "toggle" }))

hl.bind(M "H", hy3.move_focus("left", {"visible"}))
hl.bind(M "L", hy3.move_focus("right", {"visible"}))
hl.bind(M "J", hy3.move_focus("down", {"visible"}))
hl.bind(M "K", hy3.move_focus("up", {"visible"}))

hl.bind(M "SHIFT + H", hy3.move_window("left", {"visible"}))
hl.bind(M "SHIFT + L", hy3.move_window("right", {"visible"}))
hl.bind(M "SHIFT + J", hy3.move_window("down", {"visible"}))
hl.bind(M "SHIFT + K", hy3.move_window("up", {"visible"}))

hl.bind(M "CTRL + J", hy3.change_focus("raise"))
hl.bind(M "CTRL + K", hy3.change_focus("lower"))

hl.bind(M "E", hy3.change_group("opposite"))
hl.bind(M "W", hy3.change_group("toggletab"))

hl.bind(M "CTRL + H", hy3.move_focus("left"))
hl.bind(M "CTRL + L", hy3.move_focus("right"))

hl.bind(M "bracketleft", hl.dsp.focus({ monitor = "+1" }))
hl.bind(M "bracketright", hl.dsp.focus({ monitor = "-1" }))

hl.bind(M "C", hl.dsp.exec_cmd("pypr menu"))
hl.bind(M "T", hl.dsp.exec_cmd("pypr toggle term-quake"))

if nixcfg.flameshot then
    hl.bind("SHIFT + print", hl.dsp.exec_cmd("flameshot full"))
    hl.bind("print", hl.dsp.exec_cmd("flameshot gui"))
else
    hl.bind(M "SHIFT + print", hl.dsp.exec_cmd(nixcfg.screenshot .. " e f"))
    hl.bind("SHIFT + print", hl.dsp.exec_cmd(nixcfg.screenshot .. " n f"))
    hl.bind(M "print", hl.dsp.exec_cmd(nixcfg.screenshot .. " e"))
    hl.bind("print", hl.dsp.exec_cmd(nixcfg.screenshot))
end

for i = 1, 10 do
    local key = i % 10
    hl.bind(M(key), hl.dsp.focus({ workspace = i, on_current_monitor = true }))
    hl.bind(M("SHIFT + " .. key), hy3.move_to_workspace(i, {"follow"}))
    hl.bind(M("CTRL + " .. key), hy3.move_to_workspace(i))
end

hl.bind(M "R", hl.dsp.submap("resize"))
hl.define_submap("resize", function()
    hl.bind("L", hl.dsp.window.resize({ x = 30, y = 0, relative = true }))
    hl.bind("H", hl.dsp.window.resize({ x = -30, y = 0, relative = true }))
    hl.bind("J", hl.dsp.window.resize({ x = 0, y = -30, relative = true }))
    hl.bind("K", hl.dsp.window.resize({ x = 0, y = 30, relative = true }))
    hl.bind("escape", hl.dsp.submap("reset"))
end)

hl.bind(M "SHIFT + E", hl.dsp.submap("power"))
hl.define_submap("power", "reset", function()
    hl.bind("S", hl.dsp.exec_cmd("systemctl suspend"))
    hl.bind("R", hl.dsp.exec_cmd("systemctl reboot"))
    hl.bind("SHIFT + S", hl.dsp.exec_cmd("systemctl poweroff"))
    if nixcfg.lock then
        hl.bind("L", hl.dsp.exec_cmd(nixcfg.lock))
    end
    hl.bind("E", hl.dsp.exit())
    hl.bind("escape", hl.dsp.submap("reset"))
end)

hl.bind(M "N", hl.dsp.submap("notifications"))
hl.define_submap("notifications", "reset", function()
    hl.bind("A", hl.dsp.exec_cmd("dunstctl close-all"))
    hl.bind("escape", hl.dsp.submap("reset"))
end)

-- # Scratchpads
-- bind = $mainMod, S, submap, scratchpads
-- submap = scratchpads
--
-- bind = ,T, exec, pypr toggle term-quake
-- bind = ,T, submap, reset
-- windowrule = match:class local.iliayar.term-quake, float on
-- # $term_quake = class:term-quake
-- # windowrule = workspace special silent,$term_quake
-- # windowrule = float,$term_quake
--
-- bind = ,N, exec, pypr toggle org-notes
-- bind = ,N, submap, reset
-- $org_notes = org-notes
-- windowrule = match:title $org_notes, workspace special silent
-- windowrule = match:title $org_notes, float on
--
-- bind = ,O, exec, pypr toggle obsidian
-- bind = ,O, submap, reset
-- $obsidian = obsidian
-- windowrule = match:class $obsidian, workspace special silent
-- windowrule = match:class $obsidian, float on
--
-- bind=,escape,submap,reset
-- submap = reset

hl.bind("XF86AudioMute", hl.dsp.exec_cmd("wpctl set-mute '@DEFAULT_SINK@' toggle"))
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("wpctl set-volume '@DEFAULT_SINK@' 5%-"))
hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("wpctl set-volume '@DEFAULT_SINK@' 5%+"))
hl.bind("XF86AudioMicMute", hl.dsp.exec_cmd("wpctl set-mute '@DEFAULT_SOURCE@' toggle"))

hl.bind("XF86MonBrightnessUp", hl.dsp.exec_cmd("brightnessctl set 10%+"))
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("brightnessctl set 10%-"))

function playerctl(cmd)
    return "playerctl -p " .. nixcfg.player .. " " .. cmd
end

hl.bind("XF86AudioPlay", hl.dsp.exec_cmd(playerctl "play-pause"))
hl.bind("XF86AudioPrev", hl.dsp.exec_cmd(playerctl "previous"))
hl.bind("XF86AudioNext", hl.dsp.exec_cmd(playerctl "next"))

function force_opacity(class)
    hl.window_rule({
        name = class .. "-opacity",
        match = { class = class },
        opacity = "0.9"
    })
end

force_opacity("Spotify")
force_opacity("VSCodium")
force_opacity("Code")

