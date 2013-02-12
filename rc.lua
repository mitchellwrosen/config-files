-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")

-- Widget and layout library
local wibox = require("wibox")
local vicious = require("vicious")

-- Theme handling library
local beautiful = require("beautiful")

-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")


-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init("/usr/share/awesome/themes/zenburn/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "urxvt"
editor = os.getenv("EDITOR") or "vim"
editor_cmd = terminal .. " -e " .. editor

-- Windows key
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
    awful.layout.suit.fair,
    awful.layout.suit.tile,
    awful.layout.suit.floating,
    awful.layout.suit.max,
    awful.layout.suit.max.fullscreen,
    awful.layout.suit.magnifier
}
-- }}}

-- Wallpaper
if beautiful.wallpaper then
   for s = 1, screen.count() do
      gears.wallpaper.maximized(beautiful.wallpaper, s, true)
   end
end

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = awful.tag({ " one ", " two ", " three ", " four "}, s, layouts[1])
end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu({
   items = {
      { "awesome", myawesomemenu, beautiful.awesome_icon },
      { "terminal", terminal },
      { "chromium", "chromium" }
    }
})

mylauncher = awful.widget.launcher({
   image = beautiful.awesome_icon,
   menu = mymainmenu
})
-- }}}

-- {{{ Wibox
-- Create a textclock widget
mytextclock = awful.widget.textclock()

-- Separators
separator = wibox.widget.textbox()
separator:set_text(" | ")
spacer = wibox.widget.textbox()
spacer:set_text(" ")

---- Date/Time
local datewidget_formats = {"%H:%M", "%l:%M %P"}
local datewidget_index = 0
datewidget = wibox.widget.textbox()
vicious.register(datewidget, vicious.widgets.date,
                 "<span weight='bold'>" ..
                       "%H:%M" ..
                       --datewidget_formats[datewidget_index] ..
                       "</span>",
                 61)

---- Battery usage
local batwidget_show_remaining = false
batwidget = wibox.widget.textbox()
batwidget:buttons(awful.util.table.join(
   awful.button({ }, 1, function()
      if batwidget_show_remaining then
         batwidget_show_remaining = false
         vicious.register(batwidget, vicious.widgets.bat,
               "<span weight='bold'>Bat:</span> $2%", 67, "BAT0")
      else
         batwidget_show_remaining = true
         vicious.register(batwidget, vicious.widgets.bat,
               "<span weight='bold'>Bat:</span> $2% ($3)", 67, "BAT0")
      end
   end)
))
vicious.register(batwidget, vicious.widgets.bat,
                 "<span weight='bold'>Bat:</span> $2%", 67, "BAT0")

---- System Temp
tempwidget = wibox.widget.textbox()
vicious.register(tempwidget, vicious.widgets.thermal,
                 "<span weight='bold'>Temp:</span> $1°C", 71, "thermal_zone0")

---- Memory usage
memwidget = wibox.widget.textbox()
memwidget:buttons(awful.util.table.join(
   awful.button({ }, 3, function()
      awful.util.spawn_with_shell("echo $LOGNAME | xargs ps auwwk -%mem --user | awk '{ print $2,$4,$11 }' | xmessage -file -")
   end)
))
vicious.cache(vicious.widgets.mem)
vicious.register(memwidget, vicious.widgets.mem,
                 "<span weight='bold'>Mem:</span> $1%", 13)

---- CPU usage
cpuwidgetlabel = wibox.widget.textbox()
cpuwidgetlabel:set_markup("<span weight='bold'>CPU:</span> ")
cpuwidget = awful.widget.graph()
cpuwidget:set_width(40)
cpuwidget:set_background_color("#494B4F")
cpuwidget:set_color("#FF5656")
--cpuwidget:set_gradient_colors({ "#FF5656", "#88A175", "#AECF96" })
vicious.cache(cpuwidget, vicious.widgets.cpu)
vicious.register(cpuwidget, vicious.widgets.cpu, "$1", 7)

-- Volume level
--volwidget = widget({ type = "textbox" })
--vicious.cache(volwidget, vicious.widgets.volume)
--vicious.register(volwidget, vicious.widgets.volume,
                 --"<span weight='bold'>Vol:</span> $1%", 2, "Master")

-- Wifi widget
--wifiwidget = widget({ type = "textbox" })
--vicious.register(wifiwidget, vicious.widgets.wifi,
--      "${ssid} ${mode} ${chan} ${rate} ${link} ${linp} ${sign}", 2, "wlan0")

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
   awful.button({        }, 1, awful.tag.viewonly),
   awful.button({ modkey }, 1, awful.client.movetotag),
   awful.button({        }, 3, awful.tag.viewtoggle),
   awful.button({ modkey }, 3, awful.client.toggletag),
   awful.button({        }, 4, awful.tag.viewnext),
   awful.button({        }, 5, awful.tag.viewprev)
)
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
   awful.button({ }, 1, function (c)
      if c == client.focus then
         c.minimized = true
      else
         -- Without this, the following :isvisible() makes no sense
         c.minimized = false
         if not c:isvisible() then
            awful.tag.viewonly(c:tags()[1])
         end
         -- This will also un-minimize the client, if needed
         client.focus = c
         c:raise()
      end
   end),
   awful.button({ }, 3, function ()
      if instance then
         instance:hide()
         instance = nil
      else
         instance = awful.menu.clients({ width=250 })
      end
   end),
   awful.button({ }, 4, function ()
      awful.client.focus.byidx(1)
      if client.focus then
         client.focus:raise()
      end
   end),
   awful.button({ }, 5, function ()
      awful.client.focus.byidx(-1)
      if client.focus then
         client.focus:raise()
      end
   end)
)

for s = 1, screen.count() do
   -- Create a promptbox for each screen
   mypromptbox[s] = awful.widget.prompt()

   -- Create a taglist widget
   mytaglist[s] = awful.widget.taglist(
         s, awful.widget.taglist.filter.all, mytaglist.buttons)

   -- Create a tasklist widget
   mytasklist[s] = awful.widget.tasklist(
         s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

   -- Create the wibox
   mywibox[s] = awful.wibox({ position = "top", screen = s })

   -- Widgets that are aligned to the left
   local left_layout = wibox.layout.fixed.horizontal()
   left_layout:add(mylauncher)
   left_layout:add(mytaglist[s])
   left_layout:add(mypromptbox[s])

   -- Widgets that are aligned to the right
   local right_layout = wibox.layout.fixed.horizontal()
   if s == 1 then right_layout:add(wibox.widget.systray()) end
   right_layout:add(spacer)
   right_layout:add(cpuwidget)
   right_layout:add(separator)
   right_layout:add(memwidget)
   right_layout:add(separator)
   right_layout:add(tempwidget)
   right_layout:add(separator)
   right_layout:add(batwidget)
   right_layout:add(separator)
   right_layout:add(mytextclock)

   -- Now bring it all together (with the tasklist in the middle)
   local layout = wibox.layout.align.horizontal()
   layout:set_left(left_layout)
   layout:set_middle(mytasklist[s])
   layout:set_right(right_layout)

   mywibox[s]:set_widget(layout)
end
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
   awful.button({ }, 3, function () mymainmenu:toggle() end)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
   awful.key({ modkey,           }, "h",      awful.tag.viewprev       ),
   awful.key({ modkey,           }, "l",      awful.tag.viewnext       ),
   awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
   awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),

   awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

   awful.key({ modkey,           }, "j",
      function ()
         awful.client.focus.byidx( 1)
         if client.focus then
            client.focus:raise()
         end
      end),
   awful.key({ modkey,           }, "k",
      function ()
         awful.client.focus.byidx(-1)
         if client.focus then
            client.focus:raise()
         end
      end),
   awful.key({ modkey,           }, "w",
      function ()
         mymainmenu:show({keygrabber=true})
      end),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- Standard program
   awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
   awful.key({ modkey, "Control" }, "r", awesome.restart),
   awful.key({ modkey, "Shift"   }, "q", awesome.quit),

   awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
   awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
   awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
   awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
   awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
   awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

   awful.key({ modkey, "Control" }, "n", awful.client.restore),

   -- Prompt
   awful.key({ modkey }, "r", function()
      awful.util.spawn("dmenu_run -b -nb '#000' -nf '#FFF'")
   end),

   -- Internet
   awful.key({modkey}, "i", function() awful.util.spawn("chromium") end)
)

clientkeys = awful.util.table.join(
   awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
   awful.key({ modkey,           }, "c",      function (c) c:kill()                         end),
   awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
   awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
   awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
   awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
   awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
   awful.key({ modkey,           }, "n",
       function (c)
           -- The client currently has the input focus, so it cannot be
           -- minimized, since minimized clients can't have the focus.
           c.minimized = true
       end),
   awful.key({ modkey,           }, "m",
       function (c)
           c.maximized_horizontal = not c.maximized_horizontal
           c.maximized_vertical   = not c.maximized_vertical
       end
   )
)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        if tags[screen][i] then
                            awful.tag.viewonly(tags[screen][i])
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      if tags[screen][i] then
                          awful.tag.viewtoggle(tags[screen][i])
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.movetotag(tags[client.focus.screen][i])
                      end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.toggletag(tags[client.focus.screen][i])
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = true,
                     keys = clientkeys,
                     buttons = clientbuttons } },
    { rule = { class = "MPlayer" },
      properties = { floating = true } },
    { rule = { class = "pinentry" },
      properties = { floating = true } },
    { rule = { class = "gimp" },
      properties = { floating = true } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
    -- Add a titlebar
    -- awful.titlebar.add(c, { modkey = modkey })

    -- Enable sloppy focus
    c:connect_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
