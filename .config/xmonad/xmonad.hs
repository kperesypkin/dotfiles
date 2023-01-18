import XMonad

import Graphics.X11.ExtraTypes.XF86

import XMonad.Layout.Magnifier
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns

import XMonad.Hooks.DynamicLog (wrap, shorten, xmobarPP, xmobarColor, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce
import XMonad.Util.Ungrab


myFont :: String
myFont = "xft:Fira Code:regular:size=12:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask      -- sets modkey to super/windows key

myTerminal :: String
myTerminal = "urxvt"      -- sets default terminal

myBrowser :: String
myBrowser = "firefox"     -- sets firefox as browser

myEmacs :: String
myEmacs = "emacsclient -c -a 'emacs' "  -- makes emacs keybindings easier to type

myEditor :: String
myEditor = "emacsclient -c -a 'emacs' "  --sets emacs as editor

myBorderWidth :: Dimension
myBorderWidth = 2         -- sets border width for windows

-- myNormColor :: String     -- border color of normal window
-- myNormColor = colorBlack  -- variable imported from Colors.THEME

-- myFocusColor :: String    -- border color of focused window
-- myFocusColor = color15    -- variable imported from Colors.THEME

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "nitrogen --restore &"   -- if you prefer nitrogen to feh
  setDefaultCursor xC_left_ptr
  setWMName "LG3D"

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = renamed [Replace "Columns"] $ magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1    -- default number of windows in the master pane
    ratio = 1/2    -- default proportion of screen occupied by master pane
    delta = 3/100  -- percent of screen to increment by when resizing panes

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = cyan . wrap " " "" . xmobarBorder "Bottom" "#66D9EF" 2
    , ppHidden          = blue . wrap " " ""
    , ppHiddenNoWindows = darkBlue . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 10

    blue, darkBlue, cyan, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#F92660" ""
    blue     = xmobarColor "#268BD2" ""
    darkBlue = xmobarColor "#727280" ""
    cyan     = xmobarColor "#66d9ef" ""
    white    = xmobarColor "#C1C1C1" ""
    yellow   = xmobarColor "#E6DB74" ""
    red      = xmobarColor "#E74C3C" ""
    lowWhite = xmobarColor "#DFDFDF" ""

myStatusBar = statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (pure myXmobarPP)

-- myKeys =
--   [ ("<XF86MonBrightnessUp>", spawn "lux -a 10%")
--   , ("<XF86MonBrightnessDown>", spawn "lux -s 10%")
--   ]

myConfig = def
    { modMask             = myModMask
    , layoutHook          = mySpacing 5 $ myLayout
    , startupHook         = myStartupHook
    , terminal            = myTerminal
    --, borderWidth         = myBorderWidth 
    }
   `additionalKeysP`
    [ ("M-e"  , spawn "emacsclient -c -a 'emacs'"    )
    , ("M-f"  , spawn "firefox"                      )
    , ("M-C-s", unGrab *> spawn "scrot -s"           )
    , ("<XF86MonBrightnessUp>", spawn "lux -a 5%"   )
    , ("<XF86MonBrightnessDown>", spawn "lux -s 5%" )
    , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
    ]

  
main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB myStatusBar defToggleStrutsKey 
     $ myConfig
