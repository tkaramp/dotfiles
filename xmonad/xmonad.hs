-- Imports.
import XMonad                          -- (0) core xmonad libraries

import qualified XMonad.StackSet as W  -- (0a) window stack manipulation
import qualified Data.Map        as M  -- (0b) map creation
import Data.Monoid

import XMonad.Layout.MagicFocus        -- (0c)

-- Hooks -----------------------------------------------------

import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers   -- 	   	for doCenterFloat, put floating
                                    --     	windows in the middle of the
                                    --     	screen
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks     --	   	dock/tray mgmt
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ICCCMFocus

-- Layout ----------------------------------------------------

import XMonad.Layout.ResizableTile  --		resize non-master windows too
import XMonad.Layout.Grid			--		grid layout
import XMonad.Layout.NoBorders		--		get rid of borders sometimes
import XMonad.Layout.Tabbed
import XMonad.Layout.Fullscreen
import XMonad.Layout.Circle
import XMonad.Layout.IM
import XMonad.Layout.ThreeColumns
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Spacing		--		insert gap between windows

-- Actions ---------------------------------------------------

import XMonad.Actions.CycleWS      	--		general workspace-switching
                                   	--      goodness
import XMonad.Actions.Search hiding (Query, images)
									--		some predefined web searches

-- Prompts ---------------------------------------------------
import XMonad.Prompt                -- (23) general prompt stuff.
import XMonad.Prompt.Man            -- (24) man page prompt
import XMonad.Prompt.AppendFile     -- (25) append stuff to my NOTES file
import XMonad.Prompt.Ssh            -- (26) ssh prompt
import XMonad.Prompt.Input          -- (26) generic input prompt, used for
                                    --      making more generic search
                                    --      prompts than those in
                                    --      XMonad.Prompt.Search



-- Utilities -------------------------------------------------

import XMonad.Util.Run				--		for 'spawnPipe', 'hPutStrLn'
import XMonad.Util.EZConfig			--		"M-C-x" style keybindings
import XMonad.Util.WindowProperties
import XMonad.Util.NamedScratchpad  --		'scratchpad' terminal
import XMonad.Util.WorkspaceCompare(getSortByTag, getSortByXineramaPhysicalRule)

import Data.Ratio ((%))

--import System.IO
--import System.Exit
--import XMonad.Operations

{-
  Xmonad configuration variables. These settings control some of the
  simpler parts of xmonad's behavior and are straightforward to tweak.
-}

myModMask            = mod4Mask       -- changes the mod key to "super"

-- default programs
myTerminal           = "terminator"   -- which terminal software to use
myIMRosterTitle      = "Buddy List"   -- title of roster on IM workspace
                                      -- use "Buddy List" for Pidgin, but
                                      -- "Contact List" for Empathy
myBrowser = "google-chrome"
myEmail    = "thundbird"
myFileMan  = "nautilus"
myVolumeControl = "pavucontrol"
--myEditor   = "sublime-text"
myPiracy   = "transmission-gtk"
myMusic    = "spotify"
myScreenshooter = "scrot -e 'mv $f ~/screenshots/'"
myControledScreenshooter = "sleep 0.2; " ++ myScreenshooter ++ " -s"

-- window border customization
--"#154D83" --"#2E9AFE"
myFocusedBorderColor = "#1793D1" -- color of focused border
myNormalBorderColor  = "#413F3B"      -- color of inactive border
myBorderWidth        = 2              -- width of border around windows


-- | Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- | Whether a mouse click select the focus or is just passed to the window
myClickJustFocuses :: Bool
myClickJustFocuses = True


{-
  Xmobar configuration variables. These settings control the appearance
  of text which xmonad is sending to xmobar via the DynamicLog hook.
-}

--myTitleColor     = "black"  -- color of window title
--myTitleLength    = 80         -- truncate window title to this length
--myCurrentWSColor = "#2E9AFE"  -- color of active workspace
myVisibleWSColor = "#c185a7"  -- color of inactive workspace
--myUrgentWSColor  = "#cc0000"  -- color of workspace with 'urgent' window
--myCurrentWSLeft  = "["        -- wrap active workspace with these
--myCurrentWSRight = "]"
--myVisibleWSLeft  = "("        -- wrap inactive workspace with these
--myVisibleWSRight = ")"
--myUrgentWSLeft  = "{"         -- wrap urgent workspace with these
--myUrgentWSRight = "}"


-- Visual settings used by e.g. dmenu
myFont         = "xft:ProggyTiny:pixelsize=9"
myBgColor      = "#1b1d1e"
myFgColor      = "#bbbbbb"
mySelFgColor   = "#ffffff"
mySelBgColor   = "#333333"
myBorderColor  = "#40464b"
myFocusedColor = "#839cad"
myCurrentColor = "#cd5c5c"
myEmptyColor   = "#4c4c4c"
myHiddenColor  = "#dddddd"
myLayoutColor  = "#839cad"
myUrgentColor  = "#2b9ac8"
myTitleColor   = "#ffffff"
myTitleLength    = 80         -- truncate window title to this length
mySepColor     = "#58504c"

{-
  Workspace configuration. Here you can change the names of your
  workspaces. Note that they are organized in a grid corresponding
  to the layout of the number pad.

  I would recommend sticking with relatively brief workspace names
  because they are displayed in the xmobar status bar, where space
  can get tight. Also, the workspace labels are referred to elsewhere
  in the configuration file, so when you change a label you will have
  to find places which refer to it and make a change there as well.

  This central organizational concept of this configuration is that
  the workspaces correspond to keys on the number pad, and that they
  are organized in a grid which also matches the layout of the number pad.
  So, I don't recommend changing the number of workspaces unless you are
  prepared to delve into the workspace navigation keybindings section
  as well.
-}

{-myWorkspaces =
  [
    "7:Video",  "8:Music", "9:Pix",
    "4:Chat",  "5:Mail", "6:Files",
    "1:Dev",  "2:Term", "3:Web",
    "0:Min",    "Extr1", "Extr2"
  ]-}

myWorkspaces =
  [
    "1:Dev",  "2:Term", "3:Web",
    "4:Chat",  "5:Mail", "6:Files",
    "7:Video",  "8:Music", "9:Pix",
    "0:Min",    "Extr1", "Extr2"
  ]

startupWorkspace = "5:Dev"  -- which workspace do you want to be on after launch?

{-
  Layout configuration. In this section we identify which xmonad
  layouts we want to use. I have defined a list of default
  layouts which are applied on every workspace, as well as
  special layouts which get applied to specific workspaces.
  Note that all layouts are wrapped within "avoidStruts". What this does
  is make the layouts avoid the status bar area at the top of the screen.
  Without this, they would overlap the bar. You can toggle this behavior
  by hitting "super-b" (bound to ToggleStruts in the keyboard bindings
  in the next section).
-}

-- Define group of default layouts used on most screens, in the
-- order they will appear.
-- "smartBorders" modifier makes it so the borders on windows only
-- appear if there is more than one visible window.
-- "avoidStruts" modifier makes it so that the layout provides
-- space for the status bar at the top of the screen.
defaultLayouts = smartBorders(avoidStruts(
  -- ResizableTall layout has a large master window on the left,
  -- and remaining windows tile on the right. By default each area
  -- takes up half the screen, but you can resize using "super-h" and
  -- "super-l".
  spacing 5 (ResizableTall 1 (3/100) (1/2) [])

  -- Mirrored variation of ResizableTall. In this layout, the large
  -- master window is at the top, and remaining windows tile at the
  -- bottom of the screen. Can be resized as described above.
  ||| Mirror (spacing 5 (ResizableTall 1 (3/100) (1/2) []))

  -- ThreeColMid layout puts the large master window in the center
  -- of the screen. As configured below, by default it takes of 3/4 of
  -- the available space. Remaining windows tile to both the left and
  -- right of the master window. You can resize using "super-h" and
  -- "super-l".
  -- ||| spacing 5 (ThreeColMid 1 (3/100) (3/4))

  -- Circle layout places the master window in the center of the screen.
  -- Remaining windows appear in a circle around it
  -- ||| spacing 5 (Circle)

  -- Single window with tabs
  -- ||| noBorders simpleTabbed

  -- Grid layout tries to equally distribute windows in the available
  -- space, increasing the number of columns and rows as necessary.
  -- Master window is at top left.
  ||| spacing 5 (Grid)))

  -- Full layout makes every window full screen. When you toggle the
  -- active window, it will bring the active window to the front.
  ||| noBorders (fullscreenFull Full)

-- Here we define some layouts which will be assigned to specific
-- workspaces based on the functionality of that workspace.

-- Video layout should mainly be Kodi fullscreen
videoLayout = noBorders (fullscreenFull Full)

-- The chat layout uses the "IM" layout. We have a roster which takes
-- up 1/8 of the screen vertically, and the remaining space contains
-- chat windows which are tiled using the grid layout. The roster is
-- identified using the myIMRosterTitle variable, and by default is
-- configured for Pidgin, so if you're using something else you
-- will want to modify that variable.
--chatLayout = avoidStruts(spacing 5 (withIM (1%7) (Title myIMRosterTitle) Grid))
gridLayout = spacing 5 $ Grid      
--chatLayout = avoidStruts(withIM (18/100) (Role "MainWindow") gridLayout)
-- Another IM layout, for use with Skype.
--skypeLayout = withIM (1/6) skypeMainWindow Grid
--skypeMainWindow = (And (Resource "skype")(Not (Or (Title "Transferts de fichiers")(Role "ConversationsWindow"))))
--skypeLayout = withIM (1/6) skypeMainWindow Grid
--skypeMainWindow = (And (Resource "skype")(Not (Or (Title "Transferts de fichiers")(Role "ConversationsWindow"))))
skypeLayout = avoidStruts(spacing 5 $ withIM (1%7) skypeRoster gridLayout)

skypeRoster     = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "ConversationsWindow")) `And` (Not (Role "CallWindow"))

-- The GIMP layout uses the ThreeColMid layout. The traditional GIMP
-- floating panels approach is a bit of a challenge to handle with xmonad;
-- I find the best solution is to make the image you are working on the
-- master area, and then use this ThreeColMid layout to make the panels
-- tile to the left and right of the image. If you use GIMP 2.8, you
-- can use single-window mode and avoid this issue.
gimpLayout = smartBorders(avoidStruts(spacing 5 (ThreeColMid 1 (3/100) (3/4))))

-- Here we combine our default layouts with our specific, workspace-locked
-- layouts.
myLayouts =
  onWorkspace "4:Chat" skypeLayout
  $ onWorkspace "9:Pix" gimpLayout
  $ onWorkspace "7:Video" videoLayout
  $ defaultLayouts


{-
  Custom keybindings. In this section we define a list of relatively
  straightforward keybindings. This would be the clearest place to
  add your own keybindings, or change the keys we have defined
  for certain functions.
  It can be difficult to find a good list of keycodes for use
  in xmonad. I have found this page useful -- just look
  for entries beginning with "xK":
  http://xmonad.org/xmonad-docs/xmonad/doc-index-X.html
  Note that in the example below, the last three entries refer
  to nonstandard keys which do not have names assigned by
  xmonad. That's because they are the volume and mute keys
  on my laptop, a Lenovo W520.
  If you have special keys on your keyboard which you
  want to bind to specific actions, you can use the "xev"
  command-line tool to determine the code for a specific key.
  Launch the command, then type the key in question and watch
  the output.
-}

myKeyBindings =
	[
		((myModMask, xK_b), sendMessage ToggleStruts)
		, ((myModMask, xK_a), sendMessage MirrorShrink)
		, ((myModMask, xK_z), sendMessage MirrorExpand)

		, ((myModMask, xK_u), focusUrgent)
		, ((0, xK_F7), spawn "amixer -q -D pulse sset Master toggle")
		, ((0, xK_F8), spawn "amixer -q -D pulse sset Master 10%-")
		, ((0, xK_F9), spawn "amixer -q -D pulse sset Master 10%+")
--		, ((0, xK_F5), spawn "xbacklight -dec 10")
--		, ((0, xK_F6), spawn "xbacklight -inc 10")
		, ((0, xK_F11), spawn "/opt/i3lock-fancy/lock -pf Comic-Sans-MS -- scrot -z")
		, ((myModMask .|. shiftMask, xK_b), spawn myBrowser)
		, ((myModMask .|. shiftMask, xK_T), spawn myEmail)
		, ((myModMask .|. shiftMask, xK_f), spawn myFileMan)
		--    , ((myModMask .|. shiftMask, xK_s), spawn myEditor)
		, ((myModMask .|. shiftMask, xK_d), spawn myPiracy)
		, ((myModMask .|. shiftMask, xK_m), spawn myMusic)
		--, ((myModMask, xK_p), spawn "synapse")
		, ((myModMask, xK_p), spawn "rofi -show drun -modi drun,ssh,run")
		, ((myModMask .|. shiftMask, xK_s), spawn myVolumeControl)
		, ((noModMask, xK_Print), spawn myScreenshooter)
		  -- in conjunction with manageHook, open a small temporary
		  -- floating terminal
		, ((myModMask .|. shiftMask, xK_h), scratchTop)
--		, ((myModMask .|. shiftMask, xK_s), scratchMixer)
		, ((0, xK_F12), scratchTerm)
		-- man page prompt
		--, ((myModMask, xK_Insert), manPrompt myXPConfig)                           -- (24)
		-- add single lines to my NOTES file from a prompt.       -- (25)
		, ((myModMask, xK_Insert), appendFilePrompt myXPConfig "/home/thomas/Documents/notes")
		-- shell prompt.
		--, ((myModMask, xK_Insert), sshPrompt myXPConfig)                         -- (26)
	]
	where
           -- this simply means "find the scratchpad in myScratchPads that is 
           -- named terminal and launch it"
           scratchTerm  = namedScratchpadAction myScratchPads "term"
--           scratchMixer = namedScratchpadAction myScratchPads "mixer"
           scratchTop = namedScratchpadAction myScratchPads "htop"
{-
  Management hooks. You can use management hooks to enforce certain
  behaviors when specific programs or windows are launched. This is
  useful if you want certain windows to not be managed by xmonad,
  or sent to a specific workspace, or otherwise handled in a special
  way.
  Each entry within the list of hooks defines a way to identify a
  window (before the arrow), and then how that window should be treated
  (after the arrow).
  To figure out to identify your window, you will need to use a
  command-line tool called "xprop". When you run xprop, your cursor
  will temporarily change to crosshairs; click on the window you
  want to identify. In the output that is printed in your terminal,
  look for a couple of things: - WM_CLASS(STRING): values in this list of strings can be compared
      to "className" to match windows.
    - WM_NAME(STRING): this value can be compared to "resource" to match
      windows.
  The className values tend to be generic, and might match any window or
  dialog owned by a particular program. The resource values tend to be
  more specific, and will be different for every dialog. Sometimes you
  might want to compare both className and resource, to make sure you
  are matching only a particular window which belongs to a specific
  program.
  Once you've pinpointed the window you want to manipulate, here are
  a few examples of things you might do with that window:
    - doIgnore: this tells xmonad to completely ignore the window. It will
      not be tiled or floated. Useful for things like launchers and
      trays.
    - doFloat: this tells xmonad to float the window rather than tiling
      it. Handy for things that pop up, take some input, and then go away,
      such as dialogs, calculators, and so on.
    - doF (W.shift "Workspace"): this tells xmonad that when this program
      is launched it should be sent to a specific workspace. Useful
      for keeping specific tasks on specific workspaces. In the example
      below I have specific workspaces for chat, development, and
      editing images.
-}

-- Set up a customized manageHook (rules for handling windows on
--   creation)


-- some nice colors for the prompt windows to match the dzen status bar.
myXPConfig = defaultXPConfig                                    -- (23)
    { fgColor = "#cccccc"
    , bgColor = "#154D83"
    }

-- Scratchpads -----------------------------------------------------

scratchpadSize = W.RationalRect (l) (t) (w) (h)
--scratchpadSize = W.RationalRect (1/4) (1/4) (1/2) (1/2)
      where
        -- reusing these variables is ok since they're confined to their own 
        -- where clauses 
        h = 0.4       -- height, 40% 
        w = 1         -- width, 100%
        t = 1 - h     -- bottom edge
        l = 1 - w -- centered left/right
--        h = 1/4       -- height, 10% 
--        w = 1/4         -- width, 100%
--        t = 1/2     -- bottom edge
--        l = 1/2 -- centered left/right
mySPFloat = customFloating $ scratchpadSize
niceRect x y = W.RationalRect x y (1-2*x) (1-2*y)

myScratchPads = [ NS "term" spawnTerm findTerm manageTerm
				, NS "htop" "terminator -T htop -e htop" (title =? "htop") manageTerm
--				, NS "mixer" "pavucontrol" (title =? "pavucontrol") manageTerm
                ]
  where
    spawnTerm  = "terminator" ++ " -T scratchpad"       		-- launch my terminal
    findTerm   = title  =? "scratchpad"               			-- its window will be named "scratchpad" (see above)
    manageTerm = mySPFloat			-- and I'd like it fixed using the geometry above
--    manageTerm = customFloating $ niceRect (1/5) (1/4)			-- and I'd like it fixed using the geometry above

myIgnores       = ["synapse", "stalonetray"]
myFloatsC = ["Save As...","Downloads"]
myFloats        = ["nitrogen", "Spotify"]
myManageHook :: ManageHook

myManageHook = composeAll $
	[
		isFullscreen --> myDoFullFloat
		,	isDialog --> doCenterFloat
	]

	++

	[
		resource    =? r                 --> doIgnore | r <- myIgnores
	]
   
	++

	[
		className   =? c                 --> doFloat | c <- myFloats
		--className =? "nitrogen" --> doFloat
	]

	++

	[
		className    =? c     --> doCenterFloat  |   c   <- myFloatsC
		--className =? "nitrogen" --> doFloat
	]

	++

	[
		(className =? "jetbrains-idea-ce") --> doF (W.shift "1:Dev")
		, (className =? "google-chrome") -->doF (W.shift "3:Web")
		, (className =? "Pidgin") --> doF (W.shift "4:Chat")
		, (className =? "ViberPC") --> doF (W.shift "4:Chat")
		, (className =? "Skype") --> doF (W.shift "4:Chat")
		, (className =? "Thunderbird") --> doF (W.shift "5:Mail")
		, (className =? "Kodi") --> doF (W.shift "7:Video")
		, (className =? "Popcorn Time") --> doF (W.shift "7:Video")
		, (className =? "Spotify") --> doF (W.shift "8:Music")
		, (className =? "Gimp-2.8") --> doF (W.shift "9:Pix")
	]
	
	++

	[
		namedScratchpadManageHook myScratchPads
	]


-- this enables to cover xmobar without pressing ctrl b
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat

{-
  Workspace navigation keybindings. This is probably the part of the
  configuration I have spent the most time messing with, but understand
  the least. Be very careful if messing with this section.
-}

-- We define two lists of keycodes for use in the rest of the
-- keyboard configuration. The first is the list of numpad keys,
-- in the order they occur on the keyboard (left to right and
-- top to bottom). The second is the list of number keys, in an
-- order corresponding to the numpad. We will use these to
-- make workspace navigation commands work the same whether you
-- use the numpad or the top-row number keys. And, we also
-- use them to figure out where to go when the user
-- uses the arrow keys.
{--numPadKeys =
  [
    xK_KP_Home, xK_KP_Up, xK_KP_Page_Up
    , xK_KP_Left, xK_KP_Begin,xK_KP_Right
    , xK_KP_End, xK_KP_Down, xK_KP_Page_Down
    , xK_KP_Insert, xK_KP_Delete, xK_KP_Enter
  ]

numKeys =
  [
    xK_7, xK_8, xK_9
    , xK_4, xK_5, xK_6
    , xK_1, xK_2, xK_3
    , xK_0, xK_minus, xK_equal
  ]--}

-- Here, some magic occurs that I once grokked but has since
-- fallen out of my head. Essentially what is happening is
-- that we are telling xmonad how to navigate workspaces,
-- how to send windows to different workspaces,
-- and what keys to use to change which monitor is focused.
myKeys = myKeyBindings {--++
  [
    ((m .|. myModMask, k), windows $ f i)
       | (i, k) <- zip myWorkspaces numPadKeys
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++
  [
    ((m .|. myModMask, k), windows $ f i)
       | (i, k) <- zip myWorkspaces numKeys
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++
  M.toList (planeKeys myModMask (Lines 4) Finite) ++
  [
    ((m .|. myModMask, key), screenWorkspace sc
      >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [1,0,2]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]
--}



--define our own fadeInactiveLogHook
{--fadeInactiveLogHook' = 
	fadeOutLogHook . fadeIf (propertyToQuery ((Not (ClassName "google-chrome")) 
	`And` (Not (ClassName "Terminator")) 
	`And` (Not (ClassName "Gimp")) 
	`And` (Not (ClassName "Firefox")) 
	`And` (Not (ClassName "Gcolor2"))))
myLogHook = fadeInactiveLogHook' 0.8--}

myLogHook h = dynamicLogWithPP $ defaultPP

    -- display current workspace as darkgrey on light grey (opposite of 
    -- default colors)
    { ppCurrent         = dzenColor "#303030" "#909090" . pad 

  , ppHidden          = dzenColor "#909090" "#606060" . pad . noScratchPad -- haskell makes it so easy,
  , ppHiddenNoWindows = noScratchPad                                -- just tack on another function

    -- display the current layout as a brighter grey
    , ppLayout          = dzenColor "#909090" "" . pad 

    -- if a window on a hidden workspace needs my attention, color it so
    , ppUrgent          = dzenColor "#ff0000" "" . pad . dzenStrip

    -- shorten if it goes over 100 characters
    , ppTitle           = shorten 100

    -- no separator between workspaces
    , ppWsSep           = ""

    -- put a few spaces between each object
    , ppSep             = "  "

    -- output to the handle we were given as an argument
    , ppOutput          = hPutStrLn h
    }
	where
		-- then define it down here: if the workspace is NSP then print
		-- nothing, else print it as-is
		noScratchPad ws = if ws == "NSP" then "" else ws


main = do
  --d <- spawnPipe "dzen2 -p -w 5 -ta l -e 'onstart=lower'"
  --spawn $ "conky -c ~/.xmonad/data/conky/dzen | " ++
  --             "dzen2 -p -xs 0 ta -r -e 'onstart=lower'"
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
    focusedBorderColor = myFocusedBorderColor
  , normalBorderColor = myNormalBorderColor
  , terminal = myTerminal
  , borderWidth = myBorderWidth
  , layoutHook = myLayouts
  , workspaces = myWorkspaces
  , modMask = myModMask
  , XMonad.focusFollowsMouse  = myFocusFollowsMouse
  , XMonad.clickJustFocuses   = myClickJustFocuses
  , handleEventHook = fullscreenEventHook
  , startupHook = do
      setWMName "LG3D" 	--	required for JAVA (e.g. jdownloader). without
                     	--	it menues and clicks and window drawing
                     	--	according to window size do not work
      windows $ W.greedyView startupWorkspace
      spawn "~/.xmonad/startup-hook"
  , manageHook = manageHook defaultConfig
      <+> myManageHook
      <+> manageDocks
  , logHook = 
--myLogHook d
--myLogHook >>
      takeTopFocus <+> dynamicLogWithPP xmobarPP {
        ppOutput = hPutStrLn xmproc
	, ppCurrent = xmobarColor myCurrentColor ""
        , ppHidden = xmobarColor myHiddenColor ""
        , ppVisible = xmobarColor myVisibleWSColor ""
--        , ppHiddenNoWindows = xmobarColor myEmptyColor ""
        , ppUrgent = xmobarColor myUrgentColor "" . xmobarStrip
        , ppLayout = xmobarColor myLayoutColor ""
        , ppWsSep = "  "
        , ppSep = xmobarColor mySepColor "" "   |   "
        , ppTitle = xmobarColor myTitleColor "" . shorten 80 . trim
        , ppSort            = fmap (namedScratchpadFilterOutWorkspace.) getSortByXineramaPhysicalRule
--      ppOutput = hPutStrLn xmproc
--      , ppTitle = xmobarColor myTitleColor "" . shorten myTitleLength
--      , ppCurrent = xmobarColor myCurrentWSColor ""
--        . wrap myCurrentWSLeft myCurrentWSRight
--      , ppVisible = xmobarColor myVisibleWSColor ""
--        . wrap myVisibleWSLeft myVisibleWSRight
--      , ppUrgent = xmobarColor myUrgentWSColor ""
--        . wrap myUrgentWSLeft myUrgentWSRight

    }
--		where 
  			-- then define it down here: if the workspace is NSP then print
    		-- nothing, else print it as-is
--    		noScratchPad ws = if ws == "NSP" then "" else ws

  }
    `additionalKeys` myKeys

{--
-- The main function.
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- Command to launch the bar.
myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { 
		ppVisible = xmobarColor "#FF0000" "", 
		ppCurrent = xmobarColor "#2E9AFE" "", 
		ppTitle = xmobarColor "black" "", 
		ppHiddenNoWindows = xmobarColor "#0404B4" "", 
		ppLayout = xmobarColor "#790a0a" "", 
		ppUrgent = xmobarColor "#525252" "" . wrap "[" "]" }



-- Main configuration, override the defaults to your liking.
myConfig = defaultConfig { 
			 , workspaces = myWorkspaces
			 , keys = myKeys
			 , layoutHook = smartBorders $ myLayoutHook
			 , manageHook = myManageHook <+> manageHook defaultConfig
			 
			 }


--}
