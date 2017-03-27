import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Spacing
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig (additionalKeys, removeKeys)
import System.IO
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified GHC.IO.Handle.Types as H

-- Basic
myNormalBorderColor = bgColor
myFocusBorderColor = "#ee0000"
myModMask = mod1Mask
myTerminal = "gnome-terminal"
myBorderWidth = 1

myLogHook h = do
	dynamicLogWithPP $ myPP h

myPP :: Handle -> PP
myPP h = defaultPP
	{ ppOutput 		= hPutStrLn h
	, ppCurrent	    = dzenColor dzenFGColor dzenBGColor . pad . wrap space space
	, ppVisible	    = dzenColor highlightColor dzenBGColor . pad . wrap space space
	, ppHidden	    = dzenColor highlightColor dzenBGColor . pad . wrap space space
	, ppHiddenNoWindows = dzenColor highlightColor dzenBGColor . pad . wrap space space
	, ppWsSep	    = ""
	, ppSep		    = ""
	, ppLayout 		= wrap "^ca(1,xdotool key alt+space)" "^ca()" . dzenColor dzenFGColor highlightColor . pad . wrap space space .
	( \t -> case t of
	"Spacing 0 Grid"		-> cmdLayoutIcon ++ "grid.xbm)"
	"Spacing 0 Tall"		-> cmdLayoutIcon ++ "sptall.xbm)"
	"Mirror Spacing 0 Tall"	-> cmdLayoutIcon ++ "mptall.xbm)"
	"Full"					-> cmdLayoutIcon ++ "full.xbm)"
	)					
	, ppOrder  		= \(ws:l:t:_) -> [l,ws]
	}
cmdLayoutIcon = "^ca(1,xdotool key alt+space)^i(/home/neo/.xmonad/icons/"

myWorkspace :: [String]
myWorkspace = clickable . (map dzenEscape) $ [ "Main"
	, "Learn"
	, "Misc"
	--, "よん"
	--, "ご"
	--, "ろく"
	--, "なの"
	--, "はち"
	]
	where clickable l = [ "^ca(1,xdotool key alt+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
		(i,ws) <- zip [1..] l,
		let n = i ]

myKeys = [ ((mod1Mask, xK_d), spawn dmenu) 
	, ((mod1Mask, xK_F12), spawn "scrot /home/neo/scrot/screen_%Y-%m-%d-%H-%M-%S.png -d 1")
	, ((mod1Mask, xK_q), spawn "killall dzen2; xmonad --recompile; xmonad --restart")
    , ((mod1Mask, xK_v), spawn "firefox")]

myLayout = avoidStruts $ smartBorders ( myTall ||| Mirror myTall ||| myGrid ||| Full )
	where 
	 myTall = spacing 0 $ Tall 1 (3/100) (1/2)
	 myGrid = spacing 0 $ Grid

myDocks = composeAll 
	[ className =? "code" --> doFloat
	, className =? "firefox" --> doFullFloat 
	, className =? "google-chrome" --> doFullFloat 
	]

-- About "LG3D" see http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Hooks-SetWMName.html for detail
myStartupHook = spawnOnce "sh /home/neo/.xmonad/scripts/startup.sh"
				<+> setWMName "LG3D"

main = do
	barAtas <- spawnPipe dzenBar
	barAtasKanan <- spawnPipe dzenBarKanan
	xmonad $ defaultConfig
		{ manageHook = myDocks <+> manageDocks <+> manageHook defaultConfig
		, layoutHook = myLayout
		, modMask = myModMask
		, workspaces = myWorkspace
		, terminal = myTerminal
		, focusedBorderColor = myFocusBorderColor
		, normalBorderColor = myNormalBorderColor
		, borderWidth = myBorderWidth
		, startupHook = myStartupHook
		, logHook = myLogHook barAtas
		} 
		`additionalKeys` myKeys
		`removeKeys` [(mod1Mask, xK_p)]

-- Utilities
space = "   "
bgColor = "#2d2d2d"
highlightColor = "#7d7d7d"
font = "Pragmata"

dzenHeight = "24"
dzenWidth1 = "750"
dzenWidth2 = "1280"
dzenFGColor = "#fcfcfc"
dzenBGColor = bgColor
dzenBar = "dzen2 -dock -p -ta l -e 'button3=' -fn '" ++ font ++ "-8:bold' -fg '" ++ dzenFGColor ++ "' -bg '" ++ dzenBGColor ++ "' -h " ++ dzenHeight ++ " -w " ++ dzenWidth1
dzenBarKanan = "conky -c ~/.xmonad/scripts/conky.conf | dzen2 -dock -p -ta r -e 'button3=' -fn 'Pragmata-8' -fg '" ++ dzenFGColor ++ "' -bg '" ++ dzenBGColor ++ "' -h " ++ dzenHeight ++ " -w " ++ dzenWidth2 ++ " -x " ++ dzenWidth1

dmenuFGColor = "#F33C58"
dmenuBGColor = bgColor
dmenu = "dmenu_run -b -nb '" ++ dmenuBGColor ++ "' -sb '" ++ dmenuFGColor ++ "'"
