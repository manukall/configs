import Data.Ratio ((%))
import XMonad
import qualified XMonad.Actions.DynamicWorkspaces as DW
import qualified XMonad.Actions.WithAll as WithAll
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.ResizableTile
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ToggleLayouts
import qualified XMonad.Layout.WorkspaceDir as WD
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.WindowBringer
import qualified XMonad.StackSet as W
import XMonad.Layout.NoBorders
import System.IO
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ICCCMFocus
import qualified XMonad.Prompt 		as P
import qualified XMonad.Prompt.Input as PI
import XMonad.Prompt.Shell
import XMonad.Prompt

import qualified Data.Map as M

{-colors-}
gray = "#c4c4c4"
green = "#46772d"
red = "#880b32"

myManageHook = composeAll
  [ className =? "Gimp"   --> doFloat
    , className =? "Chromium" --> doF (W.shift "3:web")
    , className =? "Firefox" --> doF (W.shift "3:web")
    , className =? "Thunderbird" --> doShift "5:mail"
    , className =? "Skype" --> doShift "4:skype"
    , className =? "Xchat" --> doShift "9:chat"
    , className =? "Volwheel" --> doFloat
    , className =? "Vlc" --> doFloat
    , isDialog --> doFloat
    , isFullscreen --> doFullFloat
  ]

myLayout = ResizableTall 1 (3/100) (1/2) []
imLayout = avoidStruts $ IM (1%7) (Title "manukall - Skype™ (Beta)")
tabbedLayout = tabbed shrinkText tabbedConf
 
tabbedConf = defaultTheme {
    fontName = "xft:Terminus"
}

genericLayouts = avoidStruts $
                 smartBorders $
                 toggleLayouts (noBorders Full) $
                 Mirror tiled ||| tiled ||| tabbedLayout ||| (noBorders Full)
  where
      tiled = Tall 1 (3 / 100) (3 / 4)

myLayouts = onWorkspace "4:skype" imLayout $
            onWorkspace "3:web" (avoidStruts $ tabbedLayout ||| Mirror tiled ) $
            genericLayouts
  where
      tiled = Tall 1 (3 / 100) (3 / 4)


myWorkspaces    = ["1:main","2:work","3:web","4:skype","5:mail", "6:media", "7", "8", "9:chat"]

-- some nice colors for the prompt windows to match the dzen status bar.
myXPConfig = defaultXPConfig                                    
    { 
	font  = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-u" 
	,fgColor = gray
	, bgColor = "#000000"
	, bgHLight    = "#000000"
	, fgHLight    = green
	, position = Top
    }
 
shift = windows . W.shift

manuPP :: PP
manuPP = xmobarPP
              { 
                ppCurrent = xmobarColor green "" . wrap "[" "]"
              , ppTitle = xmobarColor green "" . shorten 50
              , ppUrgent = xmobarColor gray red . xmobarStrip
              , ppHiddenNoWindows = showNamedWorkspaces
              }
  where showNamedWorkspaces wsId = if any (`elem` wsId) ['a'..'z']
                           then pad wsId
                           else ""


main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ withUrgencyHook NoUrgencyHook
        $ defaultConfig
          { manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
          , layoutHook = myLayouts
          , logHook = do
            dynamicLogWithPP $ manuPP 
              { ppOutput = hPutStrLn xmproc
              }
            takeTopFocus
          , modMask = mod4Mask
          , terminal = "urxvt"
          , borderWidth = 3
          , normalBorderColor = gray
          , focusedBorderColor = red
          , workspaces = myWorkspaces
          , startupHook = setWMName "LG3D"
          }`additionalKeys`
              [ ((mod4Mask,               xK_a), sendMessage MirrorShrink)
              , ((mod4Mask,               xK_y), sendMessage MirrorExpand)
              , ((mod4Mask,               xK_Right), nextWS)
              , ((mod4Mask .|. shiftMask, xK_Right), shiftToNext >> nextWS)
              , ((mod4Mask,               xK_Left), prevWS)
              , ((mod4Mask .|. shiftMask, xK_Left), shiftToPrev >> prevWS)
              , ((mod4Mask,               xK_Up), prevScreen)
              , ((mod4Mask .|. shiftMask, xK_Up), shiftPrevScreen >> prevScreen)
              , ((mod4Mask,               xK_Down), nextScreen)
              , ((mod4Mask .|. shiftMask, xK_Down), shiftNextScreen >> nextScreen)

              , ((mod4Mask,               xK_g), goToSelected defaultGSConfig)

              , ((mod4Mask,               xK_w), gotoMenu)

              , ((mod4Mask,               xK_f), sendMessage ToggleLayout)
              , ((mod4Mask,               xK_p), shellPrompt myXPConfig)
              , ((mod4Mask .|. shiftMask, xK_n), PI.inputPrompt P.defaultXPConfig "New Workspace:" PI.?+ newWorkspace)
              , ((mod4Mask .|. shiftMask, xK_BackSpace), WithAll.killAll >> DW.removeWorkspace) --buggy, messes with focus and creates flicker, needs to be fixed
              , ((mod4Mask .|. shiftMask, xK_r), DW.renameWorkspace P.defaultXPConfig)
              ]

newWorkspace :: WorkspaceId -> X ()
newWorkspace w = do exists <- widExist w
                    if (not exists) then DW.addWorkspace w else return ()
 
widExist :: WorkspaceId -> X Bool
widExist wid = do xs <- get
                  return $ widExists wid ( windowset xs )
 
widExists :: WorkspaceId -> W.StackSet WorkspaceId l a s sd -> Bool
widExists wid ws = wid `elem` map W.tag  (W.workspaces ws)
 
