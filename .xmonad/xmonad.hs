import Data.Ratio ((%))
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.ResizableTile
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.WindowBringer
import qualified XMonad.StackSet as W
import XMonad.Layout.NoBorders
import System.IO

myManageHook = composeAll
  [ className =? "Gimp"   --> doFloat
    , className =? "Chromium" --> doF (W.shift "3:web")
    , className =? "Lanikai" --> doShift "5:mail"
    , className =? "Skype" --> doShift "4:skype"
    , className =? "Xchat" --> doShift "9:chat"
  ]

myLayout = ResizableTall 1 (3/100) (1/2) []
imLayout = avoidStruts $ IM (1%7) (Title "manukall - Skype™ (Beta)")
tabbedLayout = tabbed shrinkText tabbedConf
 
tabbedConf = defaultTheme {
    fontName = "xft:Terminus"
}

genericLayouts = avoidStruts $
                 smartBorders $
                 {-toggleLayouts (noBorders Full) $-}
                 Mirror tiled ||| tiled ||| tabbedLayout ||| (noBorders Full)
  where
      tiled = Tall 1 (3 / 100) (3 / 4)

myLayouts = onWorkspace "4:skype" imLayout $
            genericLayouts


myWorkspaces    = ["1:main","2:work","3:web","4:skype","5:mail", "6:media", "7", "8", "9:chat"]

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ defaultConfig
    { manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
    , layoutHook = myLayouts
    , logHook = dynamicLogWithPP $ xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor "green" "" . shorten 50
      }
    , modMask = mod4Mask
    , terminal = "urxvt"
    , borderWidth = 2
    , normalBorderColor = "#CCCCC6"
    , focusedBorderColor = "#fd971f"
    , workspaces = myWorkspaces
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
        ]
