import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.ResizableTile
import XMonad.Actions.CycleWS
import System.IO

myManageHook = composeAll
  [ className =? "Gimp"   --> doFloat
  ]

myLayout = ResizableTall 1 (3/100) (1/2) []

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ defaultConfig
    { manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
    , layoutHook = avoidStruts $ layoutHook defaultConfig ||| myLayout
    , logHook = dynamicLogWithPP $ xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor "green" "" . shorten 50
      }
    , modMask = mod4Mask
    , terminal = "urxvt"
    , borderWidth = 3
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
        ]
