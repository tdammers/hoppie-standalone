module Web.Hoppie.TUI.MCDU.Views.Enum
where

data ViewID
  = MainMenuView
  | StatusView
  | ConfigView
  | MessageView Word
  | DLKMenuView
  | ATCMenuView
  | FPLView
  | RTEView
  | NAVView
  | PROGView
  | INITView
  deriving (Show, Eq, Ord)
