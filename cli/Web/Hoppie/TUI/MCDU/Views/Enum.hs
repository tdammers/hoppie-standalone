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
  deriving (Show, Eq, Ord)
