module Web.Hoppie.TUI.MCDU.Views
( module M
, defResolveViewID
)
where

import Web.Hoppie.TUI.MCDU.Views.Enum as M
import Web.Hoppie.TUI.MCDU.Views.MainMenu as M
import Web.Hoppie.TUI.MCDU.Views.Config as M
import Web.Hoppie.TUI.MCDU.Views.Messages as M
import Web.Hoppie.TUI.MCDU.Views.Status as M
import Web.Hoppie.TUI.MCDU.Views.FGFS as M

import Web.Hoppie.TUI.MCDU.Monad (MCDUView (..))

defResolveViewID :: ViewID -> MCDUView
defResolveViewID MainMenuView = mainMenuView
defResolveViewID StatusView = statusView
defResolveViewID ConfigView = configView
defResolveViewID (MessageView uid) = messageView uid
defResolveViewID DLKMenuView = dlkMenuView
defResolveViewID ATCMenuView = atcMenuView
defResolveViewID FPLView = fplView
defResolveViewID RTEView = rteView
defResolveViewID NAVView = navView
defResolveViewID PROGView = progView
