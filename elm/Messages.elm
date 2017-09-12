module Messages exposing(..)

import Http 
import Models

type Msg = 
  InputSearch String 
  | DoSearch 
  | SearchResult (Result Http.Error Models.SearchResult)
  | SearchLinkClick String
  | SearchUserOrders String
  | GetUserBackpack String
  | GetWinningGames String
  | GetGamesWithUser String
  | GetGamesWithItem String
  | GetTradesWithItem String
  | GetGameOrderByTrade String
  | GetRake String
  | SetTradeActive String
  | ClearHistory 
  | GetOrdersWithItem String
  | DiscardResult (Result Http.Error String)
  -- Echo (Result Http.Error String) |
  -- Jackpot (Result Http.Error Models.Game) |



