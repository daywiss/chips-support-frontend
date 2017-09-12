module Api exposing (..)

import Messages exposing (..)
import Http  
import Models 

import Json.Decode exposing (string)
import Json.Encode as Encode

-- type Msg = 
--   Echo (Result Http.Error String) 

-- type AnyResponse = 
--   GameResponse Game | GameItemResponse GameItem | UserResponse User

-- host = "http://dadams.info:3335/"
host = "http://api.fiestasupport.dadams.info/"

-- call: String -> Encode.Value -> Message 
-- call action params msg out= 
--   let 
--     url = host ++ action
--     -- body = Http.jsonBody params
--     -- request = Http.post url body out
--   in
--    Http.send msg request

search: String -> Cmd Msg
search input =
  let 
    url = host ++ "searchUnknown"
    params = Encode.encode 0 (Encode.object [("search", (Encode.string input))])
    request = Http.post url (Http.stringBody "application/json" params) Models.searchResult
  in 
     Http.send Messages.SearchResult request
     -- call "searchUnknown" params SearchResult Models.anyResponse

searchUserOrders: String -> Cmd Msg
searchUserOrders input =
  let 
    url = host ++ "searchUserOrders"
    params = Encode.encode 0 (Encode.object [("userid", (Encode.string input))])
    request = Http.post url (Http.stringBody "application/json" params) Models.searchResult
  in 
     Http.send Messages.SearchResult request

getUserBackpack: String -> Cmd Msg
getUserBackpack input =
  let 
    url = host ++ "getUserBackpack"
    params = Encode.encode 0 (Encode.object [("userid", (Encode.string input))])
    request = Http.post url (Http.stringBody "application/json" params) Models.searchResult
  in 
     Http.send Messages.SearchResult request

getWinningGames: String -> Cmd Msg
getWinningGames input =
  let 
    url = host ++ "getWinningGames"
    params = Encode.encode 0 (Encode.object [("userid", (Encode.string input))])
    request = Http.post url (Http.stringBody "application/json" params) Models.searchResult
  in 
     Http.send Messages.SearchResult request

getTradesWithItem: String -> Cmd Msg
getTradesWithItem input =
  let 
    url = host ++ "getTradesWithItem"
    params = Encode.encode 0 (Encode.object [("itemid", (Encode.string input))])
    request = Http.post url (Http.stringBody "application/json" params) Models.searchResult
  in 
     Http.send Messages.SearchResult request

getGamesWithItem: String -> Cmd Msg
getGamesWithItem input =
  let 
    url = host ++ "getGamesWithItem"
    params = Encode.encode 0 (Encode.object [("itemid", (Encode.string input))])
    request = Http.post url (Http.stringBody "application/json" params) Models.searchResult
  in 
     Http.send Messages.SearchResult request

getGamesWithUser: String -> Cmd Msg
getGamesWithUser input =
  let 
    url = host ++ "getGamesWithUser"
    params = Encode.encode 0 (Encode.object [("userid", (Encode.string input))])
    request = Http.post url (Http.stringBody "application/json" params) Models.searchResult
  in 
     Http.send Messages.SearchResult request


getOrderByTrade: String -> Cmd Msg
getOrderByTrade input =
  let 
    url = host ++ "getOrderByTrade"
    params = Encode.encode 0 (Encode.object [("tradeid", (Encode.string input))])
    request = Http.post url (Http.stringBody "application/json" params) Models.searchResult
  in 
     Http.send Messages.SearchResult request

getRake: String -> Cmd Msg
getRake input =
  let 
    url = host ++ "getRake"
    params = Encode.encode 0 (Encode.object [("gameid", (Encode.string input))])
    request = Http.post url (Http.stringBody "application/json" params) Models.searchResult
  in 
     Http.send Messages.SearchResult request

setTradeActive: String -> Cmd Msg
setTradeActive tradeid =
  let 
    url = host ++ "setTradeActive"
    params = Encode.encode 0 (Encode.object [("tradeid", (Encode.string tradeid))])
    request = Http.post url (Http.stringBody "application/json" params) string
  in 
     Http.send Messages.DiscardResult request

getOrdersWithItem: String -> Cmd Msg
getOrdersWithItem itemid =
  let 
    url = host ++ "getOrdersWithItem"
    params = Encode.encode 0 (Encode.object [("itemid", (Encode.string itemid))])
    request = Http.post url (Http.stringBody "application/json" params) Models.searchResult
  in 
     Http.send Messages.SearchResult request
