module Main exposing (..)

import Html  exposing (..)
import Html.Events  exposing (..)
import Html.Attributes  exposing (..)
import Date 
import Time 

import Http exposing (..)
import Dict 
import Models
import Debug exposing (..)
import Json.Decode exposing (string)
import Json.Encode as Encode
import Api
import Messages exposing (Msg)
import Array

type alias Model = 
  {
    -- echo : String,
    -- game : Models.Game,
    -- user : Models.User,
    -- item : Models.GameItem,
    -- response : String,
    history : List Models.SearchResult,
    search: String,
    error: String
  }

goodSearches = Array.fromList [
    "07dc7e5c-99b1-4062-b20f-3eb6de4750f2" -- fiesta order id
    ,"aed44ca1-7619-4820-8ab8-9aeff3a48454" --jackpot id
    ,"869ebd45-1886-4f2a-83ec-70f463f74af7" --bot trade id
  ]

defaultModel = Model
    -- ""
    -- Models.defaultGame
    -- Models.defaultUser
    -- Models.defaultItem
    -- ""
    []
    "" -- fiesta order id
    ""

update: Msg -> Model -> (Model,Cmd Msg)
update message model =
    let 
        msg = log "Message" message
    in
      case msg of 
        Messages.SearchUserOrders userid->
          (model,Api.searchUserOrders userid)
        -- Messages.SearchUserTrades userid->
        --   (model,Api.searchUserOrders userid)

        Messages.InputSearch input->
          ({model | search = input}, Cmd.none)
        Messages.DoSearch ->
          (model , Api.search model.search)
        Messages.SearchLinkClick search->
          ({model | search = search}, (Api.search search))
          -- (model , Api.search model.search)
          -- (model, Cmd.none)
        Messages.SearchResult (Ok result) ->
          ({model | history = result :: model.history},Cmd.none)
        Messages.SearchResult (Result.Err error) ->
          ({model | error = (toString error)} , Cmd.none)
        Messages.GetUserBackpack userid ->
          (model , Api.getUserBackpack userid)
        Messages.GetWinningGames userid ->
          (model , Api.getWinningGames userid)
        Messages.GetGamesWithUser userid ->
          (model , Api.getGamesWithUser userid)
        Messages.GetGamesWithItem itemid ->
          (model , Api.getGamesWithItem itemid)
        Messages.GetGameOrderByTrade tradeid ->
          (model , Api.getOrderByTrade tradeid)
        Messages.GetTradesWithItem itemid ->
          (model , Api.getTradesWithItem itemid)
        Messages.SetTradeActive tradeid ->
          (model, Api.setTradeActive tradeid )
        Messages.GetRake gameid ->
          (model, Api.getRake gameid)
        Messages.ClearHistory->
          ({model | history = [] }, Cmd.none)
        Messages.GetOrdersWithItem itemid->
          (model, Api.getOrdersWithItem  itemid)
        Messages.DiscardResult (Ok result)->
          (model,Cmd.none)
        Messages.DiscardResult (Result.Err error)->
          (model,Cmd.none)

        

init = 
  ( 
  -- ,call "getJackpot" """{"id":"aed44ca1-7619-4820-8ab8-9aeff3a48454"}""" Messages.Jackpot Models.game
  defaultModel,
  Cmd.none
  )

subscriptions model = Sub.none

showDate: Int -> String
showDate ts =
  let
    date = Date.fromTime (toFloat ts)
  in
     if ts <= 0 then
        "-"
     else
      toString (Date.month date) 
      ++ " " ++ toString (Date.day date) 
      ++ " " ++ toString (Date.year date) 
      ++ " " ++ toString (Date.hour date) 
      ++ ":" ++ toString (Date.minute date)

view: Model -> Html Msg
view model = 
  div [class "container"] [
    label [][text "Search"]
    ,input [class "u-full-width", placeholder "Search", onInput Messages.InputSearch, value model.search][]
    ,button [ onClick Messages.DoSearch][text "Search"]
    ,button [ onClick Messages.ClearHistory][text "Clear Results"]
    ,viewHistory model
    -- viewGame model.game
  ]

emptyView item =
  div[][text (toString item) ]    

-- chooseView: Models.SearchResult Html Msg
chooseView: Models.SearchResult -> Html Msg
chooseView historyItem =
  case historyItem.result of
    Models.GameResponse game ->
      viewGame game
    Models.TradeResponse trade ->
      viewTrade trade
    Models.TradesResponse trades ->
      viewTradeList trades
    Models.GameOrderResponse order ->
      viewGameOrder order
    Models.GameOrdersResponse orders ->
      viewGameOrderList orders
    Models.GameItemResponse item ->
      viewGameItem item
    Models.BotResponse bot ->
      viewBot bot
    Models.UserResponse user ->
      viewUser user
    Models.GameRakeResponse rake ->
      viewRake rake
    Models.BackpackResponse backpack ->
      viewBackpack backpack
    Models.GamesResponse games ->
      viewGames games
    _ ->
      emptyView historyItem.result
    -- Models.User ->
    --   emptyView historyItem.result
    -- Models.GameItem ->
    --   emptyView historyItem.result

viewHistory: Model -> Html Msg
viewHistory model = 
  div[] (List.intersperse viewDivider (List.map chooseView model.history ))

viewDivider =
  hr[style [("borderWidth","2")]][]

viewGames: List Models.Game -> Html Msg
viewGames games = 
  div[][
    text (toString (List.length games) ++ " Games Found")
    ,div[] (List.map viewGame games)
  ]

viewGame: Models.Game -> Html Msg
viewGame game =
  div[][
    text "Game Data"
    ,div[][
      a[ onClick (Messages.GetRake game.id), href "#"][text "View Game Rake"]
    ]
    ,table[class "u-full-width"][
      thead[][
        tr[][
          th[][text "id"],
          th[][text "Type"],
          th[][text "Config Name"],
          th[][text "Updated"],
          th[][text "Winner"]
        ]
      ],
      tbody[] [
        tr[][
          td[][
            makeLink game.id
          ],
          td[][
            text game.gameType
          ],
          td[][
            text (Maybe.withDefault "-" game.config.name)
          ],
          td[][
            text (showDate game.created)
          ],
          td[][
            makeLink game.winner
          ]
        ]
      ]
    ]
    ,viewItemsTable game.items
    ,viewUsers game.players
  ]

viewGameItem item = 
  div[][
    text "Item"
    ,div[][
      a[ onClick (Messages.GetOrdersWithItem item.id), href "#"][text "Get Game Orders With Item"]
    ]
    ,div[][
      a[ onClick (Messages.GetGamesWithItem item.id), href "#"][text "Get Games With Item"]
    ]
    ,div[][
      a[ onClick (Messages.GetTradesWithItem item.id), href "#"][text "Get Trades With Item"]
    ]
    ,table[class "u-full-width"][
      thead[][
        tr[][
          th[][text "id"]
          ,th[][text "Image"]
          ,th[][text "Name"]
          ,th[][text "Price"]
          ,th[][text "Location"]
          ,th[][text "BotID"]
          ,th[][text "SteamID"]
          ,th[][text "TradeID"]
        ]
      ],
      tbody[] [viewItemRow item]
    ]
  ]


viewItemsTable: Models.GameItems -> Html Msg
viewItemsTable items = 
  table[class "u-full-width"][
    thead[][
      tr[][
        th[][text "id"]
        ,th[][text "Image"]
        ,th[][text "Name"]
        ,th[][text "Price"]
        ,th[][text "Location"]
        ,th[][text "BotID"]
        ,th[][text "SteamID"]
        ,th[][text "TradeID"]
      ]
    ],
    tbody[] (List.map viewItemRow (Dict.values items) ) 
  ]

viewItemRow: Models.GameItem -> Html Msg
viewItemRow item = 
  tr [][
    td[][
      makeLink item.id
    ]
    ,td[][
      img[src item.imageURL, height 32][]
    ],
    td[][
      text item.marketHashName
    ]
    ,td[][
      text (toString (Maybe.withDefault 0 item.price))
    ]
    ,td[][
      text item.itemType
    ]
    ,td[][
      -- makeLink item.botid
    ]
    ,td[][
      makeLink (Maybe.withDefault "" item.steamid)
    ]
    ,td[][
      makeLink (Maybe.withDefault "" item.tradeid)
    ]
  ]


viewTradeRow: Models.Trade -> Html Msg
viewTradeRow trade =
  tr[][
    td[][makeLink trade.id],
    td[][text trade.tradeType],
    td[][makeLink trade.messageid],
    td[][ makeLink trade.itemid ],
    -- td[][ text trade.frombotid ],
    td[][ makeLink trade.tosteamid ],
    td[][text (showDate trade.created)]
    ,td[][text (Maybe.withDefault "-" (Maybe.withDefault Models.defaultTradeState (Dict.get "Done" trade.history )).previous)]
  ]

viewTrade trade = 
  div[][
    text "Bot Trade"
    ,div[][
      a[ onClick (Messages.GetGameOrderByTrade trade.id), href "#"][text "Get Game Order"]
    ]
    ,div[][
      a[ onClick (Messages.SetTradeActive trade.id), href "#"][text "Reset Trade to Active"]
    ]
    ,viewTradeList [trade]
    ,viewTradeHistory trade.history
  ]

viewTradeHistory: Models.TradeHistory -> Html Msg
viewTradeHistory history =
  table[class "u-full-width"][
    thead[][
      tr[][
        th[][text "State"]
        ,th[][text "Created"]
        ,th[][text "Ended"]
        ,th[][text "Previous"]
        ,th[][text "Reason"]
        ,th[][text "Error"]
      ]
    ],
    tbody[] (List.map viewTradeState (Dict.toList history))
  ]

viewTradeState: (String,Models.TradeState) -> Html Msg
viewTradeState (name,state)= 
  tr[][
    td[][text name]
    ,td[][text (showDate state.start)]
    ,td[][text (showDate (Maybe.withDefault 0 state.end ))]
    ,td[][text (Maybe.withDefault "" state.previous)]
    ,td[][text (Maybe.withDefault "" state.reason)]
    ,td[][text (Maybe.withDefault "" state.error)]
  ]

viewGameOrderList: List Models.GameOrder -> Html Msg
viewGameOrderList orders =
  div[][
    text (toString (List.length orders) ++ " Game orders")
    ,div[](List.map viewGameOrder orders)
  ]


viewGameOrder: Models.GameOrder -> Html Msg
viewGameOrder order =
  div[][
    text "Game Order"
    ,table[class "u-full-width"][
      thead[][
        tr[][
          th[][text "id"],
          th[][text "userid"],
          th[][text "action"],
          th[][text "tradeurl"]
          ,td[][text "Created"]
          ,td[][text "gameid"]
          ,td[][text "onComplete"]
          ,td[][text "onFailure"]
        ]
      ],
      tbody[][
        tr[][
          td[][text order.id]
          ,td[][makeLink order.userid]
          ,td[][text order.action]
          ,td[][text (Maybe.withDefault "-" order.tradeurl)]
          ,td[][text (showDate order.created)]
          ,td[][makeLink (Maybe.withDefault "-" order.params.gameid)]
          ,td[][text (Maybe.withDefault "-" order.params.onComplete)]
          ,td[][text (Maybe.withDefault "-" order.params.onFailure)]
        ]
      ]
    ]
    ,viewTradeList (Maybe.withDefault [] order.trades)
    ,viewItemList (Maybe.withDefault [] order.items)
  ]
viewItemList: List Models.GameItem -> Html Msg
viewItemList items=
  div[][
    text ("Items: " ++ (toString (List.length items)))
    ,table[class "u-full-width"][
      thead[][
        tr[][
          th[][text "id"]
          ,th[][text "Image"]
          ,th[][text "Name"]
          ,th[][text "Price"]
          ,th[][text "Location"]
          ,th[][text "BotID"]
          ,th[][text "SteamID"]
          ,th[][text "TradeID"]
        ]
      ],
      tbody[] (List.map viewItemRow items)
    ]
  ]
  

viewTradeList: List Models.Trade -> Html Msg
viewTradeList trades=
  div[][
    text ("Trades: " ++ (toString (List.length trades)))
    ,table[][
      thead[][
        tr[][
          th[][text "Tradeid"]
          ,th[][text "Type"]
          ,th[][text "MessageID"]
          ,th[][text "Itemid"]
          -- ,th[][text "From Bot"]
          ,th[][text "To SteamID"]
          ,th[][text "Date"]
          ,th[][text "Previous State"]
        ]
      ]
      ,tbody[](List.map viewTradeRow trades)
    ]
  ]
  
viewBot: Models.Bot -> Html Msg
viewBot bot= 
  div[][
    text "Bot Data"
    ,table[class "u-full-width"][
      thead[][
        tr[][
          th[][text "id"]
         ,th[][text "username"]
         ,th[][text "clusterid"]
         ,th[][text "state"]
         ,th[][text "desired state"]
         ,th[][text "tradeurl"]
         ,th[][text "steamid"]
         ,th[][text "asset count"]
          -- th[][text "onComplete"]
        ]
      ],
      tbody[][
        tr[][
          td[][text bot.id],
          td[][text bot.username],
          td[][text bot.clusterid],
          td[][text bot.state],
          td[][text bot.desiredState],
          td[][text bot.tradeurl],
          td[][text bot.steamid],
          td[][text (toString bot.assetCount)]
        ]
      ]
    ]
    
  ]

viewUsers: Models.Users -> Html Msg
viewUsers users =
  div[][
    text "User Data"
    ,table[class "u-full-width"][
      thead[][
        tr[][
          th[][text "id"]
         ,th[][text "avatar"]
         ,th[][text "username"]
         ,th[][text "tradeurl"]
         ,th[][text "steamid"]
         ,th[][text "profile"]
        ]
      ],
      tbody[] (List.map viewUserRow users)
    ]
  ]
      


viewUserRow: Models.User -> Html Msg
viewUserRow user =
  let
      tradeurl = (Maybe.withDefault "" user.tradeurl)
      steamid = (Maybe.withDefault Models.defaultSteamInfo user.steam).steamid
      avatar = (Maybe.withDefault (Maybe.withDefault Models.defaultSteamInfo user.steam).avatar user.avatar).small
      -- avatar = (Maybe.withDefault (Maybe.withDefault Models.defaultSteamAvatar user.steam)
      profile = (Maybe.withDefault Models.defaultSteamInfo user.steam).profile
  in
    tr[][
      td[][makeLink user.id]
      ,td[][
        img[src avatar, height 64][]
      ]
      ,td[][text user.username]
      ,td[][a[href tradeurl, target "_blank"][text tradeurl]]
      ,td[][(makeLink steamid)]
      ,td[][a[href profile, target "_blank"][text profile]]
    ]

viewUser: Models.User -> Html Msg
viewUser user =
  div[][
    text "User Data"
    ,div[][
      a[onClick (Messages.SearchUserOrders user.id), href "#"][text "View Latest Orders"]
    ]
    ,div[][
      a[ onClick (Messages.GetUserBackpack user.id), href "#"][text "View Backpack"]
    ]
    ,div[][
      a[ onClick (Messages.GetWinningGames user.id), href "#"][text "View Latest Game Wins"]
    ]
    ,div[][
      a[ onClick (Messages.GetGamesWithUser user.id), href "#"][text "View All Games With User"]
    ]
    ,table[class "u-full-width"][
      thead[][
        tr[][
          th[][text "id"]
         ,th[][text "avatar"]
         ,th[][text "username"]
         ,th[][text "tradeurl"]
         ,th[][text "steamid"]
         ,th[][text "profile"]
        ]
      ],
      tbody[][
        tr[][
          td[][text user.id]
          ,td[][
            img[src (Maybe.withDefault Models.defaultSteamInfo user.steam).avatar.small, height 64][]
          ]
          ,td[][text user.username]
          ,td[][a[href (Maybe.withDefault "" user.tradeurl), target "_blank"][text (Maybe.withDefault "" user.tradeurl)]]
          ,td[][(makeLink (Maybe.withDefault Models.defaultSteamInfo user.steam).steamid)]
          ,td[][a[href (Maybe.withDefault Models.defaultSteamInfo user.steam).profile, target "_blank"][text (Maybe.withDefault Models.defaultSteamInfo user.steam).profile]]
        ]
      ]
    ]
  ]

viewRake: Models.GameRake -> Html Msg
viewRake rake = 
  div[][
    text "Game Rake"
    ,table[class "u-full-width"][
      thead[][
        tr[][
          th[][text "id"]
         ,th[][text "gameType"]
         ,th[][text "winnerid"]
        ]
      ],
      tbody[][
        tr[][
          td[][text rake.id]
          ,td[][text rake.gameType]
          ,td[][makeLink rake.payout.userid]
        ]
      ]
    ]
    ,text ("Payout Items: " ++ (toString (List.length rake.payout.items)))
    ,table[][
      tbody[](List.map viewItemRow rake.payout.items)
    ]
    ,text ("Raked Items: " ++ (toString (List.length rake.rake)))
    ,table[][
      tbody[](List.map viewItemRow rake.rake)
    ]
  ]

viewBackpack: Models.Backpack -> Html Msg
viewBackpack backpack =
  div[][
    text "User Backpack"
    ,table[class "u-full-width"][
      thead[][
        tr[][
          th[][text "id"]
         ,th[][text "userid"]
        ]
      ],
      tbody[][
        tr[][
          td[][text backpack.id]
          ,td[][makeLink backpack.id]
        ]
      ]
    ]
    ,viewItemList (Dict.values backpack.items)
  ]

makeLink search =
  if (String.length search > 0) then
    a[onClick (Messages.SearchLinkClick search), href "#"][text search] 
  else
    text "-"

main = 
  Html.program {
    init = init,
    view = view,
    update = update,
    subscriptions = subscriptions
  }
