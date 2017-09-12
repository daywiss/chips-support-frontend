module Models exposing (..)
import Dict exposing(Dict)
import Json.Decode exposing (..)
-- import Json.Decode.Extra exposing (fromResult)

type alias Backpack = {
  id: String
  ,items:Dict String GameItem
}

backpack: Decoder Backpack
backpack = map2 Backpack
  (field "id" string)
  (field "items" (dict gameItem))

type alias Payout = {
  items:List GameItem
  ,userid:String
}

payout: Decoder Payout
payout = map2 Payout
  (field "items" (list gameItem))
  (field "userid" string)

type alias GameRake = {
  id:String
  ,gameType:String
  ,payout:Payout
  ,rake:List GameItem
}
gameRake: Decoder GameRake
gameRake = map4 GameRake
  (field "id" string)
  (field "type" string)
  (field "payout" payout)
  (field "rake" (list gameItem))


type alias Bot = {
  id: String
  ,clusterid: String
  ,desiredState: String
  ,state: String
  ,username: String
  ,tradeurl: String
  ,steamid: String
  ,assetCount: Int
}

bot: Decoder Bot
bot = map8 Bot
  (field "id" string)
  (field "clusterid" string)
  (field "desiredState" string)
  (field "state" string)
  (field "username" string)
  (field "tradeurl" string)
  (field "steamid" string)
  (field "assetCount" int)

type alias GameOrderParams = {
  onComplete: Maybe String
  ,onFailure: Maybe String
  ,gameid: Maybe String
}

gameOrderParams: Decoder GameOrderParams
gameOrderParams = map3 GameOrderParams
  (maybe (field "onComplete" string))
  (maybe (field "onFailure" string))
  (maybe (field "gameid" string))

type alias GameOrder = {
  id: String
  ,userid: String
  ,action: String
  ,created: Int
  ,params: GameOrderParams
  ,tradeurl: Maybe String
  ,items: Maybe (List GameItem)
  ,trades: Maybe (List Trade)
}

gameOrder: Decoder GameOrder
gameOrder = map8 GameOrder
  (field "id" string)
  (field "userid" string)
  (field "action" string)
  (field "created" int)
  (field "params" gameOrderParams)
  (maybe(field "tradeurl" string))
  (maybe (field "items" (list gameItem)))
  (maybe (field "trades" (list trade)))

type alias TradeState = {
  start: Int
  ,end: Maybe Int
  ,reason:Maybe String 
  ,error:Maybe String 
  ,previous:Maybe String 
}
tradeState : Decoder TradeState
tradeState = map5 TradeState
  (field "start" int)
  (maybe (field "end" int))
  (maybe (field "reason" string))
  (maybe (field "error" string))
  (maybe (field "previous" string))

defaultTradeState = TradeState 0 (Just 0) (Just "-") (Just "-") (Just "-")

type alias TradeHistory =  Dict String TradeState 

tradeHistory : Decoder TradeHistory
tradeHistory = dict tradeState

type alias Trade= {
  id : String,
  itemid: String,
  created: Int,
  history: TradeHistory,
  messageid: String
  ,tradeType: String
  ,frombotid: String
  ,tosteamid: String
}
trade: Decoder Trade
trade= map8 Trade
    (field "id" string)
    (field "itemid" string)
    (field "created" int)
    (field "history" tradeHistory)
    (field "messageid" string)
    (field "type" string)
    (field "frombotid" string)
    (field "tosteamid" string)

type alias SteamAvatar = {
  small : String
}

steamAvatar : Decoder SteamAvatar
steamAvatar = map SteamAvatar
    (field "small" string)

type alias SteamInfo = {
  username : String,
  steamid : String
  ,profile : String
  ,avatar : SteamAvatar
}

steamInfo : Decoder SteamInfo
steamInfo = map4 SteamInfo
    (field "username" string)
    (field "steamid" string)
    (field "profile" string)
    (field "avatar" steamAvatar)

type alias User = {
  id : String
  ,username : String
  ,tradeurl : Maybe String
  ,steam : Maybe SteamInfo
  ,avatar : Maybe SteamAvatar
}

user = map5 User
    (field "id" string)
    (field "username" string)
    (maybe (field "tradeurl" string))
    (maybe (field "steam" steamInfo))
    (maybe (field "avatar" steamAvatar))

type alias Users = List User

users = list user

type alias GameConfig = {
  name: Maybe String
}

gameConfig = map GameConfig
    (maybe (field "name" string))

type alias GameItem = {
  id: String
  ,imageURL: String
  ,itemType: String
  ,marketHashName: String
  ,price: Maybe Float
  -- ,botid: String
  ,tradeid: Maybe String
  ,updated: Maybe Int
  ,steamid: Maybe String
}

gameItem = map8 GameItem
    (field "id" string)
    (field "imageURL" string)
    (field "type" string)
    (field "market_hash_name" string)
    (maybe (field "price" float))
    -- (field "botid" string)
    (maybe (field "tradeid" string))
    (maybe (field "updated" int))
    (maybe (field "steamid" string))

type alias GameItems = Dict String GameItem

gameItems = dict gameItem

type alias Game = {
  id: String
  ,gameType: String
  ,created: Int
  ,winner: String
  ,config: GameConfig
  ,items: GameItems
  ,players: Users
}

playerTableToList: (Dict String User) -> Users
playerTableToList players =
  Dict.values players

game = map7 Game
    (field "id" string)
    (field "type" string)
    (field "created" int)
    (field "winner" string)
    (field "config" gameConfig)
    (field "items" gameItems)
    (field "players" (dict user) |> map playerTableToList)

type AnyResult = 
  GameResponse Game 
  | GamesResponse (List Game)
  | GameItemResponse GameItem 
  | GameItemsResponse (List GameItem)
  | TradesResponse (List Trade)
  | GameOrderResponse GameOrder
  | GameOrdersResponse (List GameOrder)
  | UserResponse User 
  | TradeResponse Trade
  | BotResponse Bot
  | GameRakeResponse GameRake
  | BackpackResponse Backpack
  | JsString String 


anyResult = oneOf [
    map GameResponse game,
    map GamesResponse (list game),
    map GameItemResponse gameItem,
    map GameItemsResponse (list gameItem),
    map TradesResponse (list trade),
    map GameOrderResponse gameOrder,
    map GameOrdersResponse (list gameOrder),
    map UserResponse user,
    map TradeResponse trade,
    map BotResponse bot,
    map GameRakeResponse gameRake,
    map BackpackResponse backpack,
    map JsString string
  ] 

type alias SearchResult = {
  resultType:String,
  search:String,
  result:AnyResult
}

searchResult = map3 SearchResult
    (field "resultType" string)
    (field "search" string)
    (field "result" anyResult)


-- defaultSearchResult = SearchResult "" "" (JsString "")

-- defaultItem = GameItem  "" "" "" (Just 0) "" "" 0

-- defaultGame = Game 
--     ""
--     ""
--     0
--     ""
--     {name=""}
--     Dict.empty

-- defaultUser = User "" "" "" defaultSteamInfo

defaultSteamInfo = SteamInfo "" "" "" defaultSteamAvatar 

defaultSteamAvatar = SteamAvatar ""
