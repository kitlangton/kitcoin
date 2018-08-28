module Component where

import Data.Blockchain
import Prelude

import Component.Node as Node
import Crypto.Simple (KeyPair, generateKeyPair)
import Data.Array (catMaybes, foldr, fromFoldable, length, range)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (rem)
import Data.Map (Map, empty, singleton, unionWith)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String.CodeUnits as S
import Data.Time (Time(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Now (nowTime)
import Effect.Random (randomInt)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = {
  nodes :: Array NodeData,
  blockchain :: Maybe Blockchain,
  highlight :: Maybe NodeId,
  transaction :: Maybe NewTransaction,
  startingTime :: Maybe Time
}

type NewTransaction = {
  from :: Int,
  amount :: Int,
  to :: Maybe Int
}

data Query a
  = Initialize a
  | HandleNodeMessage NodeId Node.Message a
  | MineBlocks a
  | HighlightNode (Maybe NodeId) a
  | CreateNewTransaction Int Int a
  | SelectOutputAddress Int a
  | CancelTransaction a

type NodeId = Int

data ChildSlot = NodeSlot NodeId

type NodeData = {
  id :: NodeId,
  seed :: Int,
  keypair :: KeyPair
}

instance showChildSlot :: Show ChildSlot where
  show (NodeSlot id) = show id

derive instance eqChildSlot :: Eq ChildSlot
derive instance ordChildSlot :: Ord ChildSlot

type ChildQuery = Node.Query

type ParentHTML = H.ParentHTML Query ChildQuery ChildSlot Aff
type ParentDSL = H.ParentDSL State Query ChildQuery ChildSlot Void Aff

-- Blockchain Functions


ui :: H.Component HH.HTML Query Unit Void Aff
ui =
  H.lifecycleParentComponent
    { initialState: const { nodes: [], blockchain: Nothing, highlight: Nothing, transaction: Nothing, startingTime: Nothing }
    , render
    , eval
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  render :: State -> ParentHTML
  render st =
    HH.div [ class_ "container"] [
      HH.div [ class_ "stats"]
        case st.blockchain of
          Just blockchain ->
            [
              HH.div [ class_ "stat" ] [
                HH.div [ class_ "stat-term"] [
                  HH.text "Average Mining Time"
                ],
                HH.div [ class_ "stat-definition"] [
                  HH.text $ S.take 5 (show $ averageMine blockchain) <> " Seconds"
                ]
              ],
              -- HH.div [ class_ "stat" ] [
              --   HH.div [ class_ "stat-term"] [
              --     HH.text "Average Mining Time"
              --   ],
              --   HH.div [ class_ "stat-definition"] [
              --     HH.text $ (show $ blockMineTimeDiffs blockchain)
              --   ]
              -- ],
              HH.div [ class_ "stat" ] [
                HH.div [ class_ "stat-term"] [
                  HH.text "Block Height"
                ],
                HH.div [ class_ "stat-definition"] [
                  HH.text $ show (length blockchain) <> " Blocks"
                ]
              ]
            ]
          Nothing ->
            [HH.text ""]
      ,
      HH.div [ class_ "main"] [
        HH.div [ HE.onClick $ HE.input_ MineBlocks, class_ "mine-all"] [
          HH.text "Mine All Nodes"
        ],
        HH.div [ class_ "nodes" ] $
          case st.startingTime of
            Just timestamp ->
              map (renderNode timestamp st.highlight) st.nodes
            Nothing -> []
        , maybe (HH.text "") renderTransactionForm st.transaction
        , maybe (HH.text "") renderBlockchain st.blockchain
      ]
    ]

  renderTransactionForm :: NewTransaction -> ParentHTML
  renderTransactionForm {from, amount, to} =
    HH.div [ class_ "transaction-container"] [
      HH.div [ class_ "ledger-header"] [
        HH.text "New Transaction",
          HH.div [ class_ "ledger-subheader", HE.onClick $ HE.input_ CancelTransaction] [
            HH.text "Cancel"
          ]
      ],
      HH.div [ class_ "transaction-form"] [
        HH.div [ class_ "transaction-from"] [
          HH.text $ show from
        ],
        HH.div [ class_ "transaction-amount"] [
          HH.text $ show amount
        ],
        HH.div [ class_ "transaction-to"] [
          HH.text $ maybe "Click on a Node above" show to
        ],
        if isJust to
          then
            HH.div [ class_ "transaction-submit"] [
              HH.text "Submit to random nodes"
            ]
          else HH.text ""
      ]
    ]

  renderBlockchain :: Blockchain -> ParentHTML
  renderBlockchain blockchain =
    let
      ledger = getLedger blockchain
    in
      HH.div [ class_ "" ] $ [
        HH.div [ class_ "ledger-header"] [
          HH.text "Ledger",
          HH.div [ class_ "ledger-subheader"] [
            HH.text "From Main Chain"
          ]
        ],
        HH.div [ class_"blockchain"] $ fromFoldable $ mapWithIndex renderLedgerEntry ledger
      ]

  renderLedgerEntry :: Int -> Int -> ParentHTML
  renderLedgerEntry address value =
    HH.div [ class_ "ledger-entry",
              HE.onMouseOver $ HE.input_ $ HighlightNode $ Just address,
              HE.onMouseLeave $ HE.input_ $ HighlightNode Nothing,
              HE.onClick $ HE.input_ $ CreateNewTransaction address value
              ] [
      HH.div [ class_ "ledger-entry-address"] [
        HH.text $ show address
      ],
      HH.div [ class_ "ledger-entry-value"] [
        HH.text $ show value
      ]
    ]

  renderNode :: Time -> Maybe NodeId -> NodeData -> ParentHTML
  renderNode timestamp highlightId {id, seed, keypair} =
      let
        isHighlighted = highlightId == Just id
        peers = catMaybes
          [ Just $ id + 8
          , Just $ id - 8
          , if id `rem` 8 == 1 then Nothing else Just $ id - 1
          , if id `rem` 8 == 0 then Nothing else Just $ id + 1]
      in
        HH.slot
        (NodeSlot id)
        (Node.ui)
        {
          id,
          peers,
          seed,
          keypair,
          isHighlighted,
          timestamp
        }
        (HE.input $ HandleNodeMessage id)

  class_ = HP.class_ <<< ClassName

  mkNode :: Int -> Effect NodeData
  mkNode id = do
    seed <- randomInt 0 999999999
    keypair <- generateKeyPair
    pure {id, seed, keypair}

  eval :: Query ~> ParentDSL
  eval = case _ of
    Initialize next -> do
      let nodes = range 1 64
      startingTime <- H.liftEffect nowTime
      nodes' <- H.liftEffect $ traverse mkNode nodes
      H.modify_ _ { nodes = nodes', startingTime = Just startingTime }
      pure next
      -- eval $ MineBlocks next

    MineBlocks next -> do
      _ <- H.queryAll (H.action Node.ToggleMining)
      pure next

    HighlightNode maybeNode next -> do
      H.modify_ _ { highlight = maybeNode}
      pure next

    CreateNewTransaction from amount next -> do
      let newTransaciton = { from, amount, to: Nothing }
      H.modify_ _ { transaction = Just newTransaciton }
      pure next

    SelectOutputAddress to next -> do
      transaction' <- H.gets _.transaction
      case transaction' of
        Just transaction -> do
          H.modify_ _ { transaction = Just transaction { to = Just to} }
          pure next
        Nothing ->
          pure next

    CancelTransaction next -> do
      H.modify_ _ { transaction = Nothing }
      pure next

    HandleNodeMessage id nodeMessage next -> do
      case nodeMessage of
        Node.NodeClicked -> do
          transaction <- H.gets _.transaction
          if isJust transaction then
              eval $ SelectOutputAddress id next
            else do
              _ <- H.query (NodeSlot id) (H.action $ Node.ShowMenu true)
              pure next

        (Node.NewBlockchain newBlockchain) -> do
          blockchain <- H.gets _.blockchain
          replaceChain blockchain newBlockchain

          maybePeers <- H.query (NodeSlot id) (H.request Node.GetPeers)
          case maybePeers of
            Just peers -> do
              _ <- queryPeers peers (Node.ReceiveBlockchain newBlockchain)
              pure next
            Nothing ->
              pure next

-- replaceChain :: Maybe Blockchain -> Blockchain -> _
replaceChain Nothing newBlockchain =
    H.modify_ _ { blockchain = Just newBlockchain }
replaceChain (Just blockchain) newBlockchain =
    if length newBlockchain > length blockchain && isValidBlockchain newBlockchain
      then do
        H.modify_ _ { blockchain = Just newBlockchain }
      else
        pure unit

queryPeer action id =
          H.query (NodeSlot id) (H.action $ action)

queryPeers ids action =
  traverse (queryPeer action) ids
