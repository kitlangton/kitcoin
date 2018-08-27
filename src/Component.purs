module Component where

import Data.Map
import Prelude

import Component.Node (Block(..), Blockchain, Coinbase(..), Transactions(..), isValidBlockchain)
import Component.Node as Node
import Crypto.Simple (KeyPair, generateKeyPair)
import Data.Array (catMaybes, foldr, fromFoldable, length, range)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (rem)
import Data.List (nub)
import Data.Map as Data.Map
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = {
  nodes :: Array NodeData,
  blockchain :: Maybe Blockchain,
  highlight :: Maybe NodeId
  -- transaction :: Maybe NewTransaction
}

type NewTransaction = {
  from :: Int,
  to :: Maybe Int,
  amount :: Maybe Int
}

data Query a
  = Initialize a
  | HandleNodeMessage NodeId Node.Message a
  | MineBlocks a
  | HighlightNode (Maybe NodeId) a

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

type Ledger = Map Int Int

getLedger :: Blockchain -> Ledger
getLedger =
  (foldr (unionWith (+)) empty) <<< map getLedgerBlock

getLedgerBlock :: Block -> Ledger
getLedgerBlock (Block {transactions}) =
  getLedgerTransactions transactions

getLedgerTransactions :: Transactions -> Ledger
getLedgerTransactions (Transactions { coinbase: Coinbase { to } }) =
  singleton to 10

ui :: H.Component HH.HTML Query Unit Void Aff
ui =
  H.lifecycleParentComponent
    { initialState: const { nodes: [], blockchain: Nothing, highlight: Nothing }
    , render
    , eval
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  render :: State -> ParentHTML
  render st =
    HH.div_ [
      HH.div [ HE.onClick $ HE.input_ MineBlocks, class_ "mine-all"] [
        HH.text "Mine All Nodes"
      ],
      HH.div [ class_ "nodes" ] $
        map (renderNode st.highlight) st.nodes
      ,
      maybe (HH.text "") renderBlockchain st.blockchain
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
    HH.div [ class_ "ledger-entry", HE.onMouseOver $ HE.input_ $ HighlightNode $ Just address, HE.onMouseLeave $ HE.input_ $ HighlightNode Nothing] [
      HH.div [ class_ "ledger-entry-address"] [
        HH.text $ show address
      ],
      HH.div [ class_ "ledger-entry-value"] [
        HH.text $ show value
      ]
    ]

  renderNode :: Maybe NodeId -> NodeData -> ParentHTML
  renderNode highlightId {id, seed, keypair} =
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
        isHighlighted
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
      nodes' <- H.liftEffect $ traverse mkNode nodes
      H.modify_ _ { nodes = nodes'}
      pure next
      -- eval $ MineBlocks next

    MineBlocks next -> do
      _ <- H.queryAll (H.action Node.MineBlock)
      pure next

    HighlightNode maybeNode next -> do
      H.modify_ _ { highlight = maybeNode}
      pure next

    HandleNodeMessage id nodeMessage next -> do
      case nodeMessage of
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
