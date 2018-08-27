module Component where

import Prelude

import Component.Node as Node
import Data.List (nub)
import Data.Map as Data.Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Halogen (ClassName(..))
import Halogen (HalogenM(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Array NodeId

data Query a
  = Initialize a
  | HandleNodeMessage NodeId Node.Message a
  | MineBlocks a

type NodeId = Int

data ChildSlot = NodeSlot NodeId

instance showChildSlot :: Show ChildSlot where
  show (NodeSlot id) = show id

derive instance eqChildSlot :: Eq ChildSlot
derive instance ordChildSlot :: Ord ChildSlot

type ChildQuery = Node.Query

type ParentHTML = H.ParentHTML Query ChildQuery ChildSlot Aff
type ParentDSL = H.ParentDSL State Query ChildQuery ChildSlot Void Aff

ui :: H.Component HH.HTML Query Unit Void Aff
ui =
  H.parentComponent
    { initialState: const [0,1,2,3,4,5]
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> ParentHTML
  render st =
    HH.div_ [
      HH.div [ HE.onClick $ HE.input_ MineBlocks ] [
        HH.text "MINE BLOCKS"
      ],
      HH.div_ $
        map (renderNode st) st
    ]

  renderNode :: Array Int -> Int -> ParentHTML
  renderNode ids id =
      HH.slot
      (NodeSlot id)
      (Node.ui)
      [id + 1, id - 1]
      (HE.input $ HandleNodeMessage id)

  class_ = HP.class_ <<< ClassName

  eval :: Query ~> ParentDSL
  eval = case _ of
    Initialize next -> do
      pure next

    MineBlocks next -> do
      _ <- H.queryAll (H.action Node.MineBlock)
      pure next

    HandleNodeMessage id nodeMessage next -> do
      case nodeMessage of
        (Node.NewBlockchain blockchain) -> do
          H.liftEffect $ log $ "Node #" <> show id <> " received a blockchain"

          -- View Current Blockchains
          hashMap <- H.queryAll (H.request Node.GetHash)
          let values = nub $ Data.Map.values hashMap
          H.liftEffect $ log $ show values

          maybePeers <- H.query (NodeSlot id) (H.request Node.GetPeers)
          case maybePeers of
            Just peers -> do
              _ <- queryPeers peers (Node.ReceiveBlockchain blockchain)
              pure next
            Nothing ->
              pure next

queryPeer action id =
          H.query (NodeSlot id) (H.action $ action)

queryPeers ids action =
  traverse (queryPeer action) ids
