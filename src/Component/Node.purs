module Component.Node where

import Crypto.Simple
import Prelude

import Data.Array (all, head, length, (:))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid ((<>))
import Data.String (take)
import Effect.Aff (Aff, Milliseconds(..), delay, forkAff)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Message = NewBlockchain Blockchain

type Address = String
type Signature = String
type NodeId = Int

data Transaction = Transaction {
  from :: Address,
  to :: Address,
  signature :: Signature
}

instance hashableTransction :: Hashable Transaction where
  hash hashType (Transaction t) =
      hash hashType (t.from <> t.to <> t.signature)

data Block = Block {
  nonce :: Int,
  prevHash :: String,
  hash :: String,
  transactions :: Array Transaction
}

instance showBlock :: Show Block where
  show (Block {nonce, hash}) = "Nonce: " <> show nonce <> " - Hash: " <> hash

instance hashableBlock :: Hashable Block where
  hash hashType (Block b) =
    let
      transactionsString = foldl (<>) "" $ map (toString <<< hash hashType) b.transactions
    in
      hash hashType (show b.nonce <> b.prevHash <> transactionsString)


isValidBlockchain :: Blockchain -> Boolean
isValidBlockchain blockchain =
  all isValidBlock blockchain

isValidBlock :: Block -> Boolean
isValidBlock (Block {hash}) =
  "00" == take 2 hash

hashBlock :: Block -> String
hashBlock = toString <<< hash SHA256 <<< toString <<< hash SHA256

mineOnce :: Block -> Block
mineOnce (Block b) =
  let
    Block b' = Block $ b { nonce = b.nonce + 1}
    hash = hashBlock (Block b')
  in
    Block $ b' { hash = hash }

type Blockchain = Array Block

type State = {
  keypair :: Maybe KeyPair,
  candidateBlock :: Block,
  blockchain :: Blockchain,
  startingNonce :: Int,
  peers :: Array NodeId
}

mkCandidateBlock :: State -> State
mkCandidateBlock st@{ startingNonce, blockchain} =
  let
    candidate =
      case head blockchain of
        Just (Block { hash} ) ->
          Block {
            prevHash: hash,
            hash: "",
            nonce: startingNonce,
            transactions: []
          }
        Nothing ->
          Block {
            prevHash: "<<Genesis Block>>",
            hash: "",
            nonce: startingNonce,
            transactions: []
          }
  in
    st { candidateBlock = candidate}

addBlock :: State -> State
addBlock st =
  mkCandidateBlock $ st { blockchain = st.candidateBlock : st.blockchain }

data Query a
  = Initialize a
  | MineBlock a
  | GetPeers (Array NodeId -> a)
  | GetHash (String -> a)
  | ReceiveBlockchain Blockchain a

ui :: H.Component HH.HTML Query (Array NodeId) Message Aff
ui =
  H.lifecycleComponent
    { initialState
    , render
    , eval
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  initialState peers = {
    keypair: Nothing,
    candidateBlock: Block {
      nonce: 0,
      prevHash: "",
      hash: "",
      transactions: []
    },
    blockchain: [],
    startingNonce: 0,
    peers: peers
  }

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div [ class_ "node" ] [
      HH.div_ [
        HH.strong_ [HH.text "Node"]
      ],
      HH.div_ [
        HH.div_ [ HH.text $ show $ length st.blockchain],
        HH.div [HE.onClick $ HE.input_ MineBlock] [ HH.text $ "Block: " ],
        HH.div_ [ HH.text $ show st.candidateBlock]
      ]
    ]

  class_ = HP.class_ <<< ClassName

  eval :: Query ~> H.ComponentDSL State Query Message Aff
  eval = case _ of
    Initialize next -> do
      nonce <- H.liftEffect $ randomInt 0 999999999
      keypair <- H.liftEffect generateKeyPair
      H.modify_ (mkCandidateBlock <<< _ { keypair = Just keypair, startingNonce = nonce})
      pure next

    GetPeers reply -> do
      peers <- H.gets _.peers
      pure (reply peers)

    GetHash reply -> do
      blockchain <- H.gets _.blockchain
      case head blockchain of
        Just (Block {hash}) ->
          pure (reply hash)
        Nothing ->
          pure (reply "")

    ReceiveBlockchain newBlockchain next -> do
      blockchain <- H.gets _.blockchain
      if length newBlockchain > length blockchain && isValidBlockchain newBlockchain
        then do
          H.modify_ ( mkCandidateBlock <<< _ { blockchain = newBlockchain })
          _ <- H.fork $ do
            H.liftAff $ delay $ Milliseconds 500.0
            H.raise $ NewBlockchain newBlockchain
            pure unit
          pure next
        else
          pure next

    MineBlock next -> do
      block <- H.gets (mineOnce <<<_.candidateBlock)
      H.modify_ _ { candidateBlock = block}

      if isValidBlock block
        then do
          H.modify_ addBlock
          blockchain <- H.gets _.blockchain
          H.raise $ NewBlockchain blockchain
          pure next
        else do
          _ <- H.fork $ do
            H.liftAff $ delay $ Milliseconds 20.0
            _ <- eval $ MineBlock next
            pure unit
          pure next
