module Component.Node where

import Crypto.Simple
import Prelude

import CSS (Color, color, rgb, hsl)
import Color.Scheme.Clrs (red)
import Data.Array (all, head, length, (!!), (:))
import Data.Enum (fromEnum)
import Data.Foldable (foldl, sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Monoid ((<>))
import Data.String (take, toCodePointArray)
import Effect.Aff (Aff, Milliseconds(..), delay, forkAff)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Message = NewBlockchain Blockchain

type Blockchain = Array Block

type State = {
  keypair :: Maybe KeyPair,
  candidateBlock :: Block,
  blockchain :: Blockchain,
  startingNonce :: Int,
  peers :: Array NodeId,
  isMining :: Boolean,
  receivedBlock :: Boolean,
  minedBlock :: Boolean
}

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
    peers: peers,
    isMining: false,
    receivedBlock: false,
    minedBlock: false
  }

  colorFromHash :: String -> Color
  colorFromHash string =
    let
      h = toNumber $ flip mod 360 $ sum $ (fromEnum) <$> toCodePointArray string
    in
      hsl h 1.0 0.5

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div [ class_ "node-container"] [
      HH.div [ class_ $ "node "
        <> if st.receivedBlock then "received-block " else " "
        <> if st.minedBlock then "mined-block " else " "
        <> if st.isMining then "mining " else " ",
        HE.onClick $ HE.input_ MineBlock
        ] [
        -- HH.div_ [
        --   HH.div_ [ HH.text $ show $ length st.blockchain],
        --   HH.div [HE.onClick $ HE.input_ MineBlock] [ HH.text $ "Block: " ],
        --   HH.div_ [ HH.text $ show st.candidateBlock]
        -- ]
      ],
      HH.div [
        class_ "block-height",
        CSS.style do color (colorFromHash prevHash)
      ] [
        HH.strong_ [HH.text $ show $ length st.blockchain]
      ]
    ]
    where
      prevHash = (\(Block {prevHash}) -> prevHash) st.candidateBlock

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
          _ <- receivedBlock
          H.modify_ ( mkCandidateBlock <<< _ { blockchain = newBlockchain })
          _ <- H.fork $ do
            H.liftAff $ delay $ Milliseconds 500.0
            H.raise $ NewBlockchain newBlockchain
          pure next
        else
          pure next

    MineBlock next -> do
      H.modify_ _ { isMining = true }
      block <- H.gets (mineOnce <<<_.candidateBlock)
      H.modify_ _ { candidateBlock = block}

      if isValidBlock block
        then do
          minedBlock block
          pure next
        else do
          _ <- H.fork $ do
            H.liftAff $ delay $ Milliseconds 20.0
            eval $ MineBlock next
          pure next

receivedBlock = do
  H.modify_ _ { receivedBlock = true }
  H.fork $ do
    H.liftAff $ delay $ Milliseconds 300.0
    H.modify_ _ { receivedBlock = false }

minedBlock block = do
  H.modify_ addBlock
  blockchain <- H.gets _.blockchain
  H.modify_ _ { isMining = false, minedBlock = true }
  _ <- H.fork $ do
    H.liftAff $ delay $ Milliseconds 1000.0
    H.modify_ _ { minedBlock = false }
  H.raise $ NewBlockchain blockchain
