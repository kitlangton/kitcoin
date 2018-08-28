module Component.Node where

import Data.Blockchain
import Prelude

import CSS (Color, color, hsl)
import Crypto.Simple (KeyPair, toString)
import Data.Array (head, length, (:))
import Data.Enum (fromEnum)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.String (toCodePointArray)
import Data.Time (Time(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Now (nowTime)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Message = NewBlockchain Blockchain | NodeClicked

type State = {
  keypair :: KeyPair,
  candidateBlock :: Block,
  blockchain :: Blockchain,
  seed :: Int,
  id :: Int,
  peers :: Array NodeId,
  isMining :: Boolean,
  receivedBlock :: Boolean,
  showMenu :: Boolean,
  isGreedy :: Boolean,
  isHighlighted :: Boolean,
  minedBlock :: Boolean
}

getAddress :: State -> String
getAddress { keypair: { public } } = toString public

addBlock :: State -> Effect State
addBlock st = do
  timestamp <- nowTime
  let
    st' = st { blockchain = st.candidateBlock : st.blockchain }
    newCandidate = mkCandidateBlock timestamp st.id (getAddress st') st'.seed st'.blockchain
  pure (st' { candidateBlock = newCandidate })

type Input = { id :: Int, peers :: Array NodeId, seed :: Int, keypair :: KeyPair, isHighlighted :: Boolean, timestamp :: Time }

data Query a
  = ToggleMining a
  | MineBlock a
  | GetPeers (Array NodeId -> a)
  | GetHash (String -> a)
  | ReceiveBlockchain Blockchain a
  | ShowMenu Boolean a
  | MakeGreedy Boolean a
  | HandleInput Input a
  | HandleClick a

ui :: H.Component HH.HTML Query Input Message Aff
ui =
  H.component
    { initialState
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  initialState { id, seed, peers, keypair, timestamp} = {
    keypair,
    id,
    candidateBlock: Block {
      nonce: seed,
      prevHash: "",
      hash: "",
      blockTransactions: BlockTransactions {
        coinbase: Coinbase { coinbaseAddress: id },
        transactions: []
      },
      timestamp
    },
    blockchain: [],
    seed,
    peers,
    isMining: false,
    receivedBlock: false,
    minedBlock: false,
    isGreedy: false,
    isHighlighted: false,
    showMenu: false
  }

  colorFromHash :: String -> Color
  colorFromHash string =
    let
      h = toNumber $ flip mod 360 $ sum $ (fromEnum) <$> toCodePointArray string
    in
      hsl h 1.0 0.5

  render :: State -> H.ComponentHTML Query
  render st =
    let
      chainLength = length st.blockchain
    in
    HH.div [ class_ $ "node-container "
        <> if st.showMenu then "floating " else " "
        <> if st.isHighlighted then "highlighted " else " "] [
      HH.div [ class_ $ "node "
        <> if st.receivedBlock then "received-block " else " "
        <> if st.minedBlock then "mined-block " else " "
        <> if st.isMining then "mining " else " ",
        HE.onClick $ HE.input_ $ HandleClick
        ] [
      ],
      (if chainLength > 0 then
        HH.div [
          class_ "block-height",
          CSS.style do color (colorFromHash prevHash)
        ] [
          HH.strong_ [HH.text $ show $ chainLength]
        ]
        else HH.text ""
      ),
      if st.showMenu
        then
          HH.div_ [
            HH.div [
              class_ "menu floating"
            ] [
              HH.div [
                class_ "menu-item"
              ] [
                HH.text $ (\(Block {nonce}) -> show nonce) st.candidateBlock
              ],
              HH.div [
                class_ "menu-item"
              ] [
                HH.text $ (\(Block {hash}) -> hash) st.candidateBlock
              ]
              -- HH.div [
              --   class_ "menu-item",
              --   HE.onClick $ HE.input_ MineBlock
              -- ] [
              --   HH.text "Start Mining"
              -- ],
              -- HH.div [
              --   class_ "menu-item",
              --   HE.onClick $ HE.input_ $ MakeGreedy true
              -- ] [
              --   HH.text "Make Greedy"
              -- ]
            ],
            HH.div [
              class_ "overlay",
              HE.onClick $ HE.input_ $ ShowMenu false
            ] [
            ]
          ]
        else
          HH.text "",
      if st.isHighlighted then
        HH.div [
          class_ "overlay",
          HE.onClick $ HE.input_ $ ShowMenu false
        ] [
        ]
        else HH.text ""

    ]
    where
      prevHash = (\(Block {prevHash}) -> prevHash) st.candidateBlock

  class_ = HP.class_ <<< ClassName

  eval :: Query ~> H.ComponentDSL State Query Message Aff
  eval = case _ of
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
      isGreedy <- H.gets _.isGreedy
      if isGreedy then pure next
        else do
          blockchain <- H.gets _.blockchain
          if length newBlockchain > length blockchain && isValidBlockchain newBlockchain
            then do
              _ <- receivedBlock
              st <- H.get
              timestamp <- H.liftEffect nowTime
              H.modify_ ( _ {
                blockchain = newBlockchain,
                candidateBlock = mkCandidateBlock timestamp st.id (getAddress st) st.seed newBlockchain
                })
              _ <- H.fork $ do
                H.liftAff $ delay $ Milliseconds 200.0
                H.raise $ NewBlockchain newBlockchain
              pure next
            else
              pure next

    ShowMenu showMenu next -> do
      H.modify_ _ { showMenu = showMenu }
      pure next

    MakeGreedy isGreedy next -> do
      H.modify_ _ { isGreedy = isGreedy }
      pure next

    HandleInput { isHighlighted } next -> do
      H.modify_ _ { isHighlighted = isHighlighted }
      pure next

    ToggleMining next -> do
      isMining <- H.gets _.isMining
      st <- H.get
      timestamp <- H.liftEffect nowTime
      H.modify_ _ { isMining = not isMining }
      if not isMining
        then do
          H.modify_ ( _ {
            candidateBlock = mkCandidateBlock timestamp st.id (getAddress st) st.seed st.blockchain
            })
          eval $ MineBlock next
        else
          pure next

    HandleClick next -> do
      H.raise NodeClicked
      pure next

    MineBlock next -> do
      shouldMine <- H.gets _.isMining
      if shouldMine then do
        block <- H.gets (mineOnce <<<_.candidateBlock)
        H.modify_ _ { candidateBlock = block}

        if isValidBlock block
          then do
            minedBlock block
            _ <- H.fork $ do
              H.liftAff $ delay $ Milliseconds 30.0
              eval $ MineBlock next
            pure next
          else do
            _ <- H.fork $ do
              H.liftAff $ delay $ Milliseconds 30.0
              eval $ MineBlock next
            pure next
        else
          pure next

receivedBlock = do
  H.modify_ _ { receivedBlock = true }
  H.fork $ do
    H.liftAff $ delay $ Milliseconds 300.0
    H.modify_ _ { receivedBlock = false }

minedBlock block = do
  st <- H.get
  st' <- H.liftEffect $ addBlock st
  H.put st'
  blockchain <- H.gets _.blockchain
  H.modify_ _ { minedBlock = true }
  _ <- H.fork $ do
    H.liftAff $ delay $ Milliseconds 1000.0
    H.modify_ _ { minedBlock = false }
  H.raise $ NewBlockchain blockchain
