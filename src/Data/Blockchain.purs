module Data.Blockchain where

import Prelude

import Crypto.Simple (class Hashable, Hash(..), hash, toString)
import Data.Array (all, foldr, head, length, tail, zipWith)
import Data.Foldable (foldl, sum)
import Data.Int (toNumber)
import Data.Lens (Lens, Lens', lens, over, set, toListOf, traversed, view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List, filter)
import Data.Map (Map, empty, insert, member, singleton, unionWith)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set as Set
import Data.String (take)
import Data.Symbol (SProxy(..))
import Data.Time (Time(..), diff)
import Data.Time.Duration (Minutes(..), Seconds(..))

type Blockchain = Array Block
type Address = String
type Signature = String
type NodeId = Int

newtype Coinbase = Coinbase {
  coinbaseAddress :: Int
}

_coinbaseAddress :: Lens' Coinbase Int
_coinbaseAddress = _Newtype <<< prop (SProxy :: SProxy "coinbaseAddress")

derive instance newtypeCoinbase :: Newtype Coinbase _

newtype BlockTransactions = BlockTransactions {
  coinbase :: Coinbase,
  transactions :: Array Transaction
}

derive instance newtypeTransactions :: Newtype BlockTransactions _

_coinbase :: Lens' BlockTransactions Coinbase
_coinbase = _Newtype <<< prop (SProxy :: SProxy "coinbase")

_transactions :: Lens' BlockTransactions (Array Transaction)
_transactions = _Newtype <<< prop (SProxy :: SProxy "transactions")

type TxIn = Int

newtype TxOut = TXOut {
  outAddress :: Int,
  outValue :: Int
}

derive instance newtypeTxOut :: Newtype TxOut _

_outAddress :: Lens' TxOut Int
_outAddress = _Newtype <<< prop (SProxy :: SProxy "outAddress")

_outValue :: Lens' TxOut Int
_outValue = _Newtype <<< prop (SProxy :: SProxy "outValue")

newtype Transaction = Transaction {
  ins :: Array TxIn,
  outs :: Array TxOut,
  signature :: Signature
}

derive instance newtypeTransaction :: Newtype Transaction _

_ins :: Lens' Transaction (Array TxIn)
_ins = _Newtype <<< prop (SProxy :: SProxy "ins")

_outs :: Lens' Transaction (Array TxOut)
_outs = _Newtype <<< prop (SProxy :: SProxy "outs")

_signature :: Lens' Transaction Address
_signature = _Newtype <<< prop (SProxy :: SProxy "signature")

instance hashableTransction :: Hashable Transaction where
  hash hashType (Transaction t) =
      hash hashType (t.signature)

newtype Block = Block {
  nonce :: Int,
  prevHash :: String,
  hash :: String,
  timestamp :: Time,
  blockTransactions :: BlockTransactions
}

derive instance newtypeBlock :: Newtype Block _

_timestamp :: Lens' Block Time
_timestamp = _Newtype <<< prop (SProxy :: SProxy "timestamp")

_hash :: Lens' Block String
_hash = _Newtype <<< prop (SProxy :: SProxy "hash")

_prevHash :: Lens' Block String
_prevHash = _Newtype <<< prop (SProxy :: SProxy "prevHash")

_blockTransactions :: Lens' Block BlockTransactions
_blockTransactions = _Newtype <<< prop (SProxy :: SProxy "blockTransactions")

_nonce :: Lens' Block Int
_nonce = _Newtype <<< prop (SProxy :: SProxy "nonce")

instance showBlock :: Show Block where
  show (Block {nonce, hash}) = "Nonce: " <> show nonce <> " - Hash: " <> hash

instance hashableBlock :: Hashable Block where
  hash hashType block =
    let
      transactions = block ^. _blockTransactions <<< _transactions
      transactionsString = foldl (<>) "" $ map (toString <<< hash hashType) transactions
    in
      hash hashType (show (block ^. _nonce) <> block ^. _prevHash <> transactionsString <> show (block ^. _timestamp))

type Ledger = Map Int Int

getLedger :: Blockchain -> Ledger
getLedger =
  foldr (unionWith (+)) empty <<< map getLedgerBlock

getLedgerBlock :: Block -> Ledger
getLedgerBlock block =
  let
    coinbaseTx = singleton (block ^. _blockTransactions <<< _coinbase <<< _coinbaseAddress) 10
    txIns = Set.fromFoldable $ toListOf (_blockTransactions <<< _transactions <<< traversed <<< _ins <<< traversed) block
    txOuts' = toListOf (_blockTransactions <<< _transactions <<< traversed <<< _outs <<< traversed) block
    txOuts = filter (\txout -> Set.member (txout ^. _outAddress) txIns) txOuts'
    f txOut map = unionWith (+) (singleton (txOut ^. _outAddress) (txOut ^. _outValue)) map
  in
    foldr f coinbaseTx txOuts

isValidBlockchain :: Blockchain -> Boolean
isValidBlockchain blockchain =
  all isValidBlock blockchain

isValidBlock :: Block -> Boolean
isValidBlock block =
  "000" == take 3 (block ^. _hash)

blockMineTimeDiffs :: Blockchain -> Array Number
blockMineTimeDiffs blockchain =
  case tail blockchain of
    Just t | length t > 0->
      let
        mins :: Array Seconds
        mins = zipWith (\b1 b2 -> diff (b1 ^. _timestamp) (b2 ^. _timestamp)) blockchain t
      in
        unwrap <$> mins
    _ ->
      []

averageMine :: Blockchain -> Number
averageMine blockchain =
  case tail blockchain of
    Just t | length t > 0->
      let
        mins :: Array Seconds
        mins = zipWith (\b1 b2 -> diff (b1 ^. _timestamp) (b2 ^. _timestamp)) blockchain t
      in
        (\secs -> unwrap secs / toNumber (length mins) ) $ foldr append (Seconds 0.0) mins
    _ ->
      0.0

hashBlock :: Block -> String
hashBlock = toString <<< hash SHA256 <<< toString <<< hash SHA256

mineOnce :: Block -> Block
mineOnce block =
  let
    block' = over _nonce ((+) 1) block
  in
    set _hash (hashBlock block') block'

mkCandidateBlock :: Time ->  Int -> String -> Int -> Blockchain -> Block
mkCandidateBlock now id address seed blockchain =
    let
      transactions = BlockTransactions {
        coinbase: Coinbase { coinbaseAddress: id },
        transactions: []
        }
      candidate =
        case head blockchain of
          Just (Block { hash} ) ->
            Block {
              prevHash: hash,
              hash: "",
              nonce: seed,
              timestamp: now,
              blockTransactions: transactions
            }
          Nothing ->
            Block {
              prevHash: "<<Genesis Block>>",
              hash: "",
              nonce: seed,
              timestamp: now,
              blockTransactions: transactions
            }
    in
      candidate
