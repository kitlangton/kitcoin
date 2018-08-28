module Data.Blockchain where

import Crypto.Simple
import Prelude

import Data.Array (all, head, length, (!!), (:))
import Data.Foldable (foldl, sum)
import Data.Maybe (Maybe(..))
import Data.String as S

type Address = String
type Signature = String

type Blockchain = Array Block

data Coinbase = Coinbase {
  to :: Address
}

data Transactions = Transactions {
  coinbase :: Coinbase,
  transactions :: Array Transaction
}

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
  transactions :: Transactions
}

instance showBlock :: Show Block where
  show (Block {nonce, hash}) = "Nonce: " <> show nonce <> " - Hash: " <> hash

instance hashableBlock :: Hashable Block where
  hash hashType (Block b@{transactions: Transactions { transactions }}) =
    let
      transactionsString = foldl (<>) "" $ map (toString <<< hash hashType) transactions
    in
      hash hashType (show b.nonce <> b.prevHash <> transactionsString)

isValidBlockchain :: Blockchain -> Boolean
isValidBlockchain blockchain =
  all isValidBlock blockchain

isValidBlock :: Block -> Boolean
isValidBlock (Block {hash}) =
  "000" == S.take 3 hash

hashBlock :: Block -> String
hashBlock = toString <<< hash SHA256 <<< toString <<< hash SHA256

mineOnce :: Block -> Block
mineOnce (Block b) =
  let
    Block b' = Block $ b { nonce = b.nonce + 1}
    hash = hashBlock (Block b')
  in
    Block $ b' { hash = hash }

mkCandidateBlock :: Int -> String -> Int -> Blockchain -> Block
mkCandidateBlock id address seed blockchain =
    let
      transactions = Transactions {
        coinbase: Coinbase { to: show id },
        transactions: []
        }
      candidate =
        case head blockchain of
          Just (Block { hash} ) ->
            Block {
              prevHash: hash,
              hash: "",
              nonce: seed,
              transactions: transactions
            }
          Nothing ->
            Block {
              prevHash: "<<Genesis Block>>",
              hash: "",
              nonce: seed,
              transactions: transactions
            }
    in
      candidate
