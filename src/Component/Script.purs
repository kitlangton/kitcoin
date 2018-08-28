module Component.Script where

import Control.Alt ((<|>))
import Data.Array (fromFoldable)
import Data.Either (Either, either)
import Data.List (List, many)
import Data.Maybe (Maybe(..))
import Data.String (codePointFromChar)
import Data.String as S
import Effect.Aff (Aff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (type (~>), Unit, Void, bind, const, discard, flip, pure, show, ($), ($>), (<$>), (<<<), (>>=))
import Text.Parsing.Parser (ParseError(..), Parser, runParser)
import Text.Parsing.Parser.Combinators (many1Till, manyTill, sepBy1, (<?>))
import Text.Parsing.Parser.String (anyChar, string)
import Text.Parsing.Parser.Token (space)

type State = {
  stack :: Array String,
  parseResult :: Either ParseError Program,
  script :: String
}

type Input = Unit

data Query a
  = NoOp a
  | HandleTextInput String a

ui :: H.Component HH.HTML Query Input Void Aff
ui =
  H.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState _ = { stack: [], script: "1 3 OP_ADD 4 OP_EQUAL", parseResult: parse "1 3 OP_ADD 4 OP_EQUAL" }

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div [] [
      HH.textarea [ HE.onValueInput $ HE.input HandleTextInput, HP.value st.script ],
      either renderError renderProgram st.parseResult
    ]

  renderError :: ParseError -> H.ComponentHTML Query
  renderError _ = HH.text "ERROR"

  renderProgram :: Program -> H.ComponentHTML Query
  renderProgram program = HH.div [class_ "program-container"] [
      HH.div [ class_ "program-header" ] [ HH.text "Program" ],
      HH.div [ class_ "program" ] $ fromFoldable $ renderScript' <$> program
    ]

  renderScript' :: Script -> H.ComponentHTML Query
  renderScript' script@(Op _) = HH.div [ class_ "token function" ] $ [ HH.text $ renderScript script ]
  renderScript' script = HH.div [ class_ "token" ] $ [ HH.text $ renderScript script ]

  class_ = HP.class_ <<< ClassName

  eval :: Query ~> H.ComponentDSL State Query Void Aff
  eval = case _ of
    HandleTextInput input next -> do
      let result = parse input
      H.modify_ _ { script = input, parseResult = result}
      pure next

    NoOp next -> do
      pure next

-- Script

type Stack =
  List ScriptVal

type Program = List Script

data Script =
  Op ScriptOp |
  Val ScriptVal

data ScriptOp =
  OpAdd |
  OpEqual

data ScriptVal =
  SBoolean Boolean |
  SInt Int

renderScript (Val (SInt int)) = show int
renderScript (Val (SBoolean bool)) = show bool
renderScript (Op OpAdd) = "OP_ADD"
renderScript (Op OpEqual) = "OP_EQUAL"

-- Language Parsers

parse :: String -> Either ParseError Program
parse = flip runParser parseProgram

parseProgram :: Parser String Program
parseProgram = sepBy1 parseScript space

parseScript :: Parser String Script
parseScript = parseVal <|> parseScriptOp

parseVal :: Parser String Script
parseVal = do
  val <- parseInt <|> parseBool <?> "Expected an Int or Boolean"
  pure $ Val val

parseBool :: Parser String ScriptVal
parseBool = SBoolean <$> (string "True" $> true <|> string "False" $> false)

parseInt :: Parser String ScriptVal
parseInt = SInt <$> digit

parseScriptOp :: Parser String Script
parseScriptOp = do
    op <- parseAdd <|> parseEqual <?> "Expected OP_ADD or OP_EQUAL"
    pure $ Op op

parseAdd :: Parser String ScriptOp
parseAdd = string "OP_ADD" $> OpAdd

parseEqual :: Parser String ScriptOp
parseEqual = string "OP_EQUAL" $> OpEqual

-- Basic Parsers

digit :: Parser String Int
digit = (string "0" >>= \_ -> pure 0)
        <|> (string "1" >>= \_ -> pure 1)
        <|> (string "2" >>= \_ -> pure 2)
        <|> (string "3" >>= \_ -> pure 3)
        <|> (string "4" >>= \_ -> pure 4)
        <|> (string "5" >>= \_ -> pure 5)
        <|> (string "6" >>= \_ -> pure 6)
        <|> (string "7" >>= \_ -> pure 7)
        <|> (string "8" >>= \_ -> pure 8)
        <|> (string "9" >>= \_ -> pure 9)
