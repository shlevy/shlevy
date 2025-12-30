{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PackageImports #-}

module Main where

import CapIO
import Control.Exception
import Data.Constraint
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Product
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Type.Equality
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec hiding (Spec)
import Test.Hspec.Hedgehog
import "e11y" Observe.Event

data TestSelector :: Spec where
  Childless :: TestSelector Integer NoChildren
  Parent :: TestSelector Integer (Children TestSelector)

deriving stock instance Show (TestSelector field cs)

deriving stock instance Eq (TestSelector field cs)

instance FieldsAndSelectorsMeet Show TestSelector where
  cSpec Childless = (Dict, ElseChildren ())
  cSpec Parent = (Dict, ThenChildren (Compose Dict))

instance FieldsAndSelectorsMeet Eq TestSelector where
  cSpec Childless = (Dict, ElseChildren ())
  cSpec Parent = (Dict, ThenChildren (Compose Dict))

instance TestSpecEquality TestSelector where
  testSpecEquality Childless Childless = Just (Refl, Refl, ElseChildren ())
  testSpecEquality Parent Parent = Just (Refl, Refl, ThenChildren (Compose Dict))
  testSpecEquality _ _ = Nothing

data TestException = MkTestException deriving (Show)

instance Exception TestException

genReplayEvent :: (MonadGen m) => m (ReplayEvent TestSelector)
genReplayEvent = do
  fieldsCount <- Gen.int (Range.linear 0 10)
  let
    fields = take fieldsCount [1 ..]
    genInstant =
      pure $
        MkReplayEvent
          { eventSelector = Childless
          , fields
          , childrenAndException = ElseChildren Instant
          }
    genException = Gen.element [Nothing, Just (SomeException MkTestException)]
    genChildless = do
      ex <- genException
      pure $
        MkReplayEvent
          { eventSelector = Childless
          , fields
          , childrenAndException = ElseChildren (Extended ex)
          }
    genParent' children = do
      ex <- genException
      pure $
        MkReplayEvent
          { eventSelector = Parent
          , fields
          , childrenAndException = ThenChildren (Pair (Const ex) (Compose children))
          }
    genChildlessParent = genParent' []
    genParent = Gen.list (Range.linear 1 10) genReplayEvent >>= genParent'
  Gen.recursive
    Gen.choice
    [ genInstant
    , genChildless
    , genChildlessParent
    ]
    [genParent]

data Caps clock = MkCaps
  { clk :: clock
  , vars :: MutVarArena (ClockState clock)
  }

start :: UTCTime
start = posixSecondsToUTCTime 0

runPure :: (forall s clock. (CanCleanup (CleanupPure s), s ~ ClockState clock, Clock clock) => Caps clock -> CapST s a) -> a
runPure go = runCapST $ \pc -> do
  let vars = mutVarArena (primPure pc)
  clk <- monotonicClock vars start
  canCleanupPure pc $
    go
      MkCaps
        { clk
        , vars
        }

main :: IO ()
main = hspec . parallel $ do
  describe "replayEvents" $ do
    it "roundtrips" $ hedgehog $ do
      evs <- forAll $ Gen.list (Range.linear 0 10) genReplayEvent
      let evs' = runPure $ \caps -> snd <$> withBackend' (allocReplayBackend caps.vars) (replayEvents evs)
      evs === evs'
  describe "TimestampEvents" $ do
    it "adds different timestamps to each event" $ do
      let
        expected =
          MkReplayEvent
            { eventSelector = TimestampParent Parent
            , fields = [Left (EventStart start), Right 1, Left (EventEnd (addUTCTime 4 start))]
            , childrenAndException = ThenChildren (Pair (Const Nothing) (Compose [child1, child2]))
            }
        child1 =
          MkReplayEvent
            { eventSelector = TimestampChildless Childless
            , fields = [Left (EventStart (addUTCTime 1 start)), Right 2, Left (EventEnd (addUTCTime 2 start))]
            , childrenAndException = ElseChildren (Extended Nothing)
            }
        child2 =
          MkReplayEvent
            { eventSelector = TimestampChildless Childless
            , fields = [Left (EventStart (addUTCTime 3 start)), Right 3]
            , childrenAndException = ElseChildren Instant
            }
        -- FIXME this type signature needed to avoid a panic
        (_, evs :: [ReplayEvent (TimestampEvents TestSelector)]) = runPure $ \caps -> withBackend' (allocReplayBackend caps.vars) $ do
          withBackend (allocTimestampEventsBackend caps.clk) $ do
            withEvent Parent $ do
              addField 1
              withEvent Childless $ do
                addField 2
              instantEvent Childless [3]
      evs `shouldBe` [expected]

-- TODO property tests for timestamping
