{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Aeson as AE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.FixedByteString as FBS
import Data.Word (Word64)

import Concordium.Common.Amount
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types
import Concordium.Types.Execution
import Concordium.Types.Queries
import Concordium.Types.Tokens

account :: Word -> AccountAddress
account n = AccountAddress $ FBS.pack $ replicate 32 (fromIntegral n)

lock :: Word64 -> LockId
lock n =
    LockId
        { liAccountIndex = n,
          liSequenceNumber = n + 1,
          liCreationOrder = n + 2
        }

summary :: [SupplementedEvent] -> SupplementedTransactionSummary
summary events =
    SupplementedTransactionSummary
        { stsSender = Just (account 1),
          stsHash = TransactionHashV0 $ Hash.hash (BS.pack [1, 2, 3]),
          stsCost = Amount 0,
          stsEnergyCost = Energy 0,
          stsType = TSTAccountTransaction (Just TTMetaUpdate),
          stsResult = TxSuccess events,
          stsIndex = TransactionIndex 0,
          stsSponsorDetails = Nothing
        }

assertJsonRoundtrip :: String -> SupplementedTransactionSummary -> IO ()
assertJsonRoundtrip name value =
    case AE.fromJSON (AE.toJSON value) of
        AE.Success decoded
            | decoded == value -> pure ()
            | otherwise -> fail $ name <> ": decoded value did not match original"
        AE.Error err -> fail $ name <> ": failed to decode summary JSON: " <> err

assertDecodeFixture :: FilePath -> IO ()
assertDecodeFixture path = do
    bytes <- LBS.readFile path
    case AE.eitherDecode bytes :: Either String SupplementedTransactionSummary of
        Right _ -> pure ()
        Left err -> fail $ path <> ": failed to decode summary JSON: " <> err

main :: IO ()
main = do
    assertJsonRoundtrip
        "meta-update lock created summary"
        ( summary
            [ LockCreated
                { elcLockId = lock 10,
                  elcLockConfig = rawCborFromBytes "\161\100test\001"
                }
            ]
        )
    assertJsonRoundtrip
        "meta-update lock destroyed summary"
        (summary [LockDestroyed{eldLockId = lock 20}])
    assertJsonRoundtrip
        "meta-update token transfer with lock metadata summary"
        ( summary
            [ TokenTransfer
                { ettTokenId = TokenId "CCD",
                  ettFrom = HolderAccount (account 2),
                  ettTo = HolderAccount (account 3),
                  ettAmount = TokenAmount (TokenRawAmount 10) 0,
                  ettMemo = Nothing,
                  ettFromLock = Just (lock 30),
                  ettToLock = Just (lock 40)
                }
            ]
        )
    assertDecodeFixture "test/fixtures/meta-update-lock-created.json"
    assertDecodeFixture "test/fixtures/meta-update-lock-destroyed.json"
    assertDecodeFixture "test/fixtures/meta-update-token-transfer-locks.json"
