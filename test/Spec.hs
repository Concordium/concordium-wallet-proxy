import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as LBS

import Concordium.Types.Queries (SupplementedTransactionSummary)
import Concordium.Types.Transactions (SpecialTransactionOutcome)

assertDecodeFixture :: FilePath -> IO ()
assertDecodeFixture path = do
    bytes <- LBS.readFile path
    case AE.eitherDecode bytes :: Either String (Either SupplementedTransactionSummary SpecialTransactionOutcome) of
        Right _ -> pure ()
        Left err -> fail $ path <> ": failed to decode summary JSON: " <> err

main :: IO ()
main = assertDecodeFixture "test/fixtures/token-update-events.json"
