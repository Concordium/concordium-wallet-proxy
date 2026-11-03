SELECT e.id, s.block, s.timestamp, s.height, s.summary
FROM ati AS e INNER JOIN summaries AS s ON e.summary = s.id
WHERE e.account = $1
  AND ($2::bigint IS NULL OR e.id > $2)
  AND ($4::bigint IS NULL OR s.timestamp >= $4)
  AND ($5::bigint IS NULL OR s.timestamp <= $5)
  AND ($6::bigint IS NULL OR s.height >= $6)
  AND ($7::bigint IS NULL OR s.height <= $7)
  AND (NOT $8::boolean OR s.summary ? 'Left' OR s.summary #>> '{Right,tag}' <> 'BlockReward')
  AND (NOT $9::boolean OR s.summary ? 'Left' OR s.summary #>> '{Right,tag}' <> 'FinalizationRewards')
  AND (NOT $10::boolean OR s.summary ? 'Left' OR s.summary #>> '{Right,tag}' <> 'BakingRewards')
  AND (NOT $11::boolean OR s.summary #>> '{Left,type,contents}' IN (
      'encryptedAmountTransfer', 'transferToEncrypted', 'transferToPublic', 'encryptedAmountTransferWithMemo'))
  AND ($12::integer = 0
      OR ($12 = 1 AND (s.summary ? 'Left' OR s.summary #>> '{Right,tag}' <> 'FinalizationRewards'))
      OR ($12 = 2 AND (s.summary ? 'Left' OR ($13::boolean AND s.summary #>> '{Right,tag}' IN (
          'ValidatorSuspended', 'ValidatorPrimedForSuspension')))))
  AND ($13::boolean OR s.summary #>> '{Right,tag}' IS NULL
      OR s.summary #>> '{Right,tag}' NOT IN ('ValidatorSuspended', 'ValidatorPrimedForSuspension'))
  AND (s.summary #>> '{Left,type,contents}' IS NULL
      OR s.summary #>> '{Left,type,contents}' NOT IN ('updateCreatePLT', 'tokenUpdate', 'metaUpdate'))
ORDER BY e.id ASC
LIMIT $3
