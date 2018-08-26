
module Game.CommonHelpers where
    import Game.Common
    import qualified Data.Text as T
    import qualified Data.Map as Map

    playersToPlayerState :: [ps] -> Map.Map PlayerId ps
    playersToPlayerState ps = Map.fromList pairs
        where
            pairs = zipWith pairP [1..] ps
            toId = T.pack . show
            pairP :: Integer -> ps -> (PlayerId, ps)
            pairP i ps = (toId i, ps)