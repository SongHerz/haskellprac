import Control.Monad

data MovieReview = MovieReview {
      revTitle :: String
    , revUser :: String
    , revReview :: String
    } deriving (Show)

simpleReview :: [(String, Maybe String)] -> Maybe MovieReview
simpleReview alist =
    case lookup "title" alist of
        -- (_:_) is a convenient pattern for matching non-empty list
        Just (Just title@(_:_)) ->
            case lookup "user" alist of
                Just (Just user@(_:_)) ->
                    case lookup "review" alist of
                        Just (Just review@(_:_)) ->
                            Just (MovieReview title user review)
                        _ -> Nothing -- no review
                _ -> Nothing -- no user
        _ -> Nothing -- no title

goodAList = [("title", Just "BIG MOVIE"), ("user", Just "Average John"), ("review", Just "What that beeeeee")]
badAList0 = [("title", Just "BIG MOVIE"), ("user", Just "Average John")]
badAList1 = [("title", Just "BIG MOVIE"), ("user", Just "Average John"), ("review", Nothing)]
badAList2 = [("title", Just "BIG MOVIE"), ("user", Just "Average John"), ("review", Just "")]


-- This is much better than simpleReview
maybeReview :: [(String, Maybe String)] -> Maybe MovieReview
maybeReview alist = do
    title <- lookup1 "title" alist
    user <- lookup1 "user" alist
    review <- lookup1 "review" alist
    return $ MovieReview title user review

lookup1 :: String -> [(String, Maybe String)] -> Maybe String
lookup1 k alist = case lookup k alist of
                      Just (Just v@(_:_)) -> Just v
                      _ -> Nothing

-- This is much better
liftReview alist =
    liftM3 MovieReview (lookup1 "title" alist)
                       (lookup1 "user" alist)
                       (lookup1 "review" alist)

-- this is much better and general
apReview alist =
    MovieReview `liftM` lookup1 "title" alist
                `ap` lookup1 "user" alist
                `ap` lookup1 "review" alist
