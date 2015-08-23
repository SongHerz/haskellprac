import qualified Data.Map as M

type PersonName = String
type PhoneNumber = String
type BillingAddress = String

data MobileCarrier = Honest_Bobs_Phone_Network
                   | Morrisas_Marvelouse_Mobiles
                   | Petes_Plutocaratic_Phones
                   deriving (Eq, Ord)

findCarrierBillingAddress :: PersonName
    -> M.Map PersonName PhoneNumber
    -> M.Map PhoneNumber MobileCarrier
    -> M.Map MobileCarrier BillingAddress
    -> Maybe BillingAddress
findCarrierBillingAddress person phoneMap carrierMap addressMap = 
    case M.lookup person phoneMap of
        Nothing -> Nothing
        Just number ->
            case M.lookup number carrierMap of
                Nothing -> Nothing
                Just carrier -> M.lookup carrier addressMap

findCarrierBillingAddress' person phoneMap carrierMap addressMap =
    M.lookup person phoneMap >>= \number ->
    M.lookup number carrierMap >>= \carrier ->
    M.lookup carrier addressMap

findCarrierBillingAddress'' person phoneMap carrierMap addressMap = do
    number <- M.lookup person phoneMap
    carrier <- M.lookup number carrierMap
    M.lookup carrier addressMap

findCarrierBillingAddress''' person phoneMap carrierMap addressMap =
    lookup phoneMap person >>= lookup carrierMap >>= lookup addressMap
    where lookup :: Ord k => M.Map k a -> k -> Maybe a
          lookup = flip M.lookup
        
findCarrierBillingAddress'''' person phoneMap carrierMap addressMap =
    return person >>= lookup phoneMap >>= lookup carrierMap >>= lookup addressMap
    where lookup :: Ord k => M.Map k a -> k -> Maybe a
          lookup = flip M.lookup

phoneMap = M.fromList [("John", "123456")]
carrierMap = M.fromList [("123456", Honest_Bobs_Phone_Network)]
addressMap = M.fromList [(Honest_Bobs_Phone_Network, "Wall Street")]

funcs = [
      findCarrierBillingAddress
    , findCarrierBillingAddress'
    , findCarrierBillingAddress''
    , findCarrierBillingAddress'''
    , findCarrierBillingAddress''''
    ]
r0 = map (\f -> f "John" phoneMap carrierMap addressMap) funcs
r1 = map (\f -> f "Bob"  phoneMap carrierMap addressMap) funcs
