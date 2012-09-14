module Data.EDN (
    -- * Core EDN types
    Value(..), TaggedValue, Tagged(..),

    -- ** Type conversion
    ToEDN, FromEDN, toEDN, fromEDN, fromEDNv,

    -- * Tag manipulation
    setTag, getTag, stripTag,

    -- * Constructors
    tag, notag,

    -- ** Basic values
    nil,
    bool, true, false,
    char, string,
    symbol, symbolNS, keyword,
    integer, floating,

    -- ** Containers
    makeList, makeVec, makeSet, makeMap, (..=),

    -- * Encoding
    encode, fromValue, fromTagged,

    -- * Parsing
    decode, parseValue, parseTagged
) where

import Data.EDN.Types
import Data.EDN.Types.Class (FromEDN, ToEDN, toEDN, fromEDN, fromEDNv)
import Data.EDN.Encode (encode, fromValue, fromTagged)
import Data.EDN.Parser (decode, parseValue, parseTagged)
