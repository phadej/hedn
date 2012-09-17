module Data.EDN (
    -- * Encoding and decoding
    decode, encode,

    -- * Core EDN types
    Value(..), TaggedValue, Tagged(..),

    -- * Type conversion
    ToEDN, FromEDN, toEDN, fromEDN, fromEDNv, (.:), (.:?), (.!=),

    -- * Constructors
    tag, notag,

    -- ** Basic values
    nil,
    bool, true, false,
    char, string,
    symbol, symbolNS, keyword,
    integer, floating,

    -- ** Containers
    makeList, makeVec, makeSet, makeMap, Pair, (.=),

    -- * Tag manipulation
    setTag, getTag, stripTag,

    -- * Parsing
    parseMaybe
) where

import Data.EDN.Types
import Data.EDN.Types.Class (decode, FromEDN, ToEDN, toEDN, fromEDN, fromEDNv, (.:), (.:?), (.!=), (.=))
import Data.EDN.Encode (encode)
import Data.EDN.Parser (parseMaybe)
